(* PlcInterp *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

use "PlcChecker.sml";(* Checador de tipos *) 

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;




exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc


(* obter o valor de uma expressao bem tipada    *)
(* eval : expr -> plcValue env -> plcValue *)
(* entrada: expr da secao 4.3 da especificacao, ie: (ConI 15) -> (IntV 15)*)
(* de acordo com secao 3.3 devemos transformar expr em PlcVal  *)
(* expr -> plcValue  pag 10 projeto.pdf *)
(*  
ConI 15
ConB true
List []
List [ConI 6; ConB false]
Item (1, List [ConI 6; ConB false])
ESeq (SeqT BoolT)
Prim2 (";", Prim1 ("print", ConI 27), ConB true)
Prim1 ("print", ConI 27)
Prim2 ("::", ConI 3, Prim2 ("::", ConI 4, Prim2 ("::", ConI 5, ESeq (SeqT IntT))))
Anon (IntT, "x", Prim1(-", Var "x"))
Let ("x", ConI 9, Prim2 ("+", Var "x", ConI 1))
Let ("f", Anon (Int, "x", Var "x"), Call ("f", ConI 1))

*)

fun eval (ConI n) (env:plcVal env)  = IntV n  (* constantes inteiras -> plcVal intV n *) 
|   eval (ConB n) (env:plcVal env)  = BoolV n (* booleanas *)
|   eval (ESeq s) (env:plcVal env)  = SeqV []  (* sequencia vazia *)
|   eval (Var v)  (env:plcVal env)  = (lookup env v) (* variaveis, sao uma lista de tuplas (NAME, valor) *)
|   eval (Let (var, exp1, exp2)) (env:plcVal env) =  (* igual let de sml, ie. let dec in exp end *) 
        let
            val lista = (var, eval exp1 env) :: env (* desenrolar o operador let*)
        in
            eval exp2 lista
        end 
|   eval (Letrec(name, argT, args, funT, exp1, exp2)) (env:plcVal env) = (* funcoes recursivas *)
        let
            val recur = (name, Clos(name, args, exp1, env)) :: env (* retorna um Clos : plcValue *)
        in
            eval exp2 recur
        end
|   eval (Prim1(oper, exp)) (env:plcVal env) =   (* operadores unarios *)
        let
            val e = eval exp env
        in
            case e of
                IntV i => 
                    if oper = "-" 
                    then IntV (~i) 
                    else if oper = "print" 
                    then let
                         val aux = print(val2string(IntV i) ^ "\n")
                         in ListV []
                         end
                    else raise Impossible
            |   BoolV b => 
                    if oper = "!"
                    then BoolV (not b)
                    else if oper = "print"
                    then let val aux = print(val2string(BoolV b) ^ "\n")
                         in ListV []
                         end
                    else raise Impossible
            |   ListV l =>
                    if oper = "print" 
                    then let val aux = print(list2string(val2string, l) ^ "\n")
                         in ListV []
                         end
                    else raise Impossible
            |   SeqV s => (  
                    case oper of
                        "hd" => ((hd s) handle Empty => raise HDEmptySeq )
                    |   "tl" => ((SeqV (tl s)) handle Empty => raise TLEmptySeq )
                    |   "ise" =>
                            if s = [] 
                            then BoolV true
                            else BoolV false                                   
                    |   "print" => 
                            let val aux = print(list2string(val2string, s) ^ "\n")
                            in ListV []
                            end 
                    |   _ => raise Impossible )
            |   _ =>  ListV []
        end
|   eval (Prim2(oper, exp1, exp2)) (env:plcVal env) =  (* operadores binarios *)
        if oper = ";" 
        then let val aux = eval exp1 env
             in eval exp2 env
             end
        else let val v1 = eval exp1 env
                 val v2 = eval exp2 env
             in
                case (v1, v2) of
                    (IntV i1, IntV i2) => (
                        case oper of
                            "+"  => IntV (i1 + i2)
                        |   "-"  => IntV (i1 - i2)
                        |   "*"  => IntV (i1 * i2)
                        |   "/"  => IntV (i1 div i2)
                        |   "!=" => BoolV (i1 <> i2)
                        |   "<=" => BoolV (i1 <= i2)
                        |   "<"  => BoolV (i1 < i2)
                        |   "="  => BoolV (i1 = i2)
                        |   _    => raise Impossible )
                |   (BoolV b1, BoolV b2) => (
                        case oper of
                            "=" => BoolV (b1 = b2)
                        |   "!=" => BoolV (b1 <> b2)
                        |   "&&" => BoolV (b1 andalso b2)
                        |   _   => raise Impossible )
                |   (IntV i1, SeqV s2) => 
                        if oper = "::" 
                        then SeqV (IntV i1 :: s2)
                        else raise Impossible 
                |   (BoolV b1, SeqV s2) => 
                        if oper = "::"
                        then SeqV (BoolV b1 :: s2)
                        else raise Impossible 
                |   (ListV l1, SeqV s2) => 
                        if oper = "::"
                        then SeqV (ListV l1 :: s2)
                        else raise Impossible 
                |   _ => raise Impossible
             end

|   eval (If(exp1, exp2, exp3)) (env:plcVal env) =  (*if exp1 then exp2 else exp3*)
        let val res = eval exp1 env
        in
            if res = (BoolV true)
            then eval exp2 env
            else if res = (BoolV false)
            then eval exp3 env
            else raise Impossible
        end
|   eval (Match(exp1, listaOpcoes)) (env:plcVal env) =  (*case of ; match*)
        let fun procurar (var, l::[]) env =  (* procura var na lista l*)
                (case l of
                    (SOME exp2, exp3) => 
                        if var = eval exp2 env 
                        then exp3 
                        else raise ValueNotFoundInMatch (*lista com unico elemento e nao deu match*)
                 |  (NONE, exp3) => exp3 )
                              
            |   procurar (var, h::t) env = 
                (case h of
                    (SOME exp2, exp3) => 
                        if var = eval exp2 env 
                        then exp3 
                        else procurar (var, t) env (*lista com mais de um elemento, procurar no resto*)
                |   (NONE, exp3) => raise Impossible )
                              
            |   procurar (var, [] ) env = raise Impossible
        in eval (procurar ((eval exp1 env), listaOpcoes) env) env
        end
|   eval (Call(exp1, exp2)) (env:plcVal env) =  (*aplicar fun*)
        let fun evalArgs (List(h::[])) = [eval h env]
            |   evalArgs (List(h::t)) = [eval h env] @ evalArgs (List t)
            |   evalArgs (exp) = [eval exp env]
        val nomeFunc = eval exp1 env
        val envs = [("$list", ListV(evalArgs exp2))] @ env
        in  
            case nomeFunc of
                Clos(name, var, exp, env) => 
                    let val env2 = (var, (eval exp2 envs) )::(name, nomeFunc)::env
                    in eval exp env2
                    end
            |   _ => raise NotAFunc
        end
|   eval (List []) (env:plcVal env) = ListV []  (*lista e Nil *)
|   eval (List l) (env:plcVal env) =
        let fun elem (h::[]) = eval h env :: [] (* feita para tratar qualquer valor na lista ie. outra lista *)
            |   elem (h::t) = eval h env :: elem t 
            |   elem [] = raise Impossible (* remover warning non-exaustive *)
        in ListV(elem l) (* construtor da lista *)
        end

|   eval (Item (i, exp)) (env:plcVal env) = (*List selector: seleciona um elemento da lista exp*)
    let fun elem (i, []) = raise Impossible
        |   elem (i, (h::[])) = if i = 1 then h else raise Impossible (*checar se o indice i eh invalido *)
        |   elem (i, (h::t)) = if i = 1 then h else elem (i-1, t)
        val valor = eval exp env
        fun checarSe (ListV l) = elem (i, l)
        |   checarSe (SeqV s) = elem (i, s)
        |   checarSe _ = raise Impossible
        in checarSe valor
        end
|   eval (Anon(typ, arg, exp)) (env:plcVal env) = Clos("", arg, exp, env)  ; (* pag 10 da especificacao*)
 


