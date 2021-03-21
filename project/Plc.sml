(* Plc interpreter main file *)

(* carregar os modulos *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

(* modulos implementados pelos alunos *)
use "PlcChecker.sml";
use "PlcInterp.sml";


Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

print "Iniciando interpretador...";

(*run : expr -> string
 run faz o sequitne:
1: obter a sintaxe abstrata usando fromString ; fromFile
2: checar tipos com teval
3: avaliar com eval retornando uma string com valor e tipo do resultado, tratar excessoes aqui caso 
*)


fun run exp =
    (let
        val env = []
        val resT = teval exp []
        val resV = eval exp []
        
    in
        val2string(resV) ^ " : " ^ type2string(resT) ^ " \n"
    end) (*handle SymbolNotFound => let val p = print ("ERROR: One or more symbols are not defined in expression evaluation.") in raise SymbolNotFound end*)
    
    handle EmptySeq => "ERRO no Checker: EmptySeq: A sequência de entrada não contém nenhum elemento " 
    |   UnknownType => "ERRO no Checker: UnknownType: Excecao de UnknownType "
    |   NotEqTypes => "ERRO no Checker: NotEqTypes: Comparacao de tipos diferentes "
    |   WrongRetType => "ERRO no Checker: WrongRetType : Tipo de retorno nao concorda "
    |   DiffBrTypes => "ERRO no Checker: DiffBrTypes: Os tipos da expressões dos possíveis caminhos de um If divergem "
    |   IfCondNotBool => "ERRO no Checker: IfCondNotBool: A condição do if não é booleana "
    |   MatchResTypeDiff => "ERRO no Checker: MatchResTypeDiff: O tipo de algum dos casos em match difere dos demais "
    |   MatchCondTypesDiff => "ERRO no Checker: MatchCondTypesDiff: O tipo das opções de match difere do tipo da expressão passada para Match "
    |   CallTypeMisM => "ERRO no Checker: CallTypeMisM: Você está passando pra uma chamada de função um tipo diferente do qual ela suporta "
    |   NotFunc => "ERRO no Checker: NotFunc: Você está tentando chamar algo que não é uma função "
    |   ListOutOfRange => "ERRO no Checker: ListOutOfRange: Tentativa de acessar um elemento fora dos limites da lista "
    |   OpNonList => "ERRO no Checker: OpNonList: Tentativa de acessar um elemento em uma expressão que não é uma lista "
    
    |   SymbolNotFound => "ERRO no Interp: SymbolNotFound: Caractere nao definido como variavel ou construtor " 
    |   Impossible => "ERRO no Interp: Impossible: Nao eh possivel fazer a operacao "
    |   HDEmptySeq => "ERRO no Interp: HDEmptySeq: Nao eh possivel acessar a cabeca da Sequencia vazia "
    |   TLEmptySeq => "ERRO no Interp: TLEmptySeq: Nao eh possivel acessar a cauda da sequencia vazia "
    |   NoMatchResults => "ERRO no Interp: NoMatchResults: Não deu match "
    |   NotAFunc => "ERRO no Interp: NotAFunc: Variavel usada nao eh uma funcao "
    ;

(*
eval (fromString "15") [];(*val it = IntV 15 : plcVal*)
eval (fromString "true") [];(*val it = BoolV true : plcVal*)
eval (fromString "()") []; (*val it = ListV [] : plcVal*)
eval (fromString "(6,false)[]") [];
eval (fromString "(6,false)[1]") [];(*val it = IntV 6 : plcVal*)
eval (fromString "([Bool] [])") [];(*val it = SeqV [] : plcVal*)
eval (fromString "print x; true") [("x", BoolV false)]; (*val it = BoolV true : plcVal*)
eval (fromString "3::7::r") []; 
eval (fromString "fn (Int x) => -x end");(*val it = fn : plcVal env -> plcVal*)
eval (fromString "var x = 9; x + 1") [];(*val it = IntV 10 : plcVal*)
eval (fromString "fun f(Int x) = x; f(1)") [];(*val it = IntV 1 : plcVal*)
eval (fromString "match x with | 0 -> 1 | _ -> -1 end") [("x", IntV 0)];(*val it = IntV 1 : plcVal*)
eval (fromString "if true then 1 else 0") [];(*val it = IntV 1 : plcVal*)
eval (fromString "true && false") [];(*val it = BoolV false : plcVal*)
eval (fromString "1+2") [];(*val it = IntV 3 : plcVal*)
eval (fromString "1-2") [];(*val it = IntV ~1 : plcVal*)
eval (fromString "1*2") [];(*val it = IntV 2 : plcVal*)
eval (fromString "1/2") [];(*val it = IntV 0 : plcVal*)
eval (fromString "fun rec f(Int n):Int = if n <= 0 then 0 else n + f(n-1); f(5)") []; (*val it = IntV 15 : plcVal*)

(* casos de Excecao:
Impossible
eval (fromString "!1") [];
NotAFunc
eval (fromString "var x = 0; x(1)") [];
TLEmptySeq
eval (fromString "tl ([Int] [])") [];
HDEmptySeq
eval (fromString "hd ([Int] [])") [];
ValueNotFoundInMatch
eval (fromString "match x with | 1 -> 0 | 2 -> 1 end") [("x", IntV 0)];
teval (fromString "match x with | 0 -> 1 | _ -> -1 end") [("x", IntV 0)];(*val it = IntV 1 : plcVal*)
*)
*)


(*
usar ((fromString "15") ou (fromFile "arquivo.plc"))
*)
fun entrada str = print(run str);


entrada (fromString "15") ;(*val it = IntV 15 : plcVal*)
entrada (fromString "true") ;(*val it = BoolV true : plcVal*)
entrada (fromString "()") ; (*val it = ListV [] : plcVal*)
entrada (fromString "(6,false)");
entrada (fromString "(6,false)[1]") ;(*val it = IntV 6 : plcVal*)
entrada (fromString "([Bool] [])") ;(*val it = SeqV [] : plcVal*)
entrada (fromString "print 1; true"); (*val it = BoolV true : plcVal*)
entrada (fromString "3::7::r") ; (*SymbolNotFound*)
entrada (fromString "fn (Int x) => -x end");(*val it = fn : plcVal env -> plcVal*)
entrada (fromString "var x = 9; x + 1") ;(*val it = IntV 10 : plcVal*)
entrada (fromString "fun f(Int x) = x; f(1)") ;(*val it = IntV 1 : plcVal*)
entrada (fromString "match x with | 0 -> 1 | _ -> -1 end") ;(*val it = IntV 1 : plcVal*)
entrada (fromString "if true then 1 else 0") ;(*val it = IntV 1 : plcVal*)
entrada (fromString "true && false") ;(*val it = BoolV false : plcVal*)
entrada (fromString "1+2") ;(*val it = IntV 3 : plcVal*)
entrada (fromString "1-2") ;(*val it = IntV ~1 : plcVal*)
entrada (fromString "1*2") ;(*val it = IntV 2 : plcVal*)
entrada (fromString "1/2") ;(*val it = IntV 0 : plcVal*)
entrada (fromString "fun rec f(Int n):Int = if n <= 0 then 0 else n + f(n-1); f(5)") ; (*val it = IntV 15 : plcVal*)

(* casos de Excecao:*)
(*Impossible*)
entrada (fromString "!1") ;
(*NotAFunc*)
entrada (fromString "var x = 0; x(1)") ;
(*TLEmptySeq*)
entrada (fromString "tl ([Int] [])") ;
(*HDEmptySeq*)
entrada (fromString "hd ([Int] [])") ;
(*ValueNotFoundInMatch*)
entrada (fromString "match x with | 1 -> 0 | 2 -> 1 end");






