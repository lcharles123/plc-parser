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
    let
        val env = []
        val expT = teval exp []
        val expRes = eval exp []
    in
        val2string(expRes) ^ " : " ^ type2string(expT)
    end;


eval (fromString "15") [];(*val it = IntV 15 : plcVal*)
eval (fromString "true") [];(*val it = BoolV true : plcVal*)
eval (fromString "()") []; (*val it = ListV [] : plcVal*)
(*
eval (fromString "(6,false)[]") [];
ParseError*)
eval (fromString "(6,false)[1]") [];(*val it = IntV 6 : plcVal*)
eval (fromString "([Bool] [])") [];(*val it = SeqV [] : plcVal*)
eval (fromString "print x; true") [("x", BoolV false)]; (*val it = BoolV true : plcVal*)(*????????????????????*)
(*
eval (fromString "3::7::r") []; 
SymbolNotFound
*)
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

(* casos de excessao:
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

 *)

