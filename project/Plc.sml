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
    end




