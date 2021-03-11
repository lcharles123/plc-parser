(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fromString "15";
fromString "true";
fromString "()";
fromString "(6,false)[1]";
fromString "([Bool] [])";
fromString "print x; true";
fromString "3::7::t";
fromString "fn (Int x) => -x end";
fromString "var x = 9; x + 1";
fromString "fun f(Int x) = x; f(1)";
fromString "match x with | 0 -> 1| _ -> -1 end";
(*fromString "fun rec f(Int n) = if n <= 0 then 0 else n + f(n-1)";*)
fromString "f(5)";

(*fromFile ("example.plc");*)

use "testParserCases.sml"

(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)



(*casos de teste, chamar t cases <numero do caso>
        solucoes, chamar r cases <numero do caso>

*)
(*
val num = 3;
t cases num; r cases num;
*)
(*
val num = 4;
t cases num; r cases num;
*)
(*
val num = 5;
t cases num; r cases num;
*)
(*
val num = 6;
t cases num; r cases num;
*)
(*
val num = 7;
t cases num; r cases num;
*)
(*
val num =8;
t cases num; r cases num;
*)
(*
val num = 9;
t cases num; r cases num;
*)
(*
val num = 10;
t cases num; r cases num;
*)


