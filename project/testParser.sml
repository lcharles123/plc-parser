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
fromString "match x with | 0 -> 1 | _ -> -1 end";
fromString "fun rec f(Int n):Int = if n <= 0 then 0 else n + f(n-1); f(5)";

fromFile ("example.plc");

use "testParserCases.sml";

(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)

print "Executando testes....";

teste (cases, 0);
teste (cases, 1);
teste (cases, 2);
teste (cases, 3);
teste (cases, 4);
teste (cases, 5);
teste (cases, 6);
teste (cases, 7);
teste (cases, 8);
teste (cases, 9);
teste (cases, 10);
teste (cases, 11);
teste (cases, 12);
teste (cases, 13);
teste (cases, 14);
teste (cases, 15);
teste (cases, 16);
teste (cases, 17);
teste (cases, 18);
teste (cases, 19);
teste (cases, 20);
teste (cases, 21);
teste (cases, 22);
teste (cases, 23);
teste (cases, 24);
teste (cases, 25);
teste (cases, 26);
teste (cases, 27);
teste (cases, 28);
teste (cases, 29);
teste (cases, 30);
teste (cases, 31);
teste (cases, 32);
teste (cases, 33);
teste (cases, 34);
teste (cases, 35);
teste (cases, 36);
teste (cases, 37);
teste (cases, 38);
teste (cases, 39);
teste (cases, 40);
teste (cases, 41);
teste (cases, 42);
teste (cases, 43);
teste (cases, 44);
teste (cases, 45);
teste (cases, 46);
teste (cases, 47);
teste (cases, 48);
teste (cases, 49);
teste (cases, 50);
teste (cases, 51);
teste (cases, 52);
teste (cases, 53);
teste (cases, 54);
teste (cases, 55);
teste (cases, 56);
teste (cases, 57);
teste (cases, 58);
teste (cases, 59);
teste (cases, 60);

print "Fim dos testes....";

