(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

(* Definições do Lexer, aqui contém os caracteres permitidos na linguagem *)

alfanumerico=[a-zA-Z];
digito=[0-9];
espaco=[\ \t];
identificadores=[a-zA-Z][a-zA-Z_0-9]*;



%%
(*casamento de padroes para tratamento dos simbolos da entrada e producao dos tokens definidos na segunda secao do .yacc*)

(*ex.
case 'token' of 't1' => T1 | 't2' => T2 *)












