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


(*checar se eh keyword, tipos estao definidos nas secoes dos tokens terminais ou nao em .yacc*)
(*lista:
Bool else end false fn fun hd if Int ise
match Nil print rec then tl true var with __
*)

fun isKeyword(str, lpos, rpos) =
    case str of
    "Bool" => BOOL (lpos, rpos)
|    "else" => ELSE (lpos, rpos)
|    "end" => END (lpos, rpos)
|    "false" => FALSE (lpos, rpos)
|    "fn" => ANONFUN (lpos, rpos)
|    "fun" => FUN (lpos, rpos)
|    "hd" => HEAD (lpos, rpos)
|    "if" => IF (lpos, rpos)
|    "Int" => INT (lpos, rpos)
|    "ise" => ISEMPTY (lpos, rpos)
|    "match" => MATCH (lpos, rpos)
|    "Nil" => NULL (lpos, rpos)
|    "print" => PRINT (lpos, rpos)
|    "rec" => RECUR (lpos, rpos)
|    "then" => THEN (lpos, rpos)
|    "tl" => TAIL (lpos, rpos)
|    "true" => TRUE (lpos, rpos)
|    "var" => VAR (lpos, rpos)
|    "with" => WITH (lpos, rpos)
|    "__" => UNDER (lpos, rpos)
|    _ => NAME (str, lpos, rpos);


fun strToInt str = let 
    val 
        SOME x = Int.fromString str 
    in 
        x
    end;

(* Define what to do when the end of the file is reached. Versao sem coments *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()

(* Comentarios nao suportados a partir da proxima secao *)
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

alfanumerico=[a-zA-Z];
digito=[0-9];
espaco=[\ \t];
identificador=[a-zA-Z][a-zA-Z_0-9]*;



%%



\n => (lineNumber := !lineNumber + 1; lex());
{espaco}+ => (lex());
{digito}+ => (CINT(strToInt(yytext), yypos, yypos ));
{identificador} => (isKeyword(yytext, yypos, yypos));
"|" => (PIPE(yypos, yypos));
":" => (DOISPONTOS(yypos, yypos));
"!" => (EXCL(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"+" => (PLUS(yypos, yypos));
"*" => (MUL(yypos, yypos));
"/" => (DIV(yypos, yypos));
"=" => (EQ(yypos, yypos));
"!=" => (DIF(yypos, yypos));
"<" => (MENOR(yypos, yypos));
"<=" => (MENOREQ(yypos, yypos));
"::" => (QUAPONTOS(yypos, yypos));
";" => (PONTVIRG(yypos, yypos));
"#" => (SHARP(yypos, yypos));
"&&" => (AND(yypos, yypos));

"{" => (ESQCHAVE(yypos, yypos));
"}" => (DIRCHAVE(yypos, yypos));
"(" => (ESQPAR(yypos, yypos));
")" => (DIRPAR(yypos, yypos));
"[" => (ESQCOL(yypos, yypos));
"]" => (DIRCOL(yypos, yypos));
"=>" => (PRODUZ(yypos, yypos));
"," => (VIRGULA(yypos, yypos));

"->" => (TPRODUZ(yypos, yypos));

. => (error("\n***Lexer error: bad simbol***\n"); raise Fail("Lexer: fail at "^yytext) );






