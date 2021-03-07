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
fun isKeyword(str, lpos, rpos)=
    case str of
    "Bool" => BOOL()
    "else" => ELSE
    "end" => END
    "false" => FALSE
    "fn" => ANONFUN
    "fun" => FUN
    "hd" => HEAD
    "if" => IF
    "Int" => INT
    "ise" => ISEMPTY
    "match" => MATCH
    "Nil" => NULL
    "print" => PRINT
    "rec" => RECUR
    "then" => THEN
    "tl" => TAIL
    "true" => TRUE
    "var" => VAR
    "with" => WITH
    "__" => UNDER
    _ => NAME(str, lpos, rpos)
    







(* Define what to do when the end of the file is reached. Versao sem coments *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

(* Definições do Lexer, aqui contém os caracteres permitidos na linguagem *)

alfanumerico=[a-zA-Z];
digito=[0-9];
espaco=[\ \t];
identificador=[a-zA-Z][a-zA-Z_0-9]*;



%%
(*casamento de padroes para tratamento dos simbolos da entrada e producao dos tokens definidos na segunda secao do .yacc*)

(*ex.
case 'token' of 't1' => T1 | 't2' => T2 *)
(*se vier um simbolo => faca alguma coisa; retorne uma acao*)
\n => (lineNumber := !lineNumber + 1; lex());
(*pelo menos um espaco => ignorar e continuar lendo*)
{espaco}+ => (lex());
(*se vier um numero composto de 1 ou mais digitos => transformar em token CINT(numberVar)*)
(*esses tokens sao tipos de dados definidos na secao de simbolos terminais em .yacc *)
(*pode-se usar funcoes implementadas na secao acima*)
(*yypos: posicao no texto do arquivo fonte*)
{digito}+ => (CINT(strToInt(yytext), yypos, yypos ));
(*se vier uma palavra qualquer, checar se eh keyword, retornando o construtor correto*)
{identificador} => (isKeyword(yytext, yypos, yypos));

(*aqui temos o casamento com os simbolos terminais da gramatica*)
"|" => (PIPE(yypos, yypos));
":" => (DOISPONTOS(yypos, yypos));

(*<expr>::=*)
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

(*delimitadores*)
"{" => (ESQCHAVE(yypos, yypos));
"}" => (DIRCHAVE(yypos, yypos));
"(" => (ESQPAR(yypos, yypos));
")" => (DIRPAR(yypos, yypos));
"[" => (ESQCOL(yypos, yypos));
"]" => (DIRCOL(yypos, yypos));
"=>" => (PRODUZ(yypos, yypos));
"," => (VIRGULA(yypos, yypos));
(*type*)
"->" => (TPRODUZ(yypos, yypos));

. => (error("\n***Lexer error: bad simbol***\n"); raise Fail("Lexer: fail at "^yytext) );






