functor PlcLexerFun(structure Tokens: PlcParser_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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
|    "_" => UNDER (lpos, rpos)
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
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\034\036\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\034\032\003\031\003\003\029\003\028\027\026\025\024\022\003\021\
\\019\019\019\019\019\019\019\019\019\019\017\016\014\012\003\003\
\\003\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\011\003\010\003\009\
\\003\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\006\005\004\003\003\
\\003"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\020\020\020\020\020\020\020\020\020\020\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\030\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\035\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 68)], trans = 0},
{fin = [(N 48),(N 68)], trans = 0},
{fin = [(N 12),(N 68)], trans = 0},
{fin = [(N 46),(N 68)], trans = 0},
{fin = [(N 10),(N 68)], trans = 7},
{fin = [(N 10)], trans = 7},
{fin = [(N 66),(N 68)], trans = 0},
{fin = [(N 56),(N 68)], trans = 0},
{fin = [(N 54),(N 68)], trans = 0},
{fin = [(N 26),(N 68)], trans = 12},
{fin = [(N 59)], trans = 0},
{fin = [(N 31),(N 68)], trans = 14},
{fin = [(N 34)], trans = 0},
{fin = [(N 39),(N 68)], trans = 0},
{fin = [(N 14),(N 68)], trans = 17},
{fin = [(N 37)], trans = 0},
{fin = [(N 7),(N 68)], trans = 19},
{fin = [(N 7)], trans = 19},
{fin = [(N 24),(N 68)], trans = 0},
{fin = [(N 18),(N 68)], trans = 22},
{fin = [(N 64)], trans = 0},
{fin = [(N 61),(N 68)], trans = 0},
{fin = [(N 20),(N 68)], trans = 0},
{fin = [(N 22),(N 68)], trans = 0},
{fin = [(N 52),(N 68)], trans = 0},
{fin = [(N 50),(N 68)], trans = 0},
{fin = [(N 68)], trans = 29},
{fin = [(N 44)], trans = 0},
{fin = [(N 41),(N 68)], trans = 0},
{fin = [(N 16),(N 68)], trans = 32},
{fin = [(N 29)], trans = 0},
{fin = [(N 4),(N 68)], trans = 34},
{fin = [(N 4)], trans = 34},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (lineNumber := !lineNumber + 1; lex())
| 10 => let val yytext=yymktext() in isKeyword(yytext, yypos, yypos) end
| 12 => (PIPE(yypos, yypos))
| 14 => (DOISPONTOS(yypos, yypos))
| 16 => (EXCL(yypos, yypos))
| 18 => (MINUS(yypos, yypos))
| 20 => (PLUS(yypos, yypos))
| 22 => (MUL(yypos, yypos))
| 24 => (DIV(yypos, yypos))
| 26 => (EQ(yypos, yypos))
| 29 => (DIF(yypos, yypos))
| 31 => (MENOR(yypos, yypos))
| 34 => (MENOREQ(yypos, yypos))
| 37 => (QUAPONTOS(yypos, yypos))
| 39 => (PONTVIRG(yypos, yypos))
| 4 => (lex())
| 41 => (SHARP(yypos, yypos))
| 44 => (AND(yypos, yypos))
| 46 => (ESQCHAVE(yypos, yypos))
| 48 => (DIRCHAVE(yypos, yypos))
| 50 => (ESQPAR(yypos, yypos))
| 52 => (DIRPAR(yypos, yypos))
| 54 => (ESQCOL(yypos, yypos))
| 56 => (DIRCOL(yypos, yypos))
| 59 => (PRODUZ(yypos, yypos))
| 61 => (VIRGULA(yypos, yypos))
| 64 => (TPRODUZ(yypos, yypos))
| 66 => (UNDER(yypos, yypos))
| 68 => let val yytext=yymktext() in error("\n***Lexer error: bad simbol***\n"); raise Fail("Lexer: fail at "^yytext)  end
| 7 => let val yytext=yymktext() in CINT(strToInt(yytext), yypos, yypos ) end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
