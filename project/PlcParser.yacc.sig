signature PlcParser_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val CINT: (int) *  'a * 'a -> (svalue,'a) token
val NAME: (string) *  'a * 'a -> (svalue,'a) token
val WITH:  'a * 'a -> (svalue,'a) token
val MATCH:  'a * 'a -> (svalue,'a) token
val EXCL:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val ISEMPTY:  'a * 'a -> (svalue,'a) token
val HEAD:  'a * 'a -> (svalue,'a) token
val TAIL:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val ANONFUN:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val NULL:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val VIRGULA:  'a * 'a -> (svalue,'a) token
val TPRODUZ:  'a * 'a -> (svalue,'a) token
val PRODUZ:  'a * 'a -> (svalue,'a) token
val DIRCOL:  'a * 'a -> (svalue,'a) token
val ESQCOL:  'a * 'a -> (svalue,'a) token
val DIRPAR:  'a * 'a -> (svalue,'a) token
val ESQPAR:  'a * 'a -> (svalue,'a) token
val DIRCHAVE:  'a * 'a -> (svalue,'a) token
val ESQCHAVE:  'a * 'a -> (svalue,'a) token
val UNDER:  'a * 'a -> (svalue,'a) token
val SHARP:  'a * 'a -> (svalue,'a) token
val PONTVIRG:  'a * 'a -> (svalue,'a) token
val QUAPONTOS:  'a * 'a -> (svalue,'a) token
val DOISPONTOS:  'a * 'a -> (svalue,'a) token
val PIPE:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val MENOREQ:  'a * 'a -> (svalue,'a) token
val MENOR:  'a * 'a -> (svalue,'a) token
val DIF:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val RECUR:  'a * 'a -> (svalue,'a) token
val FUN:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
end
signature PlcParser_LRVALS=
sig
structure Tokens : PlcParser_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
