functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\024\000\002\000\023\000\004\000\022\000\019\000\021\000\
\\021\000\020\000\028\000\019\000\034\000\018\000\035\000\017\000\
\\036\000\016\000\038\000\015\000\039\000\014\000\040\000\013\000\
\\041\000\012\000\042\000\011\000\043\000\010\000\045\000\009\000\
\\046\000\008\000\000\000\
\\001\000\003\000\061\000\045\000\060\000\000\000\
\\001\000\004\000\022\000\018\000\118\000\019\000\021\000\021\000\020\000\
\\028\000\019\000\034\000\018\000\035\000\017\000\036\000\016\000\
\\038\000\015\000\039\000\014\000\040\000\013\000\041\000\012\000\
\\042\000\011\000\043\000\010\000\045\000\009\000\046\000\008\000\000\000\
\\001\000\004\000\022\000\019\000\021\000\021\000\020\000\028\000\019\000\
\\034\000\018\000\035\000\017\000\036\000\016\000\038\000\015\000\
\\039\000\014\000\040\000\013\000\041\000\012\000\042\000\011\000\
\\043\000\010\000\045\000\009\000\046\000\008\000\000\000\
\\001\000\004\000\022\000\019\000\021\000\021\000\057\000\022\000\056\000\
\\023\000\055\000\028\000\019\000\031\000\054\000\032\000\053\000\
\\033\000\052\000\034\000\018\000\035\000\017\000\036\000\016\000\
\\038\000\015\000\039\000\014\000\040\000\013\000\041\000\012\000\
\\042\000\011\000\043\000\010\000\045\000\009\000\046\000\008\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\013\000\098\000\015\000\029\000\016\000\028\000\
\\023\000\027\000\037\000\097\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\020\000\137\000\
\\023\000\027\000\047\000\137\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\022\000\087\000\
\\023\000\027\000\027\000\086\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\
\\029\000\082\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\
\\030\000\121\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\
\\037\000\119\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\
\\044\000\075\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\127\000\023\000\027\000\000\000\
\\001\000\008\000\094\000\000\000\
\\001\000\008\000\113\000\000\000\
\\001\000\008\000\130\000\000\000\
\\001\000\013\000\098\000\037\000\097\000\000\000\
\\001\000\014\000\126\000\000\000\
\\001\000\020\000\091\000\000\000\
\\001\000\021\000\046\000\000\000\
\\001\000\021\000\081\000\022\000\080\000\023\000\055\000\031\000\054\000\
\\032\000\053\000\033\000\052\000\000\000\
\\001\000\021\000\081\000\023\000\055\000\031\000\054\000\032\000\053\000\
\\033\000\052\000\000\000\
\\001\000\022\000\085\000\000\000\
\\001\000\022\000\102\000\000\000\
\\001\000\022\000\110\000\000\000\
\\001\000\022\000\112\000\023\000\084\000\026\000\083\000\027\000\111\000\000\000\
\\001\000\022\000\112\000\026\000\083\000\027\000\111\000\000\000\
\\001\000\022\000\122\000\000\000\
\\001\000\023\000\084\000\026\000\083\000\000\000\
\\001\000\024\000\095\000\000\000\
\\001\000\024\000\106\000\000\000\
\\001\000\024\000\109\000\026\000\083\000\000\000\
\\001\000\025\000\076\000\000\000\
\\001\000\026\000\083\000\045\000\100\000\000\000\
\\001\000\026\000\128\000\000\000\
\\001\000\045\000\062\000\000\000\
\\001\000\045\000\093\000\000\000\
\\001\000\046\000\063\000\000\000\
\\001\000\047\000\000\000\000\000\
\\137\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\000\000\
\\141\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\000\000\
\\142\000\019\000\021\000\021\000\020\000\034\000\018\000\035\000\017\000\
\\036\000\016\000\045\000\009\000\046\000\008\000\000\000\
\\143\000\019\000\021\000\021\000\020\000\034\000\018\000\035\000\017\000\
\\036\000\016\000\045\000\009\000\046\000\008\000\000\000\
\\144\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\023\000\027\000\000\000\
\\145\000\000\000\
\\146\000\023\000\027\000\000\000\
\\147\000\006\000\036\000\007\000\035\000\023\000\027\000\000\000\
\\148\000\023\000\027\000\000\000\
\\149\000\023\000\027\000\000\000\
\\150\000\023\000\027\000\000\000\
\\151\000\023\000\027\000\000\000\
\\152\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\015\000\029\000\023\000\027\000\000\000\
\\153\000\006\000\036\000\007\000\035\000\023\000\027\000\000\000\
\\154\000\006\000\036\000\007\000\035\000\023\000\027\000\000\000\
\\155\000\023\000\027\000\000\000\
\\156\000\023\000\027\000\000\000\
\\157\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\010\000\032\000\011\000\031\000\015\000\029\000\023\000\027\000\000\000\
\\158\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\010\000\032\000\011\000\031\000\015\000\029\000\023\000\027\000\000\000\
\\159\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\015\000\029\000\023\000\027\000\000\000\
\\160\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\015\000\029\000\023\000\027\000\000\000\
\\161\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\015\000\029\000\023\000\027\000\000\000\
\\162\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\
\\027\000\086\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\004\000\038\000\005\000\037\000\006\000\036\000\007\000\035\000\
\\008\000\034\000\009\000\033\000\010\000\032\000\011\000\031\000\
\\012\000\030\000\015\000\029\000\016\000\028\000\023\000\027\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\027\000\101\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\026\000\083\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\026\000\083\000\027\000\111\000\000\000\
\\197\000\000\000\
\"
val actionRowNumbers =
"\000\000\066\000\045\000\044\000\
\\039\000\040\000\076\000\067\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\019\000\075\000\
\\074\000\003\000\004\000\000\000\
\\003\000\001\000\035\000\073\000\
\\072\000\037\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\011\000\048\000\053\000\
\\052\000\050\000\051\000\032\000\
\\020\000\008\000\090\000\028\000\
\\022\000\007\000\095\000\094\000\
\\096\000\021\000\077\000\004\000\
\\018\000\049\000\019\000\036\000\
\\013\000\029\000\064\000\063\000\
\\054\000\062\000\061\000\060\000\
\\059\000\058\000\057\000\055\000\
\\056\000\016\000\003\000\033\000\
\\087\000\023\000\085\000\021\000\
\\003\000\021\000\030\000\070\000\
\\003\000\069\000\031\000\024\000\
\\025\000\068\000\014\000\019\000\
\\003\000\065\000\047\000\081\000\
\\002\000\010\000\089\000\021\000\
\\086\000\026\000\009\000\093\000\
\\027\000\080\000\079\000\092\000\
\\091\000\021\000\097\000\003\000\
\\017\000\012\000\034\000\083\000\
\\084\000\071\000\088\000\003\000\
\\078\000\099\000\098\000\042\000\
\\015\000\000\000\003\000\046\000\
\\003\000\006\000\041\000\005\000\
\\043\000\082\000\038\000"
val gotoT =
"\
\\001\000\134\000\002\000\005\000\003\000\004\000\004\000\003\000\
\\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\004\000\023\000\006\000\001\000\000\000\
\\004\000\024\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\037\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\038\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\039\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\040\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\041\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\042\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\010\000\043\000\000\000\
\\000\000\
\\000\000\
\\003\000\045\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\049\000\004\000\003\000\005\000\002\000\006\000\001\000\
\\007\000\048\000\013\000\047\000\014\000\046\000\000\000\
\\001\000\056\000\002\000\005\000\003\000\004\000\004\000\003\000\
\\005\000\002\000\006\000\001\000\000\000\
\\003\000\057\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\062\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\063\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\064\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\065\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\066\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\067\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\068\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\069\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\070\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\071\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\072\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\077\000\012\000\076\000\013\000\075\000\014\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\086\000\014\000\046\000\000\000\
\\000\000\
\\003\000\049\000\004\000\003\000\005\000\002\000\006\000\001\000\
\\007\000\048\000\013\000\088\000\014\000\046\000\015\000\087\000\000\000\
\\000\000\
\\000\000\
\\010\000\090\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\094\000\000\000\
\\003\000\097\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\101\000\014\000\046\000\015\000\087\000\000\000\
\\003\000\102\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\013\000\103\000\014\000\046\000\000\000\
\\000\000\
\\000\000\
\\003\000\106\000\004\000\003\000\005\000\002\000\006\000\001\000\
\\007\000\105\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\112\000\000\000\
\\003\000\113\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\115\000\004\000\003\000\005\000\002\000\006\000\001\000\
\\009\000\114\000\000\000\
\\000\000\
\\000\000\
\\011\000\118\000\012\000\076\000\013\000\075\000\014\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\122\000\014\000\046\000\015\000\121\000\000\000\
\\000\000\
\\003\000\123\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\127\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\130\000\002\000\005\000\003\000\129\000\004\000\003\000\
\\005\000\002\000\006\000\001\000\000\000\
\\003\000\131\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\003\000\132\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\008\000\133\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 135
val numrules = 61
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | CINT of unit ->  (int) | NAME of unit ->  (string)
 | Types of unit ->  (plcType list) | AtomicType of unit ->  (plcType)
 | Type of unit ->  (plcType) | TypedVar of unit ->  (plcType*string)
 | Params of unit ->  ( ( plcType * string )  list)
 | Args of unit ->  ( ( plcType * string )  list)
 | CondExpr of unit ->  (expr option)
 | MatchExpr of unit ->  ( ( expr option * expr )  list)
 | Comps of unit ->  (expr list) | Const of unit ->  (expr)
 | AppExpr of unit ->  (expr) | AtomicExpr of unit ->  (expr)
 | Expr of unit ->  (expr) | Dec1 of unit ->  (expr)
 | Prog of unit ->  (expr)
end
type svalue = MlyValue.svalue
type result = expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 46) => true | _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "FUN"
  | (T 2) => "RECUR"
  | (T 3) => "MINUS"
  | (T 4) => "PLUS"
  | (T 5) => "MUL"
  | (T 6) => "DIV"
  | (T 7) => "EQ"
  | (T 8) => "DIF"
  | (T 9) => "MENOR"
  | (T 10) => "MENOREQ"
  | (T 11) => "AND"
  | (T 12) => "PIPE"
  | (T 13) => "DOISPONTOS"
  | (T 14) => "QUAPONTOS"
  | (T 15) => "PONTVIRG"
  | (T 16) => "SHARP"
  | (T 17) => "UNDER"
  | (T 18) => "ESQCHAVE"
  | (T 19) => "DIRCHAVE"
  | (T 20) => "ESQPAR"
  | (T 21) => "DIRPAR"
  | (T 22) => "ESQCOL"
  | (T 23) => "DIRCOL"
  | (T 24) => "PRODUZ"
  | (T 25) => "TPRODUZ"
  | (T 26) => "VIRGULA"
  | (T 27) => "IF"
  | (T 28) => "THEN"
  | (T 29) => "ELSE"
  | (T 30) => "INT"
  | (T 31) => "NULL"
  | (T 32) => "BOOL"
  | (T 33) => "TRUE"
  | (T 34) => "FALSE"
  | (T 35) => "ANONFUN"
  | (T 36) => "END"
  | (T 37) => "TAIL"
  | (T 38) => "HEAD"
  | (T 39) => "ISEMPTY"
  | (T 40) => "PRINT"
  | (T 41) => "EXCL"
  | (T 42) => "MATCH"
  | (T 43) => "WITH"
  | (T 44) => "NAME"
  | (T 45) => "CINT"
  | (T 46) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 46) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 
2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Expr
 as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 0, ( result, Expr1left, Expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Dec1 Dec11, Dec11left, Dec11right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Dec1
 as Dec11) = Dec11 ()
 in (Dec1)
end)
 in ( LrTable.NT 0, ( result, Dec11left, Dec11right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.NAME NAME1, _, _))
 :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Dec1 (fn _ => let val  (NAME as NAME1) = NAME1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (Let(NAME, Expr, Prog))
end)
 in ( LrTable.NT 1, ( result, VAR1left, Prog1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: _ :: ( _, ( 
MlyValue.Args Args1, _, _)) :: ( _, ( MlyValue.NAME NAME1, _, _)) :: (
 _, ( _, FUN1left, _)) :: rest671)) => let val  result = MlyValue.Dec1
 (fn _ => let val  NAME1 = NAME1 ()
 val  Args1 = Args1 ()
 val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 1, ( result, FUN1left, Expr1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: _ :: _ :: ( _
, ( MlyValue.Args Args1, _, _)) :: ( _, ( MlyValue.NAME NAME1, _, _))
 :: _ :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = 
MlyValue.Dec1 (fn _ => let val  NAME1 = NAME1 ()
 val  Args1 = Args1 ()
 val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 1, ( result, FUN1left, Expr1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.AtomicExpr AtomicExpr1, AtomicExpr1left, 
AtomicExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn
 _ => let val  (AtomicExpr as AtomicExpr1) = AtomicExpr1 ()
 in (AtomicExpr)
end)
 in ( LrTable.NT 2, ( result, AtomicExpr1left, AtomicExpr1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.AppExpr AppExpr1, AppExpr1left, 
AppExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AppExpr as AppExpr1) = AppExpr1 ()
 in (AppExpr)
end)
 in ( LrTable.NT 2, ( result, AppExpr1left, AppExpr1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.Expr Expr3, _, Expr3right)) :: _ :: ( _, ( 
MlyValue.Expr Expr2, _, _)) :: _ :: ( _, ( MlyValue.Expr Expr1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 val  Expr3 = Expr3 ()
 in (If(Expr1, Expr2, Expr3))
end)
 in ( LrTable.NT 2, ( result, IF1left, Expr3right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: _ :: ( _, ( MlyValue.Expr Expr1, _, _)) :: ( _, ( _, MATCH1left, _
)) :: rest671)) => let val  result = MlyValue.Expr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in (Match(Expr, MatchExpr))
end)
 in ( LrTable.NT 2, ( result, MATCH1left, MatchExpr1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
EXCL1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("!", Expr))
end)
 in ( LrTable.NT 2, ( result, EXCL1left, Expr1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("-", Expr))
end)
 in ( LrTable.NT 2, ( result, MINUS1left, Expr1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
HEAD1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("hd", Expr))
end)
 in ( LrTable.NT 2, ( result, HEAD1left, Expr1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
TAIL1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("tl", Expr))
end)
 in ( LrTable.NT 2, ( result, TAIL1left, Expr1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
ISEMPTY1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn
 _ => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("ise", Expr))
end)
 in ( LrTable.NT 2, ( result, ISEMPTY1left, Expr1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (Expr as Expr1) = Expr1 ()
 in (Prim1("print", Expr))
end)
 in ( LrTable.NT 2, ( result, PRINT1left, Expr1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("&&", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("+", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("-", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("*", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("/", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("=", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("!=", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<=", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("::", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Expr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2(";", Expr1, Expr2))
end)
 in ( LrTable.NT 2, ( result, Expr1left, Expr2right), rest671)
end
|  ( 26, ( ( _, ( _, _, DIRCOL1right)) :: ( _, ( MlyValue.CINT CINT1,
 _, _)) :: _ :: ( _, ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671)
) => let val  result = MlyValue.Expr (fn _ => let val  (Expr as Expr1)
 = Expr1 ()
 val  (CINT as CINT1) = CINT1 ()
 in (Item(CINT, Expr))
end)
 in ( LrTable.NT 2, ( result, Expr1left, DIRCOL1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.Const Const1, Const1left, Const1right)) :: 
rest671)) => let val  result = MlyValue.AtomicExpr (fn _ => let val  (
Const as Const1) = Const1 ()
 in (Const)
end)
 in ( LrTable.NT 3, ( result, Const1left, Const1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.NAME NAME1, NAME1left, NAME1right)) :: 
rest671)) => let val  result = MlyValue.AtomicExpr (fn _ => let val  (
NAME as NAME1) = NAME1 ()
 in (Var NAME)
end)
 in ( LrTable.NT 3, ( result, NAME1left, NAME1right), rest671)
end
|  ( 29, ( ( _, ( _, _, DIRCHAVE1right)) :: ( _, ( MlyValue.Prog Prog1
, _, _)) :: ( _, ( _, ESQCHAVE1left, _)) :: rest671)) => let val  
result = MlyValue.AtomicExpr (fn _ => let val  (Prog as Prog1) = Prog1
 ()
 in (Prog)
end)
 in ( LrTable.NT 3, ( result, ESQCHAVE1left, DIRCHAVE1right), rest671)

end
|  ( 30, ( ( _, ( _, _, DIRPAR1right)) :: ( _, ( MlyValue.Expr Expr1,
 _, _)) :: ( _, ( _, ESQPAR1left, _)) :: rest671)) => let val  result
 = MlyValue.AtomicExpr (fn _ => let val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 3, ( result, ESQPAR1left, DIRPAR1right), rest671)
end
|  ( 31, ( ( _, ( _, _, DIRPAR1right)) :: ( _, ( MlyValue.Comps Comps1
, _, _)) :: ( _, ( _, ESQPAR1left, _)) :: rest671)) => let val  result
 = MlyValue.AtomicExpr (fn _ => let val  (Comps as Comps1) = Comps1 ()
 in (List Comps)
end)
 in ( LrTable.NT 3, ( result, ESQPAR1left, DIRPAR1right), rest671)
end
|  ( 32, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.Expr Expr1, _,
 _)) :: _ :: ( _, ( MlyValue.Args Args1, _, _)) :: ( _, ( _, 
ANONFUN1left, _)) :: rest671)) => let val  result = 
MlyValue.AtomicExpr (fn _ => let val  (Args as Args1) = Args1 ()
 val  (Expr as Expr1) = Expr1 ()
 in (makeAnon(Args, Expr))
end)
 in ( LrTable.NT 3, ( result, ANONFUN1left, END1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.AtomicExpr AtomicExpr2, _, AtomicExpr2right
)) :: ( _, ( MlyValue.AtomicExpr AtomicExpr1, AtomicExpr1left, _)) :: 
rest671)) => let val  result = MlyValue.AppExpr (fn _ => let val  
AtomicExpr1 = AtomicExpr1 ()
 val  AtomicExpr2 = AtomicExpr2 ()
 in (Call(AtomicExpr1, AtomicExpr2))
end)
 in ( LrTable.NT 4, ( result, AtomicExpr1left, AtomicExpr2right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.AtomicExpr AtomicExpr1, _, AtomicExpr1right
)) :: ( _, ( MlyValue.AppExpr AppExpr1, AppExpr1left, _)) :: rest671))
 => let val  result = MlyValue.AppExpr (fn _ => let val  (AppExpr as 
AppExpr1) = AppExpr1 ()
 val  (AtomicExpr as AtomicExpr1) = AtomicExpr1 ()
 in ((Call(AppExpr, AtomicExpr)))
end)
 in ( LrTable.NT 4, ( result, AppExpr1left, AtomicExpr1right), rest671
)
end
|  ( 35, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Const (fn _ => (ConB true))
 in ( LrTable.NT 5, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 36, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Const (fn _ => (ConB false))
 in ( LrTable.NT 5, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.CINT CINT1, CINT1left, CINT1right)) :: 
rest671)) => let val  result = MlyValue.Const (fn _ => let val  (CINT
 as CINT1) = CINT1 ()
 in (ConI CINT)
end)
 in ( LrTable.NT 5, ( result, CINT1left, CINT1right), rest671)
end
|  ( 38, ( ( _, ( _, _, DIRPAR1right)) :: ( _, ( _, ESQPAR1left, _))
 :: rest671)) => let val  result = MlyValue.Const (fn _ => (List []))
 in ( LrTable.NT 5, ( result, ESQPAR1left, DIRPAR1right), rest671)
end
|  ( 39, ( ( _, ( _, _, DIRPAR1right)) :: _ :: _ :: ( _, ( 
MlyValue.Type Type1, _, _)) :: ( _, ( _, ESQPAR1left, _)) :: rest671))
 => let val  result = MlyValue.Const (fn _ => let val  (Type as Type1)
 = Type1 ()
 in (ESeq Type)
end)
 in ( LrTable.NT 5, ( result, ESQPAR1left, DIRPAR1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Comps (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in ([Expr1, Expr2])
end)
 in ( LrTable.NT 6, ( result, Expr1left, Expr2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.Comps Comps1, _, Comps1right)) :: _ :: ( _,
 ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result
 = MlyValue.Comps (fn _ => let val  (Expr as Expr1) = Expr1 ()
 val  (Comps as Comps1) = Comps1 ()
 in ([Expr]@Comps)
end)
 in ( LrTable.NT 6, ( result, Expr1left, Comps1right), rest671)
end
|  ( 42, ( ( _, ( _, END1left, END1right)) :: rest671)) => let val  
result = MlyValue.MatchExpr (fn _ => ([]))
 in ( LrTable.NT 7, ( result, END1left, END1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: ( _, ( MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( 
MlyValue.CondExpr CondExpr1, _, _)) :: ( _, ( _, PIPE1left, _)) :: 
rest671)) => let val  result = MlyValue.MatchExpr (fn _ => let val  (
CondExpr as CondExpr1) = CondExpr1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in ([(CondExpr, Expr)] @ MatchExpr)
end)
 in ( LrTable.NT 7, ( result, PIPE1left, MatchExpr1right), rest671)

end
|  ( 44, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.CondExpr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 in (SOME Expr)
end)
 in ( LrTable.NT 8, ( result, Expr1left, Expr1right), rest671)
end
|  ( 45, ( ( _, ( _, UNDER1left, UNDER1right)) :: rest671)) => let
 val  result = MlyValue.CondExpr (fn _ => (NONE))
 in ( LrTable.NT 8, ( result, UNDER1left, UNDER1right), rest671)
end
|  ( 46, ( ( _, ( _, _, DIRPAR1right)) :: ( _, ( _, ESQPAR1left, _))
 :: rest671)) => let val  result = MlyValue.Args (fn _ => ([]))
 in ( LrTable.NT 9, ( result, ESQPAR1left, DIRPAR1right), rest671)
end
|  ( 47, ( ( _, ( _, _, DIRPAR1right)) :: ( _, ( MlyValue.Params 
Params1, _, _)) :: ( _, ( _, ESQPAR1left, _)) :: rest671)) => let val 
 result = MlyValue.Args (fn _ => let val  (Params as Params1) = 
Params1 ()
 in (Params)
end)
 in ( LrTable.NT 9, ( result, ESQPAR1left, DIRPAR1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, 
TypedVar1right)) :: rest671)) => let val  result = MlyValue.Params (fn
 _ => let val  (TypedVar as TypedVar1) = TypedVar1 ()
 in ([TypedVar])
end)
 in ( LrTable.NT 10, ( result, TypedVar1left, TypedVar1right), rest671
)
end
|  ( 49, ( ( _, ( MlyValue.Params Params1, _, Params1right)) :: _ :: (
 _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, _)) :: rest671)) =>
 let val  result = MlyValue.Params (fn _ => let val  (TypedVar as 
TypedVar1) = TypedVar1 ()
 val  (Params as Params1) = Params1 ()
 in ([TypedVar]@Params)
end)
 in ( LrTable.NT 10, ( result, TypedVar1left, Params1right), rest671)

end
|  ( 50, ( ( _, ( MlyValue.NAME NAME1, _, NAME1right)) :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.TypedVar (fn _ => let val  (Type as Type1) = Type1 ()
 val  (NAME as NAME1) = NAME1 ()
 in ((Type, NAME))
end)
 in ( LrTable.NT 11, ( result, Type1left, NAME1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.AtomicType AtomicType1, AtomicType1left, 
AtomicType1right)) :: rest671)) => let val  result = MlyValue.Type (fn
 _ => let val  (AtomicType as AtomicType1) = AtomicType1 ()
 in (AtomicType)
end)
 in ( LrTable.NT 12, ( result, AtomicType1left, AtomicType1right), 
rest671)
end
|  ( 52, ( ( _, ( _, _, DIRPAR1right)) :: ( _, ( MlyValue.Types Types1
, _, _)) :: ( _, ( _, ESQPAR1left, _)) :: rest671)) => let val  result
 = MlyValue.Type (fn _ => let val  (Types as Types1) = Types1 ()
 in (ListT Types)
end)
 in ( LrTable.NT 12, ( result, ESQPAR1left, DIRPAR1right), rest671)

end
|  ( 53, ( ( _, ( _, _, DIRCOL1right)) :: ( _, ( MlyValue.Type Type1,
 _, _)) :: ( _, ( _, ESQCOL1left, _)) :: rest671)) => let val  result
 = MlyValue.Type (fn _ => let val  (Type as Type1) = Type1 ()
 in (SeqT Type)
end)
 in ( LrTable.NT 12, ( result, ESQCOL1left, DIRCOL1right), rest671)

end
|  ( 54, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (FunT(Type1, Type2))
end)
 in ( LrTable.NT 12, ( result, Type1left, Type2right), rest671)
end
|  ( 55, ( ( _, ( _, NULL1left, NULL1right)) :: rest671)) => let val  
result = MlyValue.AtomicType (fn _ => (ListT []))
 in ( LrTable.NT 13, ( result, NULL1left, NULL1right), rest671)
end
|  ( 56, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.AtomicType (fn _ => (BoolT))
 in ( LrTable.NT 13, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 57, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.AtomicType (fn _ => (IntT))
 in ( LrTable.NT 13, ( result, INT1left, INT1right), rest671)
end
|  ( 58, ( ( _, ( _, _, DIRPAR1right)) :: ( _, ( MlyValue.Type Type1,
 _, _)) :: ( _, ( _, ESQPAR1left, _)) :: rest671)) => let val  result
 = MlyValue.AtomicType (fn _ => let val  (Type as Type1) = Type1 ()
 in (Type)
end)
 in ( LrTable.NT 13, ( result, ESQPAR1left, DIRPAR1right), rest671)

end
|  ( 59, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Types (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in ([Type1, Type2])
end)
 in ( LrTable.NT 14, ( result, Type1left, Type2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.Types Types1, _, Types1right)) :: _ :: ( _,
 ( MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result
 = MlyValue.Types (fn _ => let val  Type1 = Type1 ()
 val  (Types as Types1) = Types1 ()
 in ([Type1] @ Types)
end)
 in ( LrTable.NT 14, ( result, Type1left, Types1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun RECUR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun DIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun MENOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun MENOREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOISPONTOS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun QUAPONTOS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun PONTVIRG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun SHARP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun ESQCHAVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun DIRCHAVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun ESQPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun DIRPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun ESQCOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun DIRCOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PRODUZ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun TPRODUZ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun VIRGULA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun NULL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun ANONFUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun TAIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun HEAD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ISEMPTY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun EXCL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.NAME (fn () => i),p1,p2))
fun CINT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.CINT (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
end
end
