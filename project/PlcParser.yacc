%%

%name PlcParser

%pos int

(* tokens que para representar todos os simbolos terminais, 
ie. tudo que nao esta dentro de < > nas regras da GLC usando absyn.sml como estrutura *)

%term VAR 
    | FUN | RECUR
    | MINUS | PLUS | MUL | DIV 
    | EQ | DIF | MENOR | MENOREQ | AND | PIPE
    | DOISPONTOS | QUAPONTOS | PONTVIRG | SHARP | UNDER 
    | ESQCHAVE | DIRCHAVE 
    | ESQPAR | DIRPAR 
    | ESQCOL | DIRCOL 
    | PRODUZ | TPRODUZ 
    | VIRGULA
    | IF | THEN | ELSE 
    | INT | NULL | BOOL 
    | TRUE | FALSE 
    | ANONFUN | END
    | TAIL | HEAD  
    | ISEMPTY 
    | PRINT  
    | EXCL 
    | MATCH | WITH 
    | NAME of string | CINT of  int
    | EOF

(* tokens para os simbolos nao terminais, tudo dentro de < > , 
aqui entra os tipos de nao terminais em sintaxe abstrata, ou seja, conforme definido em datatypes no arquivo Absyn.sml *)

%nonterm Prog of expr
    | Dec1 of expr
    | Expr of expr
    | AtomicExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomicType of plcType
    | Types of plcType list

(*regras de associatividade*)
(*Regras definidas de acordo com a subseção 3.3 da descrição do TP*)
%right PONTVIRG TPRODUZ
%nonassoc IF
%left ELSE
%left AND
%left EQ DIF
%left MENOR MENOREQ
%right QUAPONTOS
%left MINUS PLUS
%left DIV MUL
%nonassoc EXCL HEAD TAIL ISEMPTY PRINT NAME (*nas regras EXCL = '!' , ja na associatividade esta escrito 'not' provavelmente sao a mesma coisa *)
%left ESQCOL


(*retornar o %term EOF ao endontrar o fim do fluxo*)
%eop EOF

%noshift EOF

(*simbolo inicial*)
%start Prog
%verbose
%%

(*  operator domain: string * (plcType * string) list * plcType * expr * expr
  operand:         (plcType * string) list * expr * string
  in expression:
    makeFun (Args,Expr,"whatttt")*)

(*basta colocar as regras de producao aqui usando os simbolos representados pelos tokens definidos no bloco acima*)
(*Regras de Produção especificadas na subseção 3.1 da especificação do TP*)
(*Para o preenchimento deve-se observar os datatypes descritos em Absyn.sml*)
Prog : Expr (Expr)
    |  Dec1 (Dec1) (*Criar funcao de tratamento*)

Dec1 : VAR NAME EQ Expr PONTVIRG Prog (Let(NAME, Expr, Prog)) (*Let ja retorna para vars ou fun's*)


    |  FUN NAME Args EQ Expr (Let(NAME, makeAnon(Args, Expr), Expr)) (*TODO*)
    
    |  FUN RECUR NAME Args DOISPONTOS Type EQ Expr (makeFun(NAME, Args, Type, Expr, Expr )) (*TODO*)

(*

fn (Int x) => -x end
Anon (IntT,"x",Prim1 ("-",Var "x")) : expr

Let  NAME  mkanon                     call
fun f(Int x) = x; f(1)  ====  Let ("f", Anon (IntT, "x", Var "x"), Call ("f", ConI 1))
                              Let   NAME  , AtomicExpr(Expr) , Expr 
Let
    ("f",Anon (IntT,"x",Prim2 (";",Var "x",Call (Var "f",ConI 1))),
     Prim2 (";",Var "x",Call (Var "f",ConI 1))) : expr
*)
(*Tipo expr*)
Expr : AtomicExpr (AtomicExpr)
    |  AppExpr (AppExpr)
    |  IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    |  MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    |  EXCL Expr (Prim1("!", Expr)) (*Todas as chamadas de Prim1 devem passar o str que representa a op (seção 3.1) + Expr*)
    |  MINUS Expr (Prim1("-", Expr))
    |  HEAD Expr (Prim1("hd", Expr))
    |  TAIL Expr (Prim1("tl", Expr))
    |  ISEMPTY Expr (Prim1("ise", Expr))
    |  PRINT Expr (Prim1("print", Expr))
    |  Expr AND Expr (Prim2("&&", Expr1, Expr2))
    |  Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    |  Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    |  Expr MUL Expr (Prim2("*", Expr1, Expr2))
    |  Expr DIV Expr (Prim2("/", Expr1, Expr2))
    |  Expr EQ Expr (Prim2("=", Expr1, Expr2))
    |  Expr DIF Expr (Prim2("!=", Expr1, Expr2))
    |  Expr MENOR Expr (Prim2("<", Expr1, Expr2))
    |  Expr MENOREQ Expr (Prim2("<=", Expr1, Expr2))
    |  Expr QUAPONTOS Expr (Prim2("::", Expr1, Expr2))
    |  Expr PONTVIRG Expr (Prim2(";", Expr1, Expr2))
    |  Expr ESQCOL CINT DIRCOL (Item(CINT, Expr))

(*Tipo expr*)
AtomicExpr : Const (Const)
    | NAME (Var NAME)
    | ESQCHAVE Prog DIRCHAVE (Prog) (*Basta retornar o que esta dentro da chave/parentesis*)
    | ESQPAR Expr DIRPAR (Expr)
    | ESQPAR Comps DIRPAR (List Comps) (*list*)
    | ANONFUN Args PRODUZ Expr END (makeAnon(Args, Expr)) (* makeAnon (PlcParserAux) Create a Anonymus function expression. *)

(*Function application - tipo expr*)
AppExpr : AtomicExpr AtomicExpr (Call(AtomicExpr1, AtomicExpr2)) (*AtomicExpr tem que ser do tipo expr*)
    | AppExpr AtomicExpr ((Call(AppExpr, AtomicExpr))) (*AppExpr tem que ser do tipo expr*)

(*Tipo expr*)
Const : TRUE (ConB true)
    | FALSE (ConB false)
    | CINT (ConI CINT)
    | ESQPAR DIRPAR (List []) (*nil value - Lista vazia*)
    | ESQPAR Type ESQCOL DIRCOL DIRPAR (ESeq Type) (*type-annotated empty sequence*)

(*Tipo expr list*)
Comps : Expr VIRGULA Expr ([Expr1, Expr2]) (*retornar uma lista com as 2 expressões*)
    | Expr VIRGULA Comps ([Expr]@Comps) (* cria uma lista c/ Expr e concatena-se a lista Expr com a Comps*)

(*Tipo (expr option * expr) list*)
MatchExpr : END ([]) (*Lista Vazia*)
    | PIPE CondExpr TPRODUZ Expr MatchExpr ([(CondExpr, Expr)] @ MatchExpr) (*Lista contendo CondExpr e Expr concatenada com outras possiveis MatchExpr*)


(*ENTENDENDO TIPO option:
datatype 'a option = NONE | SOME of 'a
The type option provides a distinction between some value and no value,
and is often used for representing the result of partially defined functions.
It can be viewed as a typed version of the C convention of returning a NULL 
pointer to indicate no value.*)
(*Tipo - expr option*)
CondExpr : Expr (SOME Expr)
    | UNDER (NONE)

(*Tipo: (plcType * string) list*)
Args : ESQPAR DIRPAR ([]) (*Retornar lista vazia - Sem params*) 
    | ESQPAR Params DIRPAR (Params) (*Retornar Params - obs: Params já é uma lista*)

(*Tipo: (plcType * string) list - Como esperado tem que ser o msm de Args*)
Params : TypedVar ([TypedVar]) (*Lista com um unico parâmetro*)
    | TypedVar VIRGULA Params ([TypedVar]@Params)(*Lista de TypedVar concatena c/ os outros params.*)

(*Tipo: plcType * string - tupla com tipo + name dos params *)
TypedVar : Type NAME ((Type, NAME)) (*Basta retornar uma tupla com o tipo do param e o nome dele como indicado na seção 3.1*)

(*Tipo: plcType*)
Type : AtomicType (AtomicType)
    | ESQPAR Types DIRPAR (ListT Types) (*list type*)
    | ESQCOL Type DIRCOL (SeqT Type) (*sequence type*)
    | Type TPRODUZ Type (FunT(Type1, Type2)) (*function type*)

(*Tipo: plcType*)
AtomicType : NULL (ListT []) (*Nil nada mais é que uma list vazia (do tipo plcType)*)
    | BOOL (BoolT)
    | INT (IntT)
    | ESQPAR Type DIRPAR (Type) (*Basta retornar o tipo que esta entre parentesis*)

(*Tipo: plcType list - É simplesmente uma lista de Type acima*)
Types: Type VIRGULA Type ([Type1, Type2]) (*Basta retornar um lista com os 2 tipos*)
    | Type VIRGULA Types ([Type1] @ Types) (*Cria uma lista com Type e concatena com os outros Types (que já é uma lista)*)

