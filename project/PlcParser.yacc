%%

%name PlcParser

%pos int

(* tokens que para representar todos os simbolos terminais, 
ie. tudo que nao esta dentro de < > nas regras da GLC usando absyn.sml como estrutura *)
%term VAR |
    FUN | RECUR
    MINUS | PLUS | MUL | DIV |
    EQ | DIF | MENOR | MENOREQ |
    QUAPONTOS | PONTVIRG | SHARP | UNDER |
    ESQCHAVE | DIRCHAVE |
    ESQPAR | DIRPAR |
    ESQCOL | DIRCOL |
    PRODUZ | TPRODUZ |
    VIRGULA |
    IF | THEN | ELSE |
    INT | NULL | BOOL |
    TRUE | FALSE |
    ANONFUN | END
    TAIL | HEAD | 
    ISEMPTY |
    PRINT | 
    EXCL |
    MATCH | WITH |
    NAME of string | CINT of  int
    EOF

(* tokens para os simbolos nao terminais, tudo dentro de < > , 
aqui entra os tipos de nao terminais em sintaxe abstrata, ou seja, conforme definido em datatypes no arquivo Absyn.sml *)

%nonterm Prog of expr
    | Dec1 of (*DUVIDA*)
    | Expr of expr
    | AtomicExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list
    | CondExpr of (*DUVIDA*)
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomicType of plcType
    | Types of plcType list

(*regras de associatividade*)
(*Regras definidas de acordo com a subseção 3.3 da descrição do TP*)
%right PONTVIRG
%nonassoc IF
%left ELSE
%left AND
%left EQ DIF
%left MENOR MENOREQ
%right QUAPONTOS
%left MINUS PLUS
%left DIV MUL
%nonassoc EXCL HEAD TAIL ISEMPTY PRINT NAME
%left ESQCOL


(*retornar o %term EOF ao endontrar o fim do fluxo*)
%eop EOF

%noshift EOF

(*simbolo inicial*)
%start Prog

%%

(*basta colocar as regras de producao aqui usando os simbolos representados pelos tokens definidos no bloco acima*)
(*Regras de Produção especificadas na subseção 3.1 da especificação do TP*)
(*Para o preenchimento deve-se observar os datatypes descritos em Absyn.sml*)
Prog : Expr (Expr)
    |  Dec1 PONTVIRG Prog () (*Criar funcao de tratamento*)

Dec1 : VAR NAME EQ Expr ()
    |  FUN NAME Args EQ Expr ()
    |  FUN RECUR NAME Args DOISPONTOS EQ Expr ()

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

AtomicExpr : Const ()
    | NAME ()
    | ESQCHAVE Prog DIRCHAVE ()
    | ESQPAR Expr DIRPAR ()
    | ESQPAR Comps DIRPAR ()
    | ANONFUN Args PRODUZ Expr END ()

AppExpr : AtomicExpr AtomicExpr ()
    | AppExpr AtomicExpr ()

Const : TRUE ()
    | FALSE ()
    | CINT ()
    | ESQPAR DIRPAR ()
    | ESQPAR Type ESQCOL DIRCOL DIRPAR ()

Comps :  Expr VIRGULA Expr ()
    | Expr VIRGULA Comps ()

MatchExpr : END ()
    | PIPE CondExpr TPRODUZ Expr MatchExpr ()

CondExpr : Expr ()
    | UNDER ()

Args : ESQPAR DIRPAR () 
    | ESQPAR Params DIRPAR ()

Params : TypedVar ()
    | TypedVar VIRGULA Params ()

TypedVar : Type NAME ()

Type : AtomicType ()
    | ESQPAR Types DIRPAR ()
    | ESQCOL Type DIRCOL ()
    | Type TPRODUZ Type ()

AtomicType : NULL ()
    | BOOL ()
    | INT ()
    | ESQPAR Type DIRPAR ()

Types: Type VIRGULA Type ()
    | Type VIRGULA Types ()








