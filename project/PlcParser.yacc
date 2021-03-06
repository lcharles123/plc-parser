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
    WITH |
    MATCH |
    NAME of string
    EOF

(* tokens para os simbolos nao terminais, tudo dentro de < > , 
aqui entra os tipos de nao terminais em sintaxe abstrata, ou seja, conforme definido em datatypes no arquivo Absyn.sml *)

%nonterm Prog of expr


(*regras de associatividade*)
%right PONTVIRG 
%left EQ DIF
%left MENOR MENOREQ
%left QUAPONTOS
%left MINUS PLUS DIV MUL
%left ESQCOL
(*%nonassoc NOT HD TL ISE PRINT NAME*)
(*%nonassoc IF*)


(*retornar o %term EOF ao endontrar o fim do fluxo*)
%eop EOF

%noshift EOF

(*simbolo inicial*)
%start Prog

%%

(*basta colocar as regras de producao aqui usando os simbolos representados pelos tokens definidos no bloco acima*)
(*Regras de Produção especificadas na subseção 3.1 da especificação do TP*)

Prog : Expr (Expr)
    |  Dec1 PONTVIRG Prog () (*Criar funcao de tratamento*)

Dec1 : VAR NAME EQ Expr ()
    |  FUN 


