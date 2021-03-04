%%

%name PlcParser

%pos int

(* tokens que para representar todos os simbolos terminais, 
ie. tudo que nao esta dentro de < > nas regras da GLC usando absyn.sml como estrutura *)
%term TEST | EOF

(* tokens para os simbolos nao terminais, tudo dentro de < > , 
aqui entra os tipos de nao terminais em sintaxe abstrata, ou seja, conforme definido em datatypes no arquivo Absyn.sml *)

%nonterm 

(*regras de associatividade*)
%right 
%right 
%left 
%left 
%left 


(*retornar o %term EOF ao endontrar o fim do fluxo*)
%eop EOF

%noshift EOF

(*simbolo inicial*)
%start Prog

%%

(*basta colocar as regras de producao aqui usando os simbolos representados pelos tokens definidos no bloco acima*)


