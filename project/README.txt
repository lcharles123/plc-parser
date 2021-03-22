Trabalho de Linguagens de Programação - UFMG - 20202

Nomes: Lécio Alves ; Vitor Gabriel

Este trabalho consiste em um Interpretador escrita em Standard ML para a linguagem PLC.

PLC é uma linguagem funcional com suporte a funções de ordem superior e possui tipagem e escopo estáticos. Ela pode ser vista como um subconjunto da linguagem Standard ML de menor tamanho e com a sintaxe diferente.

PLC não possui entrada pelo usuário, mas apenas saída com a função print, ela também possui apenas tipos primitivos, não tendo suporte a tipos abstratos definidos pelo usuário.
Os tipos primitivos são:
Nil: Possui apenas um valor que é a lista vazia () e é o tipo retornado pela função print ou pelo fim do programa.
Bool: Possui 2 valores: true e false, os operadores binários pré-definidos são "&&" para AND, "=" para igualdade e "!=" para desigualdade, além do operador unário de negação "!".
Int: Este é o tipo inteiro com sinal e representação em 32 bits, possui os operadores convencionais + , - , * , / , < , e <=
List: É o tipo para representar listas, ie. ( elem1, elem2,...) , todos os elementos podem ser de qualquer tipo exceto List e Sequence.
Function: São tipos funcionais que podem ser usados como parâmetros nas funções.
Sequence: Pode ser visto como uma generalização do tipo List, possui os operadores: [] Lista vazia; :: construtor de liesta ; ise checa se a lista é vazia; hd retorna o primeiro elemento; tl retorna a lista exceto o primeiro elemento.

Operadores:
if then else 
fn fun hd tl ise matc print rec var with end _ 
:: [] +  -  *  /  < <= && = != ! ()

O trabalho foi feito usando ferramentas para geração automática de parsers: ml-lex e ml-yacc, são implementações SML que vem junto com o smlnj.
O código da implementação se encontra nos arquvos PlcLexer.lex e PlcLexer.yacc, que foram usados para gerar outros arquivos usados no parser. 

Pode-se entrar com codigo direto em run usando outra funcao que transforma uma string em sintaxe abstrata como fromString : string -> expr ou fromFile : string -> expr 

Essa segunda parte do trabalho consiste em construir duas funcoes: 
 - teval em PlcChecker.sml checa a concordancia de tipos da AST 
 - eval: em PlcInterp.sml avalia e retorna um resultado de comandos em AST, dado que os tipos estao corretos.

Para usa-las eh preciso gerar a AST, pode ser feito atraves das funcoes fromString ou fromFile


Para utilizar o programa basta usar o makefile incluso, ao digitar make sera exibida a lista de opcoes. 

Foram incluidos para a segunda parte os testes em testParseCases.sml e alguns da especificacao. Pode-se executa-los atraves de make test







