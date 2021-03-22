CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

(* modulos implementados pelos alunos *)
use "PlcChecker.sml";
use "PlcInterp.sml";


Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

print "\n\n\n\n...Iniciando testes...\n\n\n\n";
use "testParserCases.sml";
(*use "Plc.sml"; nao deve ser usado para nao gerar ?.type *)
(* copia da funcao em Plc.sml, por causa do problema do coment acima *)

fun run exp =
    (let
        val env = []
        val resT = teval exp []
        val resV = eval exp []
        
    in
        val2string(resV) ^ " : " ^ type2string(resT) ^ " "
    end) 
    
    handle EmptySeq => "ERRO no Checker: EmptySeq: A sequência de entrada não contém nenhum elemento " 
    |   UnknownType => "ERRO no Checker: UnknownType: Excecao de UnknownType "
    |   NotEqTypes => "ERRO no Checker: NotEqTypes: Comparacao de tipos diferentes "
    |   WrongRetType => "ERRO no Checker: WrongRetType : Tipo de retorno nao concorda "
    |   DiffBrTypes => "ERRO no Checker: DiffBrTypes: Os tipos da expressões dos possíveis caminhos de um If divergem "
    |   IfCondNotBool => "ERRO no Checker: IfCondNotBool: A condição do if não é booleana "
    |   MatchResTypeDiff => "ERRO no Checker: MatchResTypeDiff: O tipo de algum dos casos em match difere dos demais "
    |   MatchCondTypesDiff => "ERRO no Checker: MatchCondTypesDiff: O tipo das opções de match difere do tipo da expressão passada para Match "
    |   CallTypeMisM => "ERRO no Checker: CallTypeMisM: Você está passando pra uma chamada de função um tipo diferente do qual ela suporta "
    |   NotFunc => "ERRO no Checker: NotFunc: Você está tentando chamar algo que não é uma função "
    |   ListOutOfRange => "ERRO no Checker: ListOutOfRange: Tentativa de acessar um elemento fora dos limites da lista "
    |   OpNonList => "ERRO no Checker: OpNonList: Tentativa de acessar um elemento em uma expressão que não é uma lista "
    
    |   SymbolNotFound => "ERRO no Interp: SymbolNotFound: Caractere nao definido como variavel ou construtor " 
    |   Impossible => "ERRO no Interp: Impossible: Nao eh possivel fazer a operacao "
    |   HDEmptySeq => "ERRO no Interp: HDEmptySeq: Nao eh possivel acessar a cabeca da Sequencia vazia "
    |   TLEmptySeq => "ERRO no Interp: TLEmptySeq: Nao eh possivel acessar a cauda da sequencia vazia "
    |   NoMatchResults => "ERRO no Interp: NoMatchResults: Não deu match "
    |   NotAFunc => "ERRO no Interp: NotAFunc: Variavel usada nao eh uma funcao "
    ;




(* testes serao da forma abaixo, sao 60 ao total *)

print "\n\n\n\nExecutando testes....\n\n\n\n";
testeInterp (cases, 0, run);
testeInterp (cases, 1, run);
testeInterp (cases, 2, run);
testeInterp (cases, 3, run);
testeInterp (cases, 4, run);
testeInterp (cases, 5, run);
testeInterp (cases, 6, run);
testeInterp (cases, 7, run);
testeInterp (cases, 8, run);
testeInterp (cases, 9, run);
testeInterp (cases, 10, run);
testeInterp (cases, 11, run);
testeInterp (cases, 12, run);
testeInterp (cases, 13, run);
testeInterp (cases, 14, run);
testeInterp (cases, 15, run);
testeInterp (cases, 16, run);
testeInterp (cases, 17, run);
testeInterp (cases, 18, run);
testeInterp (cases, 19, run);
testeInterp (cases, 20, run);
testeInterp (cases, 21, run);
testeInterp (cases, 22, run);
testeInterp (cases, 23, run);
testeInterp (cases, 24, run);
testeInterp (cases, 25, run);
testeInterp (cases, 26, run);
testeInterp (cases, 27, run);
testeInterp (cases, 28, run);
testeInterp (cases, 29, run);
testeInterp (cases, 30, run);
testeInterp (cases, 31, run);
testeInterp (cases, 32, run);
testeInterp (cases, 33, run);
testeInterp (cases, 34, run);
testeInterp (cases, 35, run);
testeInterp (cases, 36, run);
testeInterp (cases, 37, run);
testeInterp (cases, 38, run);
testeInterp (cases, 39, run);
testeInterp (cases, 40, run);
testeInterp (cases, 41, run);
testeInterp (cases, 42, run);
testeInterp (cases, 43, run);
testeInterp (cases, 44, run);
testeInterp (cases, 45, run);
testeInterp (cases, 46, run);
testeInterp (cases, 47, run);
testeInterp (cases, 48, run);
testeInterp (cases, 49, run);
testeInterp (cases, 50, run);
testeInterp (cases, 51, run);
testeInterp (cases, 52, run);
testeInterp (cases, 53, run);
testeInterp (cases, 54, run);
testeInterp (cases, 55, run);
testeInterp (cases, 56, run);
testeInterp (cases, 57, run);
testeInterp (cases, 58, run);
testeInterp (cases, 59, run);
testeInterp (cases, 60, run);

print "\n\n\n\nFim dos 60 testes de testParserCases.sml....\n\n\n\n";




