LEX=ml-lex
YACC=ml-yacc
RLWRAP=`which rlwrap`

main: help

testg: lex yacc	
	$(RLWRAP) sml < testParser.sml
	# modo iterativo, edite o Makefile e remova ou adicione "<" para alterar, ex.:	sml < testParser.sml

lex: PlcLexer.lex
	$(LEX) $^

yacc: PlcParser.yacc
	$(YACC) $^ 

plc: lex yacc
	$(RLWRAP) sml Plc.sml
	# modo iterativo, edite o Makefile e remova ou adicione "<" para alterar, ex.:	sml < Plc.sml
test: lex yacc
	$(RLWRAP) sml < testInterp.sml
	# modo iterativo, edite o Makefile e remova ou adicione "<" para alterar, ex.:	sml < PlcInterp.sml

help:
	@printf "\n\tLinguagens de Programação - UFMG 2020/2\n"
	@printf "\tTP: Interpretador para PLC - Código em Standard ML\n\n"
	@printf "\tLeia o README.txt:\n\n"
	@printf "\tModo de usar:\n\n"
	@printf "\tmake [Targets]\n\n"
	@printf "\tLista de Targets:\n"
	@printf "\tplc  : executar o interpretador completo\n"
	@printf "\tlex  : montar .lex\n"
	@printf "\tyacc : montar .yacc\n"
	@printf "\ttest : executar testes do interpretador\n"
	@printf "\ttestg: executar testes do gerador sintatico\n"
	@printf "\thelp : mostrar esta ajuda\n\n"

	

