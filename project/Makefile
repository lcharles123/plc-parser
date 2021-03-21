LEX=ml-lex
YACC=ml-yacc
RLWRAP=`which rlwrap`

main: plc

test: lex yacc	
	$(RLWRAP) sml  testParser.sml
	# modo iterativo, edite o Makefile e remova ou adicione "<" para alterar, ex.:	sml < testParser.sml

lex: PlcLexer.lex
	$(LEX) $^

yacc: PlcParser.yacc
	$(YACC) $^ 

interp: lex yacc
	$(RLWRAP) sml < PlcInterp.sml
	# modo iterativo, edite o Makefile e remova ou adicione "<" para alterar, ex.:	sml < testParser.sml
checker: lex yacc
	$(RLWRAP) sml < PlcChecker.sml
	# modo iterativo, edite o Makefile e remova ou adicione "<" para alterar, ex.:	sml < testParser.sml
plc: lex yacc
	$(RLWRAP) sml < Plc.sml
	# modo iterativo, edite o Makefile e remova ou adicione "<" para alterar, ex.:	sml < testParser.sml


help:
	@printf "\n\tLinguagens de Programação - UFMG 2020/2\n"
	@printf "\tCódigo em Standard ML\n\n"
	@printf "\tModo de usar:\n\n"
	@printf "\tmake [Targets]\n\n"
	@printf "\tLista de Targets:\n"
	@printf "\ttest : executar cola e testes\n"
	@printf "\tlex  : montar .lex\n"
	@printf "\tyacc : montar .yacc\n"
	@printf "\trun  : executar o interpretador completo (Padrão)\n"
	@printf "\thelp : mostrar esta ajuda\n\n"

	

