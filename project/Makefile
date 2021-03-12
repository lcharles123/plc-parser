LEX=ml-lex
YACC=ml-yacc
RLWRAP=`which rlwrap`


run: lex yacc	
	$(RLWRAP) sml testParser.sml
	# modo iterativo, edite o Makefile e remova ou adicione "<" para alterar, ex.:	sml < testParser.sml

lex: PlcLexer.lex
	$(LEX) $^

yacc: PlcParser.yacc
	$(YACC) $^ 

help:
	@printf "\n\tLinguagens de Programação - UFMG 2020/2\n"
	@printf "\tCódigo em Standard ML\n\n"
	@printf "\tModo de usar:\n\n"
	@printf "\tmake [Targets]\n\n"
	@printf "\tLista de Targets:\n"
	@printf "\tlex : montar .lex\n"
	@printf "\tyacc : montar .yacc\n"
	@printf "\trun : executar cola e testes (Padrão)\n"
	@printf "\thelp : mostrar esta ajuda\n\n"
