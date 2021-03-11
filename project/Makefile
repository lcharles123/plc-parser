LEX=ml-lex
YACC=ml-yacc
RLWRAP=`which rlwrap`

padrao: r

r: x y	
	$(RLWRAP) sml testParser.sml
	# modo iterativo, edite o Makefile e remova ou adicione "<" para alterar, ex.:	sml < testParser.sml

x: *.lex
	$(LEX) $^

y: *.yacc
	$(YACC) $^ 


cases: x y
	$(RLWRAP) sml testParserCases.sml

h:
	@printf "\n\tLinguagens de Programação - UFMG 2020/2\n"
	@printf "\tCódigo em Standard ML\n\n"
	@printf "\tModo de usar:\n\n"
	@printf "\tmake [Targets]\n\n"
	@printf "\tLista de Targets:\n"
	@printf "\tx : montar .lex\n"
	@printf "\ty : montar .yacc\n"
	@printf "\tr : executar cola (Padrão)\n"
	@printf "\th : mostrar esta ajuda\n\n"
