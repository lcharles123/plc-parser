LEX=ml-lex
YACC=ml-yacc

padrao: r

r: x y
	sml testParser.sml

x: *.lex
	$(LEX) $^

y: *.yacc
	$(YACC) $^





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
