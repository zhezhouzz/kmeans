all:lex yacc

lex: dsl.lex
	mllex dsl.lex

yacc: dsl.grm
	mlyacc dsl.grm

clean:
	rm dsl.lex.sml dsl.grm.sml dsl.grm.sig dsl.grm.desc
