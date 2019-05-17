all:lex yacc parser

lex:untyped.lex
	mllex untyped.lex

yacc:untyped.grm
	mlyacc untyped.grm

parser:
	mlton -output compiler compiler.mlb

clean:
	rm untyped.lex.sml untyped.grm.sml untyped.grm.sig untyped.grm.desc compiler
