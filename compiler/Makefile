all:lex yacc parser

lex:core.lex
	mllex core.lex

yacc:core.grm
	mlyacc core.grm

parser:
	mlton -output compiler compiler.mlb

clean:
	rm core.lex.sml core.grm.sml core.grm.sig core.grm.desc compiler
