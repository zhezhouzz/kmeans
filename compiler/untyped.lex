structure Tokens = Tokens
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type svalue = Tokens.svalue
type lexresult = (svalue,pos) token
val line = ref 1
val debugFlag = ref false
val eof = fn () => Tokens.EOF(!line,!line)
val debug = fn s => if (!debugFlag) then print s else () 
(*
  Untyped_TOKENS defined using term declaration in grm
 <INITIAL>{integer} => (debug ("var: "^yytext^"\n"); Tokens.NUMB(yytext,!line,!line));
 (* <INITIAL>{optsign}{integer}({frac}{exp}?|{frac}?{exp}) => (debug ("var: "^yytext^"\n"); Tokens.NUMB(yytext,!line,!line)); *)
*)
%%
%header (functor UntypedLexFun (structure Tokens : Untyped_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
variable={alpha}({alpha}|{digit})*;
optsign=("~")?;
integer={digit}+;
frac="."{digit}+;
exp=(e|E){optsign}{digit}+;
eol=("\n"|"\013\n"|"\013");
ws=[\ \t];
%%
<INITIAL>{eol} => (line := (!line)+1; lex());
<INITIAL>{ws}+ => (debug "whitespace"; lex());
<INITIAL>("=>") => (debug "dot\n"; Tokens.DOT(!line,yypos));
<INITIAL>("fn") => (debug "lam\n";Tokens.LAM(!line,yypos));
<INITIAL>("map") => (debug "map\n";Tokens.MAP(!line,yypos));
<INITIAL>("foldl") => (debug "foldl\n";Tokens.FOLDL(!line,yypos));
<INITIAL>("(") => (debug "lparen\n"; Tokens.LPAREN(!line,yypos));
<INITIAL>(")") => (debug "rparen\n"; Tokens.RPAREN(!line,yypos));
<INITIAL>("+") => (debug "rparen\n"; Tokens.ADD(!line,yypos));
<INITIAL>("*") => (debug "rparen\n"; Tokens.MUL(!line,yypos));
<INITIAL>("=") => (debug "rparen\n"; Tokens.EQ(!line,yypos));
<INITIAL>("<") => (debug "rparen\n"; Tokens.LES(!line,yypos));
<INITIAL>(">") => (debug "rparen\n"; Tokens.GRE(!line,yypos));
<INITIAL>{variable} => (debug ("var: "^yytext^"\n"); Tokens.VAR(yytext,!line,yypos));
<INITIAL>{optsign}{integer}({frac})? => (debug ("var: "^yytext^"\n"); Tokens.NUMB(yytext,!line,!line));
