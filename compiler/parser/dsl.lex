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
variableb={alpha}({alpha}|{digit})*;
variable={variableb}("."{variableb})?;
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
<INITIAL>("->") => (debug "arrow\n"; Tokens.CARROW(!line,yypos));
<INITIAL>("fn") => (debug "lam\n";Tokens.LAM(!line,yypos));
<INITIAL>("if") => (debug "lam\n";Tokens.IF(!line,yypos));
<INITIAL>("then") => (debug "lam\n";Tokens.THEN(!line,yypos));
<INITIAL>("else") => (debug "rparen\n"; Tokens.ELSE(!line,yypos));
<INITIAL>("fst") => (debug "rparen\n"; Tokens.FST(!line,yypos));
<INITIAL>("snd") => (debug "rparen\n"; Tokens.SND(!line,yypos));

<INITIAL>("map") => (debug "map\n";Tokens.MAP(!line,yypos));
<INITIAL>("mapi") => (debug "map\n";Tokens.MAPI(!line,yypos));
<INITIAL>("foldl") => (debug "foldl\n";Tokens.FOLDL(!line,yypos));
<INITIAL>("foldli") => (debug "foldl\n";Tokens.FOLDLI(!line,yypos));
<INITIAL>("nth") => (debug "nth\n";Tokens.NTH(!line,yypos));

<INITIAL>("amap") => (debug "map\n";Tokens.AMAP(!line,yypos));
<INITIAL>("amapi") => (debug "map\n";Tokens.AMAPI(!line,yypos));
<INITIAL>("afoldl") => (debug "foldl\n";Tokens.AFOLDL(!line,yypos));
<INITIAL>("afoldli") => (debug "foldl\n";Tokens.AFOLDLI(!line,yypos));
<INITIAL>("anth") => (debug "nth\n";Tokens.ANTH(!line,yypos));

<INITIAL>("loop") => (debug "nth\n";Tokens.LOOP(!line,yypos));

<INITIAL>("int") => (debug "int\n";Tokens.CINT(!line,yypos));
<INITIAL>("real") => (debug "real\n";Tokens.CREAL(!line,yypos));
<INITIAL>("bool") => (debug "real\n";Tokens.CBOOL(!line,yypos));
<INITIAL>("unit") => (debug "real\n";Tokens.CUNIT(!line,yypos));

<INITIAL>("list") => (debug "list\n";Tokens.CLIST(!line,yypos));
<INITIAL>("apprlist") => (debug "list\n";Tokens.CAPPRLIST(!line,yypos));

<INITIAL>("true") => (debug "list\n";Tokens.TRUE(!line,yypos));
<INITIAL>("false") => (debug "list\n";Tokens.FALSE(!line,yypos));

<INITIAL>("(") => (debug "lparen\n"; Tokens.LPAREN(!line,yypos));
<INITIAL>(")") => (debug "rparen\n"; Tokens.RPAREN(!line,yypos));

<INITIAL>("+.") => (debug "rparen\n"; Tokens.ADDR(!line,yypos));
<INITIAL>("*.") => (debug "rparen\n"; Tokens.MULR(!line,yypos));
<INITIAL>("/.") => (debug "rparen\n"; Tokens.DIVR(!line,yypos));
<INITIAL>("<.") => (debug "rparen\n"; Tokens.LESR(!line,yypos));
<INITIAL>(">.") => (debug "rparen\n"; Tokens.GRER(!line,yypos));

<INITIAL>("=#") => (debug "rparen\n"; Tokens.EQB(!line,yypos));

<INITIAL>("+") => (debug "rparen\n"; Tokens.ADD(!line,yypos));
<INITIAL>("*") => (debug "rparen\n"; Tokens.MUL(!line,yypos));
<INITIAL>("/") => (debug "rparen\n"; Tokens.DIV(!line,yypos));
<INITIAL>("=") => (debug "rparen\n"; Tokens.EQ(!line,yypos));
<INITIAL>("<") => (debug "rparen\n"; Tokens.LES(!line,yypos));
<INITIAL>(">") => (debug "rparen\n"; Tokens.GRE(!line,yypos));

<INITIAL>(":") => (debug "rparen\n"; Tokens.COLON(!line,yypos));
<INITIAL>("#") => (debug "rparen\n"; Tokens.SHARP(!line,yypos));
<INITIAL>(",") => (debug "rparen\n"; Tokens.COMMA(!line,yypos));
<INITIAL>("_") => (debug "rparen\n"; Tokens.WILD(!line,yypos));
<INITIAL>{variable} => (debug ("var: "^yytext^"\n"); Tokens.VAR(yytext,!line,yypos));
<INITIAL>{optsign}{integer} => (debug ("var: "^yytext^"\n"); Tokens.NUMBI(yytext,!line,!line));
<INITIAL>{optsign}{integer}{frac} => (debug ("var: "^yytext^"\n"); Tokens.NUMBR(yytext,!line,!line));
