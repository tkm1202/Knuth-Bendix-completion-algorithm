(* $Id: trs_lex.lex,v 1.1 2007/03/05 nishida Exp $
**
**  ver 1.0: 2005/01/16 by KUSAKARI Keiichirou
**  ver 1.1: 2007/03/05 by NISHIDA Naoki
*)

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ref 1;
fun eof () = Tokens.EOF (!lineNum, !lineNum);
fun error (e, _, _) =
    TextIO.output
      (TextIO.stdOut, "Error: ignoring bad character: "^e^"\n");

%%
%header (functor TRSLexFun (structure Tokens: TRS_TOKENS));

alpha=[a-z];
Alpha=[A-Z];
digit=[0-9];
alnum=[A-Za-z0-9?'];
ws = [\ \t];

%%
%"\r\n"=> (lineNum := (!lineNum) + 1; lex ());
"\013\n"=> (lineNum := (!lineNum) + 1; lex ());
"\n"  => (lineNum := (!lineNum) + 1; lex ());
%"\r"  => (lineNum := (!lineNum) + 1; lex ());
"\013"  => (lineNum := (!lineNum) + 1; lex ());
{ws}+ => (lex ());
"->"  => (Tokens.ARROW     (!lineNum, !lineNum));
","   => (Tokens.COMMA     (!lineNum, !lineNum));
"="   => (Tokens.EQ        (!lineNum, !lineNum));
"("   => (Tokens.LPAR      (!lineNum, !lineNum));
")"   => (Tokens.RPAR      (!lineNum, !lineNum));

{alpha}{alnum}* => (Tokens.VSYM (yytext, !lineNum, !lineNum));
{Alpha}{alnum}* => (Tokens.FSYM (yytext, !lineNum, !lineNum));
{digit} => (Tokens.FSYM (yytext, !lineNum, !lineNum));

. => (error (yytext, !lineNum, !lineNum);
      raise LexError);
