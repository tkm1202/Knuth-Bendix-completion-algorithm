signature TRS_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val FSYM: (string) *  'a * 'a -> (svalue,'a) token
val VSYM: (string) *  'a * 'a -> (svalue,'a) token
end
signature TRS_LRVALS=
sig
structure Tokens : TRS_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
