(* $Id: read.sml,v 1.0 2005/01/16 kusakari Exp $
**
**  ver 1.0: 2005/01/16 by KUSAKARI Keiichirou
*)

signature SigRead = sig

  val rdterm : string -> TRS.Term
  val rdrule : string -> TRS.Rule
  val rdrules: string list -> TRS.Rule list
  val rdeq   : string -> TRS.Equation
  val rdeqs  : string list -> TRS.Equation list

end;

structure Read : SigRead = struct

  structure TRSLrVals =
      TRSLrValsFun (structure Token = LrParser.Token);

  (* exception TRSLex.LexError *)
  structure TRSLex = 
      TRSLexFun (structure Tokens = TRSLrVals.Tokens);

  (* exception TRSParser.ParseError *)
  structure TRSParser =
      Join(structure ParserData = TRSLrVals.ParserData
           structure Lex = TRSLex
           structure LrParser = LrParser);

  fun parse_error (s, _, _) =
      (TextIO.output (TextIO.stdOut, String.concat ["Error: ", s, "\n"]);
       raise TRSParser.ParseError); 

  fun read s =
      let
          val yyread = ref false;
          fun yyinput _ = if !yyread then "" else (yyread := true; s);
          val lexer = TRSParser.makeLexer yyinput;
          val (result, _ ) = TRSParser.parse (0, lexer, parse_error, ());
      in
          result
      end;

  fun rdterm s =
      case read s
        of TRSLrVals.ParserData.Header.dTerm t => t
         | _ => raise TRSParser.ParseError;

  fun rdrule s =
      case read s
        of TRSLrVals.ParserData.Header.dRule r => r
         | _ => raise TRSParser.ParseError;

  fun rdrules ss = map rdrule ss;

  fun rdeq s =
      case read s
        of TRSLrVals.ParserData.Header.dEQ e => e
         | _ => raise TRSParser.ParseError;

  fun rdeqs ss = map rdeq ss;

end;
