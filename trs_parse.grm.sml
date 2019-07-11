functor TRSLrValsFun(structure Token : TOKEN) = 
struct
structure ParserData=
struct
structure Header = 
struct
(* $Id: trs_parse.grm,v 1.0 2005/01/16 kusakari Exp $
**
**  ver 1.0: 2005/01/16 by KUSAKARI Keiichirou
*)

open TRS;

datatype dTRS = dTerm of Term
              | dRule of Rule
              | dEQ of Equation


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\005\000\002\000\004\000\000\000\
\\001\000\003\000\014\000\005\000\013\000\000\000\
\\001\000\008\000\000\000\000\000\
\\017\000\000\000\
\\018\000\000\000\
\\019\000\006\000\007\000\007\000\006\000\000\000\
\\020\000\000\000\
\\021\000\004\000\008\000\000\000\
\\022\000\000\000\
\\023\000\000\000\
\\024\000\000\000\
\"
val actionRowNumbers =
"\000\000\005\000\007\000\006\000\
\\000\000\000\000\000\000\003\000\
\\004\000\008\000\001\000\009\000\
\\000\000\010\000\002\000"
val gotoT =
"\
\\001\000\014\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\007\000\000\000\
\\002\000\008\000\000\000\
\\002\000\010\000\003\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\010\000\003\000\013\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 15
val numrules = 8
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | FSYM of unit ->  (string) | VSYM of unit ->  (string)
 | terms of unit ->  (Term list) | term of unit ->  (Term)
 | dtrs of unit ->  (dTRS)
end
type svalue = MlyValue.svalue
type result = dTRS
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 7) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 7) => true | _ => false
val showTerminal =
fn (T 0) => "VSYM"
  | (T 1) => "FSYM"
  | (T 2) => "COMMA"
  | (T 3) => "LPAR"
  | (T 4) => "RPAR"
  | (T 5) => "EQ"
  | (T 6) => "ARROW"
  | (T 7) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.term term2, _, term2right)) :: _ :: ( _, ( 
MlyValue.term term1, term1left, _)) :: rest671)) => let val  result = 
MlyValue.dtrs (fn _ => let val  term1 = term1 ()
 val  term2 = term2 ()
 in (dRule (term1, term2))
end)
 in ( LrTable.NT 0, ( result, term1left, term2right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.term term2, _, term2right)) :: _ :: ( _, ( 
MlyValue.term term1, term1left, _)) :: rest671)) => let val  result = 
MlyValue.dtrs (fn _ => let val  term1 = term1 ()
 val  term2 = term2 ()
 in (dEQ (term1, term2))
end)
 in ( LrTable.NT 0, ( result, term1left, term2right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.term term1, term1left, term1right)) :: 
rest671)) => let val  result = MlyValue.dtrs (fn _ => let val  (term
 as term1) = term1 ()
 in (dTerm term)
end)
 in ( LrTable.NT 0, ( result, term1left, term1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.VSYM VSYM1, VSYM1left, VSYM1right)) :: 
rest671)) => let val  result = MlyValue.term (fn _ => let val  (VSYM
 as VSYM1) = VSYM1 ()
 in (Node (VSym (VSYM, 0), []))
end)
 in ( LrTable.NT 1, ( result, VSYM1left, VSYM1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.FSYM FSYM1, FSYM1left, FSYM1right)) :: 
rest671)) => let val  result = MlyValue.term (fn _ => let val  (FSYM
 as FSYM1) = FSYM1 ()
 in (Node (FSym FSYM, []))
end)
 in ( LrTable.NT 1, ( result, FSYM1left, FSYM1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.terms terms1, _, terms1right)) :: _ :: ( _, 
( MlyValue.FSYM FSYM1, FSYM1left, _)) :: rest671)) => let val  result
 = MlyValue.term (fn _ => let val  (FSYM as FSYM1) = FSYM1 ()
 val  (terms as terms1) = terms1 ()
 in (Node (FSym FSYM, terms))
end)
 in ( LrTable.NT 1, ( result, FSYM1left, terms1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.term term1, 
term1left, _)) :: rest671)) => let val  result = MlyValue.terms (fn _
 => let val  (term as term1) = term1 ()
 in ([term])
end)
 in ( LrTable.NT 2, ( result, term1left, RPAR1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.terms terms1, _, terms1right)) :: _ :: ( _, 
( MlyValue.term term1, term1left, _)) :: rest671)) => let val  result
 = MlyValue.terms (fn _ => let val  (term as term1) = term1 ()
 val  (terms as terms1) = terms1 ()
 in (term::terms)
end)
 in ( LrTable.NT 2, ( result, term1left, terms1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.dtrs x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : TRS_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VSYM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VSYM (fn () => i),p1,p2))
fun FSYM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.FSYM (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
end
end
