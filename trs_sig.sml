(* $Id: trs_sig.sml,v 1.1 2004/11/21 kusakari Exp $
**
**  ver 1.0: 1997/08/01 by KUSAKARI Keiichirou
**  ver 1.1: 2004/11/21 by KUSAKARI Keiichirou
*)

signature SigTRS = sig

  datatype Symbol = VSym of string * int
                  | FSym of string;
  datatype Term = Node of Symbol * Term list
  type Pattern
  type Rule
  type RuleSet
  type Equation
  type EquationSet
	   
  type Subst

  val tsize : Pattern -> int
  val tsizelist : Pattern list -> int
  val depth : Term -> int
  val varlist : Term -> (string * int) list
end;

structure TRS : SigTRS = struct

  datatype Symbol = VSym of string * int
                  | FSym of string;
  datatype Term   = Node of Symbol * Term list;
  type Pattern    = Term;
  type Rule       = Pattern * Pattern;
  type RuleSet    = Rule list;
  type Equation   = Pattern * Pattern;
  type EquationSet= Equation list;

  type Subst = (string * int, Term) Util.Assoc;

  fun tsize (Node(VSym _,_)) = 0
    | tsize (Node(f,ts)) = 1 + tsizelist ts
  and tsizelist (t::ts) = tsize t + tsizelist ts
    | tsizelist [] = 0;
  
  fun depth (Node (f, [])) = 0
    | depth (Node (f, ts)) = 1 + depthchoice ts 
  and depthchoice [] = 0
    | depthchoice (t::ts) = if depth t <= depthchoice ts then depthchoice ts
			    else depth t;
  
  open Util;
  
  fun varlist (Node (FSym f, ts)) = varlistlist ts
    | varlist (Node (VSym (f,n),ts)) = Util.union [(f,n)] (varlistlist ts)
  and varlistlist [] = []
    | varlistlist ((Node (FSym g,tss))::ts)
      = varlist (Node (FSym g,tss)) @ varlistlist ts
    | varlistlist ((Node (VSym (g,m),_))::ts)
      = Util.union [(g,m)] (varlistlist ts);
  
end;
