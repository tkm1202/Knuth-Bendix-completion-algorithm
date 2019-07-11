(* $Id: pprint.sml,v 1.1 2004/11/21 kusakari Exp $
**
**  ver 1.0: 1997/08/01 by KUSAKARI Keiichirou
**  ver 1.1: 2004/11/21 by KUSAKARI Keiichirou
*)

signature SigPPrint = sig

  include SigTRS

  val prsym  : Symbol -> unit
  val prterm : Term -> unit
  val prrule : Rule -> unit
  val prrules: RuleSet -> unit
  val preq   : Equation -> unit
  val preqs  : EquationSet -> unit


  val prsubst: Subst -> unit


end;

structure PPrint : SigPPrint = struct

  open TRS;

  fun prsym (VSym (x, 0)) = print x
    | prsym (VSym (x, n)) = print (x^"_"^(Int.toString n))
    | prsym (FSym f) = print f;

  fun prterm (Node (a, [])) = prsym a
    | prterm (Node (a, t::ts)) =
      (prsym a; print "("; prterm t; prterm0 ts)
  and prterm0 [] = print ")"
    | prterm0 (t::ts) = (print ", "; prterm t; prterm0 ts);

  fun prrule (l, r) = (prterm l; print " -> "; prterm r);

  fun prrules [] = print "[] \n"
    | prrules (r::rs) = (print "[ "; prrule r; prrules0 rs)
  and prrules0 [] = print " ]\n"
    | prrules0 (r::rs) = (print ",\n  "; prrule r; prrules0 rs);

  fun preq (t1, t2) = (prterm t1; print " = "; prterm t2);

  fun preqs [] = print "[] \n"
    | preqs (e::es) = (print "[ "; preq e; preqs0 es)
  and preqs0 [] = print " ]\n"
    | preqs0 (e::es) = (print ",\n  "; preq e; preqs0 es);

  fun subst ((s, 0), t2) = (print s; print " := "; prterm t2)
    | subst ((s, n), t2) 
      = (print (s^"_"^(Int.toString n)); print " := "; prterm t2);
  
  fun prsubst [] = print "[] \n"
    | prsubst (s::ss) =
      (print "[ "; subst s; prsubst0 ss)
  and prsubst0 [] = print " ]\n"
    | prsubst0 (s::ss) = (print ",\n  "; subst s; prsubst0 ss);

end;
