(* $Id: unify.sml,v 1.1 1998/07/21 kusakari Exp $
**
**  ver 1.0: 1997/08/01 by KUSAKARI Keiichirou
**  ver 1.1: 1998/07/21 by KUSAKARI Keiichirou
*)

signature SigUnify = sig

  include SigTRS
  val usub : Subst -> Term -> Term -> bool * Subst
  val usublist : Subst -> Term list -> Term list -> bool * Subst

  val unify : Term -> Term -> bool * Subst

end;

structure Unify : SigUnify = struct

  open Util;
  open TRS;
  open Reduce;

	    
  fun usub alist t1 t2 =
      let
	  val (Node (f1,ts1)) = t1
	  val (Node (f2,ts2)) = t2
      in
	  case f1 of (VSym v1)
		     => if f1 = f2 then (true,alist)
			else if (not(f1 = f2) andalso (member v1 (varlist t2)))
			then (false,[])
			else (true,(valOf(append [(v1,t2)] (map (fn (x,y) => (x,(subst [(v1,t2)] y))) alist))))
		   | (FSym f) => (case f2 of (VSym v2)
					     => if (member v2 (varlist t1))
						then (false,[])
						else (true,(valOf(append [(v2,t1)] (map (fn (x,y) => (x,(subst [(v2,t1)] y))) alist))))
					   | (FSym ff) => if f1 = f2 then usublist alist ts1 ts2
							  else (false,[]))
      end		      
  and usublist alist [] [] = (true,alist)
    | usublist alist ts1 ts2 =
      if ((length ts1 > 0) andalso (length ts2 > 0)) then
	  let
	      val (s1::sn) = ts1
	      val (t1::tm) = ts2
	      val (b,alist1) = usub alist s1 t1
	  in
	      (if b then usublist alist1 (map (subst alist1) sn) (map (subst alist1) tm)
	       else (false,[]))	   
	  end
      else (false,[]);
  
  fun unify t1 t2 = usub [] t1 t2;

end;
