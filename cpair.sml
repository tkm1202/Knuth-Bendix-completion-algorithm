(* $Id: cpair.sml,v 1.1 1998/07/21 kusakari Exp $
**
**  ver 1.0: 1997/12/20 by KUSAKARI Keiichirou
**  ver 1.1: 1998/07/21 by KUSAKARI Keiichirou
2019/5/29
*)

signature SigCP = sig

  include SigTRS

  val cp_component : Term -> Rule -> (Term * Subst) list
  val cp_componentlist : Term list -> Term list -> Rule -> (Term list * Subst) list

  val cp_assemble : Term -> (Term * Subst) list -> EquationSet

  val cpair : Rule -> Rule -> EquationSet

end;

structure CP : SigCP = struct

  open TRS;
  open Unify;
  open Reduce;
  
  fun cp_component l1 (l2,r2) =
      case l1 of (Node(FSym f , ts)) =>
		 let
		     val (bool,theta) = unify l1 l2
		     val result_componentlist = (cp_componentlist [] ts (l2,r2))
		 in
		     if bool then ((r2,theta)::(map (fn (x,y) => (Node(FSym f, x) , y)) result_componentlist))
		     else (map (fn (x,y) => (Node(FSym f, x) , y)) result_componentlist)
		 end
	       | _ => []
  and cp_componentlist t_head (t::ts) r =
      (case t of (Node (FSym f,fs)) =>
		 ((map (fn (x,y) => ((t_head @ [x] @ ts),y)) (cp_component t r)) @ (cp_componentlist (t_head @ [t]) ts r))
	       | _ => cp_componentlist (t_head @ [t]) ts r)
	  
    | cp_componentlist t_head [] (l,r) = [];  
		
  fun cp_assemble r ((t,theta)::ts) =
      let
	  val (a,b) = ((subst theta t),(subst theta r)) (*危険対<r,r>を除去*)
      in
	  (*if a = b then cp_assemble r ts
	  else*) ((a,b)::(cp_assemble r ts))
      end
    | cp_assemble r [] = [];
  
  fun cpair r1 r2 =
      let
          val ((l3, r3), (l4, r4)) = Rename.uniquevar (r1, r2)
      in
          cp_assemble r3 (cp_component l3 (l4, r4))
          @ cp_assemble r4 (cp_component l4 (l3, r3))
      end;

end;
