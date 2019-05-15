(* $Id: reduce.sml,v 1.2 2000/09/18 kusakari Exp $
**
**  ver 1.0: 1997/12/15 by KUSAKARI Keiichirou
**  ver 1.1: 1998/07/21 by KUSAKARI Keiichirou
**  ver 1.2: 2000/09/18 by KUSAKARI Keiichirou
**  ver 1.3: 2007/05/15 by NISHIDA Naoki
*)

signature SigReduce = sig

  include SigTRS

  val subst : Subst -> Term ->  Term

  val match : Pattern -> Term -> bool * Subst
  val matchlist : Subst -> Pattern list -> Term list -> bool * Subst

  val rewrite : RuleSet -> Term -> bool * Term
(*
  (* Par-Out one-step reduction *)
  val postep : RuleSet -> Term -> bool * Term

  (* Get normal form by Par-Out strategy *)
  val ponf : RuleSet -> Term -> Term

  (* Left-Out one-step reduction *)
  val lostep : RuleSet -> Term -> bool * Term

  (* Get normal form by Left-Out strategy *)
  val lonf : RuleSet -> Term -> Term
*)
  (* Get normal form by Left-In strategy *)
  val linf : RuleSet -> Term -> Term

end;

structure Reduce : SigReduce =  struct

  open Util;
  open TRS;

  fun subst [] t = t   
    | subst a t =
      case t of
	  (Node (FSym f, [])) => t
       |  (Node (FSym f, ts)) =>  (Node (FSym f,(map (fn x => (subst a x)) ts)))
       |  (Node (VSym (v,n),_)) => if isSome (find (v,n) a)
				   then valOf (find (v,n) a) else t;

 
 fun match l t =
      case l of  
	  (Node (VSym (v,n),ls)) =>  (true,[((v,n),t)])
       |  (Node (FSym f, ls)) =>
	  (case t of  (Node (FSym g, ts)) =>
		      (if (f=g andalso (length ls = length ts))
		       then  matchlist [] ls ts
		       else (false,[]))
		    | (Node (VSym (v,n),ts)) => (false,[]))	      
  and matchlist sigma [] [] = (true,sigma)
    | matchlist sigma [] _ = (false,[])
    | matchlist sigma _ [] = (false,[])
    | matchlist sigma (l::ls) (t::ts) =
      let
	  val (bool,vs) = match l t 
      in
	  (case vs of (((v2,n2),t2)::vss) =>
		      if bool andalso (isSome (find (v2,n2) sigma))
		      then (if ((valOf (find (v2,n2) sigma)) = t2 )
			    then matchlist sigma ls ts
			    else (false,[]))
		      else  matchlist ([((v2,n2),t2)]@sigma) ls ts
		   | nil => if bool then matchlist sigma ls ts else (false,[]))  
      end;

  fun rewrite [] t = (false,t)
    | rewrite ((l,r)::rs) t =
      let
	  val (bool,thita)=match l t
      in
	  if bool then (true,(subst thita r))
	  else rewrite rs t
      end;

  fun linf [] t = t
    | linf rs t =
      case t of (Node(VSym v,_)) => t
	      |  (Node (FSym f, ls)) =>
		 let
		     val u = (Node (FSym f, (map (linf rs) ls)))
		     val (bool,tt) = rewrite rs u
		 in
		     if bool then linf rs tt  else u
		 end;
(*
  fun postep [] t = (false,[])
    | postep rs t =
      let
	  val (bool,tt) = rewrite rs t
      in
	  if bool then (true,tt) else posteplist [] rs t
      end
  and posteplist isT _ [] = 
    | posteplist isT (r::rs) t =
      case t of (Node(VSym v,_)) => posteplist sigma rs
	      | (Node (FSym f, ls)) =>
		let
		    val (bool2,tt2) = postep (r::rs) ls
		in
		    if bool2 then posteplist tt2
		end
*)

  fun lostep [] t = (false,[])
    | lostep rs t =
      case t of (Node(VSym v,_)) => t
	     |  (Node (FSym f, ts)) => 
      let
	  val (bool,tt) = rewrite rs t
      in
	  if bool then (true,tt) else losteplist rs ts
      end
  and losteplist rs [] = 
    | losteplist rs (t::ts) =
      let
	  val (bool2,tt2) = lostep rs t
      in
	  if bool then (true,tt2) else losteplist rs ts
      end;
      
end;
