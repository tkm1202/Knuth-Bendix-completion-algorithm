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

  val match : Subst -> Pattern -> Term -> bool * Subst
  val matchlist : Subst -> Pattern list -> Term list -> bool * Subst
  val redex : RuleSet -> Symbol list
  val rewrite : Symbol list -> RuleSet -> Term -> bool * Term

  (* Par-Out one-step reduction *)
  val postep : RuleSet -> Term -> bool * Term
  val posteplist : Pattern list -> RuleSet -> Pattern list -> Pattern list


  (* Get normal form by Par-Out strategy *)
  val ponf : RuleSet -> Term -> Term

  (* Left-Out one-step reduction *)
  val lostep : RuleSet -> Term -> bool * Term
  val losteplist : RuleSet -> Pattern list -> Pattern list

  (* Get normal form by Left-Out strategy *)
  val lonf : RuleSet -> Term -> Term

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
       |  (Node (VSym v,_)) => if isSome (find v a)
				   then valOf (find v a) else t;

  (*match 改定前
  val  match : Pattern -> Term -> bool * Subst 
*)
  (*
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
  *)
  
  (*match 改定後
    val  match : Subst -> Pattern -> Term -> bool * Subst 
*)
   fun match sigma l t =
      case l of  
	  (Node (VSym v,ls)) =>
	  if  (isSome (find v sigma)) then
	      (if ((valOf (find v sigma)) = t) then (true,sigma)
	       else (false,[]))
	  else (true,((v,t)::sigma))
       |  (Node (f, ls)) =>
	  (case t of  (Node (FSym g, ts)) =>
		      if f = (FSym g)
		      then  matchlist [] ls ts
		      else (false,[])
		    | (Node (v,ts)) => (false,[]))	      
  and matchlist sigma [] [] = (true,sigma)
    | matchlist sigma [] _ = (false,[])
    | matchlist sigma _ [] = (false,[])
    | matchlist sigma (l::ls) (t::ts) =
      let
	  val (bool,vs) = match sigma l t 
      in
	 if bool then matchlist vs ls ts else (false,[]) 
      end;

   fun redex [] = []
     | redex ((l,r)::rs) =
       case l of  (Node (FSym f, ts)) => union [FSym f] (redex rs)
		| _ => []
			   
  fun rewrite redexes [] t = (false,t)
    | rewrite redexes rs (Node (VSym v,vs)) = (false,(Node (VSym v,vs))) 
    | rewrite redexes ((l,r)::rs) (Node (f,ts)) =
      if member f redexes
      then
	 ( let
	      val (bool,theta) = match [] l (Node (f,ts))
	  in
	      if bool then (true,(subst theta r))
	      else rewrite redexes rs (Node (f,ts))
	  end)
      else (false,(Node (f,ts)));
				   (*
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
*)
   fun linf [] t = t
    | linf ((l,r)::rs) t =
      case t of (Node(VSym v,_)) => t
	      |  (Node (f, ls)) =>
		 let
		     val u = (Node (f, (map (linf ((l,r)::rs)) ls)))
		     fun linftop [] t = t					   
		       | linftop ((l,r)::rs) t =
			 let val  (bool,theta) = match [] l t
			 in
			     if bool then lisubst theta r else linftop rs t
			 end
		     and lisubst theta t =
			 case t of (Node(VSym v,_)) => subst theta t
				 | (Node(f,ts))
				   => linftop ((l,r)::rs) (Node(f,(map (lisubst theta) ts))) 
		 in linftop ((l,r)::rs) u		    
		 end;
   
  exception Empty
		
  fun postep [] t = (false,t)
    | postep rs t =
      let val re = redex rs in
      case t of (Node(VSym v,_)) => (false,t)
	     |  (Node (f, ts)) => 
      let
	  val (bool,tt) = rewrite (redex rs) rs t
      in
	  if bool then (true,tt)
	  else (true,(Node (f,(posteplist [] rs ts))))
	       handle Empty => (false,t)
      end
      end
  and posteplist [] rs [] = raise Empty
    | posteplist Tlist rs [] = Tlist
    | posteplist Tlist rs (t::ts) =
      let
	  val (bool2,tt2) = postep rs t
      in
	  if bool2 then posteplist (tt2::Tlist) rs ts
	  else posteplist Tlist rs ts
      end;
	  
  fun ponf [] t = t
    | ponf rs t =
      case t of (Node(VSym v,_)) => t
	     |  (Node (f, ls)) =>
		let
		    val (bool,tt) = postep rs t
		in
		    if bool then ponf rs tt else t
		end;


  fun lostep [] t = (false,t)
    | lostep rs t =
      let val re = redex rs in
      case t of (Node(VSym v,_)) => (false,t)
	     |  (Node (f, ts)) => 
      let
	  val (bool,tt) = rewrite re rs t
      in
	  if bool then (true,tt)
	  else (true,(Node (f, (losteplist rs ts))))
	       handle Empty => (false,t)
      end
      end
	
  and losteplist rs [] = raise Empty
    | losteplist rs (t::ts) = 
      let
	  val (bool2,tt2) = lostep rs t
      in
	  if bool2 then (tt2::ts) else (t::losteplist rs ts)
      end;

fun lonf [] t = t
  | lonf rs t =
     case t of (Node(VSym v,_)) => t
	      |  (Node (f, ls)) =>
		 let
		     val (bool,tt) = lostep rs t
		 in
		     if bool then lonf rs tt else t
		 end;

end;
