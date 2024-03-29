(* $Id: reduce.sml,v 1.2 2000/09/18 kusakari Exp $
**
**  ver 1.0: 1997/12/15 by KUSAKARI Keiichirou
**  ver 1.1: 1998/07/21 by KUSAKARI Keiichirou
**  ver 1.2: 2000/09/18 by KUSAKARI Keiichirou
**  ver 1.3: 2007/05/15 by NISHIDA Naoki
2019/5/17
*)

signature SigReduce = sig

  include SigTRS

  val subst : Subst -> Term ->  Term

  val match : Subst -> Pattern -> Term -> bool * Subst
  val matchlist : Subst -> Pattern list -> Term list -> bool * Subst

  val rewrite : RuleSet -> Term -> bool * Term

  (* Par-Out one-step reduction *)
  val postep : RuleSet -> Term -> bool * Term
  val posteplist : bool * (Term list) -> RuleSet -> Pattern list -> bool * (Term list)

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
    | subst theta t =
      case t of
	  (Node (VSym v,_)) =>
	  let
	      val a = find v theta
	  in
	      if isSome a
	      then valOf a else t
	  end
       |  (Node (FSym f, ts)) =>  (Node (FSym f,(map (subst theta) ts)));
  
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
	  (Node (VSym v,_)) =>
	  let
	      val tt = find v sigma
	  in
	  if  (isSome tt) then
	      (if ((valOf tt) = t) then (true,sigma)
	       else (false,[]))
	  else (true,((v,t)::sigma))
	  end
       |  (Node (f, ls)) =>
	  (case t of  (Node (FSym g, ts)) =>
		      if f = (FSym g)
		      then  matchlist sigma ls ts
		      else (false,[])
		    | (Node (v,ts)) => (false,[]))	      
  and  matchlist sigma (l::ls) (t::ts) =
      let
	  val (bool,vs) = match sigma l t 
      in
	 if bool then matchlist vs ls ts else (false,[]) 
      end
     | matchlist sigma [] [] = (true,sigma)
     | matchlist sigma [] _ = (false,[])
     | matchlist sigma _ [] = (false,[]);
				
  fun rewrite ((l,r)::rs) t =
      let
	  val (bool,theta) = match [] l t
      in
	  if bool then (true,(subst theta r))
	  else rewrite rs t
      end
    | rewrite [] t = (false,t);
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
  fun linf rs t =
      case t of (Node(VSym v,_)) => t
	     |  (Node (f, ls)) =>
		let
		    val u = (Node (f, (map (linf rs) ls)))
		    fun linftop [] t = t					   
		      | linftop ((l,r)::rs) t =
			let val  (bool,theta) = match [] l t
			in
			    if bool then lisubst theta r else linftop rs t
			end
		    and lisubst theta t =
			case t of (Node(VSym v,_)) => subst theta t
				| (Node(f,ts))
				  => linftop rs (Node(f,(map (lisubst theta) ts))) 
		in linftop rs u		    
		end;
  
  exception Empty
		
  fun postep rs t =
      case t of (Node (FSym f,ts)) => 
      let
	  val (bool,tt) = rewrite rs t
      in
	  if bool then (true,tt)
	  else
	      let
		  val (bool,tss) = (posteplist (false,[]) rs ts)
	      in
		  (if bool then  (true,(Node (FSym f,tss)))
		   else (false,t))
	      end
      end
	      | _ => (false,t)
  and posteplist (bool,Tlist) rs (t::ts) =
      let
	  val (bool2,tt2) = postep rs t
      in
	  case bool of false =>
		       if bool2 then posteplist (true,(tt2::Tlist)) rs ts
		       else posteplist (false,(t::Tlist)) rs ts
		     | true =>
		       posteplist (true,(tt2::Tlist)) rs ts
      end
    | posteplist (bool,[]) rs [] = (bool,[])
    | posteplist (bool,Tlist) rs [] = (bool,(rev Tlist));
(*	     
  and posteplist (bool,Tlist) rs (t::ts) =
      let
	  val (bool2,tt2) = postep rs t
      in
	  case bool of false =>
		       if bool2 then posteplist (true,(Tlist @ [tt2])) rs ts
		       else posteplist (false,(Tlist @ [t])) rs ts
		     | true =>
		       posteplist (true,(Tlist @ [tt2])) rs ts
      end
    | posteplist (bool,[]) rs [] = (bool,[])
    | posteplist (bool,Tlist) rs [] = (bool,Tlist);
*)					  
  fun ponf rs t =
      case t of (Node(VSym v,_)) => t
	     |  (Node (f, ls)) =>
		let
		    val (bool,tt) = postep rs t
		in
		    if bool then ponf rs tt else t
		end;


  fun lostep rs t =
      case t of (Node (FSym f, ts)) => 
		let
		    val (bool,tt) = rewrite rs t
		in
		    if bool then (true,tt)
		    else (true,(Node (FSym f, (losteplist rs ts))))
			 handle Empty => (false,t)
		end
	      | _ => (false,t)
	    		 
  and losteplist rs (t::ts) = 
      let
	  val (bool2,tt2) = lostep rs t
      in
	  if bool2 then (tt2::ts) else (t::losteplist rs ts)
      end
    | losteplist rs [] = raise Empty;

  fun  lonf rs t =
       case t of (Node (FSym f,fs)) =>
		 let
		     val (bool,tt) = lostep rs t
		 in
		     if bool then lonf rs tt else t
		 end
	       | _ => t;
  
end;
