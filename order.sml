(* $Id: order.sml,v 1.1 1998/07/21 kusakari Exp $
**
**  ver 1.0: 1997/08/01 by KUSAKARI Keiichirou
**  ver 1.1: 1998/07/21 by KUSAKARI Keiichirou
*)

signature SigOrder = sig

    include SigUtil
    include SigTRS

    type ''a Mset

    val mkgrter       : ('a -> 'a -> bool) -> 'a -> 'a -> bool
    val mkeq          : ('a -> 'a -> bool) -> 'a -> 'a -> bool

    val eq_symbol     : (string, int) Assoc -> string -> string -> bool
    val grtereq_symbol: (string, int) Assoc -> string -> string -> bool
    val grter_symbol  : (string, int) Assoc -> string -> string -> bool

    val grtereq_lex   : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
    val grtereq_lpo   : (string, int) Assoc -> Term -> Term -> bool
    val grter_lpo     : (string, int) Assoc -> Term -> Term -> bool

    val grtereq_mset  : (''a ->''a -> bool) -> ''a Mset -> ''a Mset -> bool
    val grtereq_rpo   : (string, int) Assoc -> Term -> Term -> bool
    val grter_rpo     : (string, int) Assoc -> Term -> Term -> bool

end;

structure Order : SigOrder = struct

    open Util;
    open TRS;

    type ''a Mset = ''a list;

    (* Let "gsim" be quasi-ordering擬順序.
     * "mkgrter" convert "gsim" to strict partial ordering狭義の半順序集合.
     * "mkeq" also convert it to equivalence relation.
     *)

    fun mkgrter gsim x y = gsim x y andalso not (gsim y x);

    fun mkeq gsim x y = gsim x y andalso gsim y x;

    (*
     -- Partial ordering半順序集合 on Symbols
     --      You must prepare list which maps symbols to integers.
     --      For example,
     --           ol = [("0",1),("S",1),("P",3),("M",4),("F",5)]
     --      Other symbols without orderlist such as "x",
     --      have only trivial relation: "x" >= "x".
     *)

    fun eq_symbol ol x y =
        if x = y then true
        else case (find x ol, find y ol)
               of (SOME n, SOME m) => n = m
                | _ => false;

    fun grtereq_symbol ol x y =
        if x = y then true
        else case (find x ol, find y ol)
               of (SOME n, SOME m) => n >= m
                | _ => false;

    fun grter_symbol ol x y =
        case (find x ol, find y ol)
          of (SOME n, SOME m) => n > m
           | _ => false;

    
    fun grtereq_lex gsim (x::xs) (y::ys) = 
	(mkgrter gsim x y) orelse ((mkeq gsim x y) andalso (grtereq_lex gsim xs ys))
      | grtereq_lex gsim [] [] = true
      | grtereq_lex gsim xs [] = true
      | grtereq_lex gsim [] ys = false;

    fun grtereq_lpo ol s t =
	case t of (Node (FSym g,ts)) =>
		  (case s of (Node (FSym f,ss)) =>
			     (forsome (fn si =>  grtereq_lpo ol si t) ss)
			     orelse
			     ((forall (fn ti => grter_lpo ol s ti) ts)
			      andalso
			      (((eq_symbol ol f g) andalso
			       (grtereq_lex (grtereq_lpo ol) ss ts))
			      orelse (grter_symbol ol f g)))
			   | _ => false
				      )
		  | (Node (VSym v,_)) => member v (varlist s)		   
    and grter_lpo ol s t = mkgrter (grtereq_lpo ol) s t;
    
    fun grtereq_mset gsim [] [] = true 
      | grtereq_mset gsim _ [] = true
      | grtereq_mset gsim [] _ = false
      | grtereq_mset gsim xs ys =
	let
	    fun find_eq gsim (m::ms) n =
		if mkeq gsim m n then ms else (m::(find_eq gsim ms n))
	      | find_eq gsim [] _ = []
	    fun reduce_eq  gsim ms (n::ns) =
		reduce_eq gsim (find_eq gsim ms n) ns
	      | reduce_eq gsim ms [] = ms
	    val re_xs = reduce_eq gsim xs ys
	    val re_ys = reduce_eq gsim ys xs
	in
	    (re_xs = [] andalso re_ys = [])
	    orelse
	    forall (fn yi => forsome (fn xi => not (gsim xi yi)) re_xs) re_ys
	end
      ;
    
    
    
    fun grtereq_rpo ol s t =
	case t of (Node (FSym g,ts)) =>
		  (case s of (Node (FSym f,ss)) =>
			     (forsome (fn si =>  grtereq_rpo ol si t) ss)
			     orelse   
			     (forall (fn ti => grter_rpo ol s ti) ts andalso
			      (grter_symbol ol f g))
			     orelse
			     ((eq_symbol ol f g) andalso
			      (grtereq_mset (grtereq_rpo ol) ss ts))
			   | _ => false
		  )
		| (Node (VSym v,_)) => member v (varlist s)
    and grter_rpo ol s t = mkgrter (grtereq_rpo ol) s t;
    
end;
