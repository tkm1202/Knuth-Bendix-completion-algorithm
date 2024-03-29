(* kb.sml 2019/6/20
*)

signature SigKB = sig

  include SigTRS

  exception KB
		
  val orientation :
      (Term -> Term -> bool) -> RuleSet * EquationSet
      -> Rule * RuleSet * EquationSet

  val composition :
      Rule * RuleSet * EquationSet -> Rule * RuleSet * EquationSet
  val deduction :
      Rule * RuleSet * EquationSet -> Rule * RuleSet * EquationSet
  val collapse :
      Rule * RuleSet * EquationSet -> Rule * RuleSet * EquationSet
  val simplification :
      RuleSet * EquationSet -> RuleSet * EquationSet
  val deletion :
      RuleSet * EquationSet -> RuleSet * EquationSet
  val kbsub :
      (Term -> Term -> bool) -> RuleSet * EquationSet -> int -> RuleSet
  val kbstep :
      (Term -> Term -> bool) -> RuleSet * EquationSet
      -> RuleSet * EquationSet
  val kb : (Term -> Term -> bool) -> EquationSet -> RuleSet

end;

structure KB : SigKB = struct

  open Util;
  open TRS;
  open Reduce;
  open CP;
  open Order;
  exception KB;

 


  fun orientation grter (rs,es) =
      let
	  fun tsize2 (a,b) = tsize a + tsize b;
	  fun findmin grter (e1::es) nomatchlist =
	      let
		  fun varsize (a,b) = length (varlist a) + length (varlist b);
		  fun minsize e_min n (e1::es) eslist=
		      let
			  val m = tsize2 e1 
		      in
			  if n < m then minsize e_min n es (e1::eslist)
			  else if n = m then
			      (if varsize e_min < varsize e1
			       then minsize e1 m es (e_min::eslist)
			       else minsize e_min n es (e1::eslist))
			  else minsize e1 m es (e_min::eslist)
		      end
		    | minsize e_min _ [] eslist = (e_min,eslist);
		  val ((ei,ej),eslist) = minsize e1 (tsize2 e1) es [];
	      in
		  if grter ei ej then ((ei,ej),eslist @ nomatchlist)
		  else if grter ej ei then ((ej,ei),eslist @ nomatchlist)
		  else findmin grter eslist ((ei,ej)::nomatchlist)
	      end
	    | findmin _ [] _ = raise KB;
	  val (e_result,es_result) = findmin grter es [];
      in
	    (e_result,rs,es_result)
      end;
  
  
  fun composition (r,rs,es) =      
       (r,(map (fn (s,t) => (s,(linf (r::rs) t))) rs), es);
  
  fun deduction (r,rs,es) =
       (r,rs,(es @ (concat(map (fn ri => (cpair r ri)) (r::rs)))));
  
  fun collapse ((l,r),rs,es) =
      let
	  fun partial_match (l,(Node (f,fs))) =
	      let
		  val (bool,theta) = match [] l (Node (f,fs))
	      in
		  if bool then true
		  else
		      (* (if fs = [] then false
		else *)(forsome (fn fi => partial_match (l,fi)) fs)
	      end
      in
	   ((l,r),(filter (fn (si,ti) => not (partial_match (l,si))) rs),es)
      end;
  
  fun simplification (rs,es) =
       (rs,(map(fn (ei,ej) => (linf rs ei , linf rs ej)) es));
  
  fun deletion (rs,es) =
       (rs,(filter (fn (ei,ej) => not (ei = ej)) es)); 
      

  fun kbsub _ (rs, []) n = (print ("Success ("^(Int.toString n)^" steps).\n");
			    rs)
    | kbsub grter (rs, es) n =
      let
	  val (rs1, es1) = kbstep grter (rs, es)
	  val n1 = n+1;
      in
	  (print ("=== ("^(Int.toString n1)^" step).===\n");
	   print ("RS=\n"); PPrint.prrules rs1;
	   print ("E=\n");PPrint.prrules es1;
	   kbsub grter (rs1, es1) (n+1))
      end
  and kbstep grter (rs, es) =
      (deletion
        (simplification
          ((fn (r, rs, es) => (r::rs, es))
            (collapse
              (deduction
                (composition (orientation grter (rs, es))))))));

  fun kb grter es =
      kbsub grter ([], filter (fn (t1,t2) => t1 <> t2) es ) 0
      handle KB => (print "Failed:\n"; es);

end;

