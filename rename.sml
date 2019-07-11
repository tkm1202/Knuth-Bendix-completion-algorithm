(* $Id: rename.sml,v 2.0 2004/11/21 kusakari Exp $
**
**  ver 1.0: 1997/12/20 by KUSAKARI Keiichirou
**  ver 1.1: 1998/07/21 by KUSAKARI Keiichirou
**  ver 2.0: 2004/11/21 by KUSAKARI Keiichirou
**  ver 2.0b: 2008/04/01 by NISHIDA Naoki
*)

signature SigRename = sig

  include SigTRS
(*
  val vars : Term -> (string * int) list
*)
  val mkrename : (string * int) list -> int ->
                 (string * int) list ->
                 ((string * int) * (string * int)) list

  val rename : ((string * int) * (string * int)) list -> Term -> Term

  val uniquevar : Rule * Rule -> Rule * Rule

end;

structure Rename : SigRename = struct

  open Util;
  open TRS;

  fun vars (Node (VSym v, ts)) = union [v] (varslist ts)
    | vars (Node (_, ts)) = varslist ts
  and varslist [] = []
    | varslist (t::ts) = union (vars t) (varslist ts);

  fun findMax [] = 0
    | findMax ((v,n)::vs) =
      let
	  val m = findMax vs
      in
	  if n <= m then m else n
      end;

  fun mkrename _ _ [] = []
    | mkrename vs1 n (v2::vs2) =
      let
          fun mkvar (v,c) =
              if member (v,c) vs1
                  then if member (v,c+n) vs2
                           then mkvar (v,c+n+1)
                       else (v,c+n)
              else (v,c);
          val v3 = mkvar v2;
      in
          (v2,v3)::(mkrename (v3::vs1) n vs2)
      end;

  fun rename alist (Node (VSym v, ts)) =
      (case find v alist
         of SOME v2 => Node (VSym v2, map (rename alist) ts)
          | NONE => Node (VSym v, map (rename alist) ts))
    | rename alist (Node (a, ts)) =
      Node (a, map (rename alist) ts);

  fun uniquevar ((l1, r1), (l2, r2)) =
      let
	  val vars1 = vars l1;
	  val n = findMax vars1;
          val alist = mkrename vars1 (n+1) (vars l2);
      in
          ((l1, r1), (rename alist l2, rename alist r2))
      end;

end;
