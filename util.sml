(* $Id: util.sml,v 1.1 1998/07/21 kusakari Exp $
**
**  ver 1.0: 1997/08/01 by KUSAKARI Keiichirou
**  ver 1.1: 1998/07/21 by KUSAKARI Keiichirou
*)

signature SigUtil = sig

  type ('a,'b) Assoc

  val find   : ''a -> (''a, 'b) Assoc -> 'b option
  val append : (''a, ''b) Assoc -> (''a, ''b) Assoc
                 -> (''a, ''b) Assoc option

  val filter : ('a -> bool) -> 'a list -> 'a list
  val concat : 'a list list -> 'a list

  val andlist : bool list -> bool
  val orlist  : bool list -> bool
  val member  : ''a -> ''a list -> bool
  val forall  : ('a -> bool) -> 'a list -> bool
  val forsome : ('a -> bool) -> 'a list -> bool

  val union : ''a list -> ''a list -> ''a list
  val intersection : ''a list -> ''a list -> ''a list

end;

structure Util : SigUtil = struct

type  ('a,'b) Assoc = ('a * 'b) list;

fun find _ [] = NONE
  | find l (((ln,vn)::zs) :(''l,'v) Assoc) = if l=ln then SOME vn
					     else find l (zs :(''l,'v) Assoc);

fun append [] zs2 = SOME zs2
  | append ((ln1,vn1)::zs1) zs2 =
    case find ln1 zs2
     of NONE => (append zs1 ((ln1,vn1)::zs2))
      | SOME vn2 => if vn1 = vn2 then (append zs1 zs2)
		    else NONE;
fun filter p [] = []
  | filter p (x::xs) =
    if (p x) then (x::filter p xs)
    else filter p xs;

fun concat L = foldr (op @) [] L;

fun andlist [] = true
  | andlist (x::xs) = x andalso andlist xs; 

fun orlist [] = false
  | orlist (x::xs) = if x then true else orlist xs; 

fun member x [] = false
  | member x (y::ys) = if x = y then true else member x (ys);

fun forall p [] = true
  | forall p (x::xs) =
    if (p x) then forall p xs else false;

fun forsome p [] = false
  | forsome p (x::xs) =
    if (p x) then true else forsome p xs;

fun cons x xs = x::xs;

fun union [] ys = ys
  | union (x::xs) ys =
    if member x ys then union xs ys else cons x (union xs ys);

fun intersection [] ys = []
  | intersection (x::xs) ys  =
    if member x ys then cons x (intersection xs ys) else intersection xs ys;
end;
