(*** KBゼミ「SML/NJ入門」演習課題 ***)

(*--------------------------------------------------------------*)
(*** 問1 ***)
(* my_not, my_and, my_or *)


(*--------------------------------------------------------------*)
(*** 問2 ***)
(*   (i) *) 
(*  (ii) *) 
(* (iii) *) 
(*  (iv) *) 
(*   (v) *) 

(*--------------------------------------------------------------*)
(*** 問3 ***)
(* (i) length *)

(* (ii) sum *)

(* (iii) member *)

(* (iv) rev_len2 *)


(*--------------------------------------------------------------*)
(*** 問4 ***)
(* insertの説明
 
**ここまで** *)

(*--------------------------------------------------------------*)
(*** 問5 ***)
(* evenlist, oddlist *)


(*--------------------------------------------------------------*)
(*** 問6 ***)
(* Set.intersection *)
signature SigSet =
sig
    type 'a Set
    val union : ''a Set -> ''a Set -> ''a Set
end;

structure Set : SigSet =
struct
    type 'a Set = 'a list;

    fun cons x xs = x::xs;

    fun union [] ys = ys
      | union (x::xs) ys =
	if member x ys then union xs ys else cons x (union xs ys);

end;

(*--------------------------------------------------------------*)
(*** 問7 ***)
(* (i) square *)

(* (ii) even *)

(* (iii) zip *)

(* (iv) concat *)

(* (v) andlist *)

(* (vi) orlist *)

(* (vii) filter *)

(* (viii) forall *)

(* (ix) forsome *)


(*--------------------------------------------------------------*)
(*** 問8 ***)
(* squaresum *)


(*--------------------------------------------------------------*)
(*** 問9 ***)
(* (i) mklist *)

(* (ii) divides *)

(* (iii) isPrime *)

(* (iv) isPrime2 *)


(*--------------------------------------------------------------*)
(*** 問10 ***)
(* (i) find *)

(* (ii) append *)


(*--------------------------------------------------------------*)
(*** 問11 ***)
(* (i) Mul *)

(* (ii) Sub *)

(* (iii) Max *)

(* (iv) Min *)


(*--------------------------------------------------------------*)
