(* $Id: contest-kb.sml,v 1.1 2001/06/03 kusakari Exp $
**
**  ver 1.0: 2000/06/06 by KUSAKARI Keiichirou
**  ver 1.1: 2001/06/03 by KUSAKARI Keiichirou
*)

local

  fun utime () =
      Time.toMilliseconds
          (#usr(Timer.checkCPUTimer(Timer.totalCPUTimer())));

  fun time f args =
      let
          val starttime = utime ();
          val result = f args;
      in
          print (Int32.toString (utime() - starttime));
          print "msec.\n\n";
          result
      end;

    fun kbtest_exec ol es =
        time (KB.kb (Order.grter_lpo ol)) (Read.rdeqs es);

in
  (* 群の公理 *)
  fun kbtest 1 =
      kbtest_exec
        [("I", 3), ("D", 2), ("E", 1)]
        ["D(E, x) = x",
         "D(I(x), x) = E",
         "D(D(x, y), z) = D(x, D(y, z))"]

  (* Fib(2,5) *)
    | kbtest 2 =
      kbtest_exec
      [("A", 8),("B", 7),("C", 3),("D", 9),("E",1),
       ("A1",5),("B1",6),("C1",10),("D1",4),("E1",2)] 
      ["A(B(x)) = C(x)",
       "B(C(x)) = D(x)",
       "C(D(x)) = E(x)",
       "D(E(x)) = A(x)",
       "E(A(x)) = B(x)",
       "A1(A(x)) = x",
       "B1(B(x)) = x", 
       "C1(C(x)) = x", 
       "D1(D(x)) = x", 
       "E1(E(x)) = x", 
       "A(A1(x)) = x", 
       "B(B1(x)) = x", 
       "C(C1(x)) = x", 
       "D(D1(x)) = x", 
       "E(E1(x)) = x"]

  (* ディラックの組み紐問題 *)
    | kbtest 3 =
      kbtest_exec
        [("I", 5), ("D", 4), ("E", 3), ("A", 2), ("B", 1)]
        ["D(E, x) = x",
         "D(I(x), x) = E",
         "D(x, D(y, z)) = D(D(x, y), z)",
         "D(D(A, B), A) = D(D(B, A), B)",        (* 組ひも関係式 *)
         "D(A, D(B, D(B, A))) = E",              (* 1+ *)  
         "D(I(A), D(I(B), D(I(B), I(A)))) = E",  (* 1- *)
         "D(B, D(A, D(A, B))) = E",              (* 3+ *)  
         "D(I(B), D(I(A), D(I(A), I(B)))) = E",  (* 3- *)
         "D(A, D(A, D(B, B))) = E",              (* 2L+ *) 
         "D(I(A), D(I(A), D(I(B), I(B)))) = E",  (* 2L- *)
         "D(B, D(B, D(A, A))) = E",              (* 2R+ *)
         "D(I(B), D(I(B), D(I(A), I(A)))) = E"]  (* 2R- *)

    | kbtest _ = [];

end
