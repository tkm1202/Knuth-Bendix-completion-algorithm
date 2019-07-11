(*
val order = [("W",3),("S",2),("B",1)];
val rs11 = [];
val es11 = [((Read.rdterm "W(x)"),(Read.rdterm "S(W(x))")),((Read.rdterm "W(B(x))"),(Read.rdterm "S(x)"))];

val r12 = ((Read.rdterm "S(W(x))"), (Read.rdterm "W(x)"));
val rs12 = [];
val es12 = [((Read.rdterm "W(B(x))"),(Read.rdterm "S(x)"))];

val r22 = ((Read.rdterm "W(B(x))"), (Read.rdterm "S(x)"));
val rs22 = [((Read.rdterm "S(W(x))"), (Read.rdterm "W(x)"))];
val es22 = [];

val r32 = ((Read.rdterm "S(S(x))"), (Read.rdterm "S(x)"));
val rs32 = [((Read.rdterm "W(B(x))"), (Read.rdterm "S(x)")),((Read.rdterm "S(W(x))"), (Read.rdterm "W(x)"))];
val es32 = [];

val ex1 = KB.kb (Order.grter_lpo order) es11;

val orderr=[("I", 3), ("D", 2), ("E", 1)];
val RS = Read.rdrules ["D(y, z2) -> z2"];
val E = Read.rdeqs ["E -> x",
  "E -> x",
  "z2 -> E" ];

*)
val o1 = [("I", 3), ("D", 2), ("E", 1)];
	      
val es1 = Read.rdeqs ["D(E, x) = x",
         "D(I(x), x) = E",
         "D(D(x, y), z) = D(x, D(y, z))"];

val o2 = [("A", 8),("B", 7),("C", 3),("D", 9),("E",1),
			 ("A1",5),("B1",6),("C1",10),("D1",4),("E1",2)];

val es2 = Read.rdeqs  ["A(B(x)) = C(x)",
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
			"E(E1(x)) = x"];

val o3 = [("I", 5), ("D", 4), ("E", 3), ("A", 2), ("B", 1)];
val es3 = Read.rdeqs ["D(E, x) = x",
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
           "D(I(B), D(I(B), D(I(A), I(A)))) = E"] ; (* 2R- *)
