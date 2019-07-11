open Reduce;
val l1 = Read.rdterm "Add(0,y)";
val r1 = Read.rdterm "y";
val l2 = Read.rdterm "Add(S(x),y)";
val r2 = Read.rdterm "S(Add(x,y))";
val l3 = Read.rdterm "Mul(0,y)";
val r3 = Read.rdterm "0";
val l4 = Read.rdterm "Mul(S(x),y)";
val r4 = Read.rdterm "Add(Mul(x,y),y)";

val rs = [(l1,r1),(l2,r2),(l3,r3),(l4,r4)];

val t = Read.rdterm "Mul(S(0),Add(0,S(0)))";
