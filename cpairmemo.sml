open CP;
val l1 = Read.rdterm "F(G(x))";
val l2 = Read.rdterm "G(F(x))";
val r1 = Read.rdterm "FF(x)";
val r2 = Read.rdterm "GG(x)";
val rr1 = (l1,r1);
val rr2 = (l2,r2);
val q1 = CP.cpair rr1 rr2;

val l11 = Read.rdterm "F(x,G(x))";
val l22 = Read.rdterm "F(H(y),z)";
val r11 = Read.rdterm "GG(x)";
val r22 = Read.rdterm "HH(y,z)";
val rr11 = (l11,r11);
val rr22 = (l22,r22);
val q2 = CP.cpair rr11 rr22;

val l111 = Read.rdterm "F(F(x))";
val r111 = Read.rdterm "G(x)";
val l222 = Read.rdterm "F(F(x))";
val r222 = Read.rdterm "G(x)";
val rr111 = (l111,r111);
val rr222 = (l222,r222);
val q3 = CP.cpair rr111 rr222;

val l1111 = Read.rdterm "A";
val r1111 = Read.rdterm "0";
val l2222 = Read.rdterm "F(A)";
val r2222 = Read.rdterm "1";
val rr1111 = (l1111,r1111);
val rr2222 = (l2222,r2222);
val q4 = CP.cpair rr1111 rr2222;


val l11111 = Read.rdterm "F(x,1)";
val r11111 = Read.rdterm "G(x)";
val l22222 = Read.rdterm "F(0,y)";
val r22222 = Read.rdterm "H(y)";
val rr11111 = (l11111,r11111);
val rr22222 = (l22222,r22222);
val q5 = CP.cpair rr11111 rr22222;

val L1 = Read.rdterm "F(G(x),G(x))";
val R1 = Read.rdterm "H1(x)";
val L2 = Read.rdterm "G(F(x,x))";
val R2 = Read.rdterm "H2(x)";
val RR1 = (L1,R1);
val RR2 = (L2,R2);
val q6 = CP.cpair RR1 RR2;
	      

val L11 = Read.rdterm "H(G(F(A,x)),F(x,B))";
val R11 = Read.rdterm "I(x)";
val L22 = Read.rdterm "F(y,y)";
val R22 = Read.rdterm "y";
val RR11 = (L11,R11);
val RR22 = (L22,R22);
val q7 = CP.cpair RR11 RR22;

