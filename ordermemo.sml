open Order;
val order = [("F",4),("G",3),("H",3),("A",2),("B",1)];

val s1 = Read.rdterm "F(A,A)";
val t1 = Read.rdterm "F(A,B)";
val lq1 = (grtereq_lpo order s1 t1,grter_lpo order s1 t1);
val rq1 = (grtereq_rpo order s1 t1,grter_rpo order s1 t1);

val s2 = Read.rdterm "F(A,B)";
val t2 = Read.rdterm "F(B,A)";
val lq2 = (grtereq_lpo order s2 t2,grter_lpo order s2 t2);
val rq2 = (grtereq_rpo order s2 t2,grter_rpo order s2 t2);

val s3 = Read.rdterm "G(F(x))";
val t3 = Read.rdterm "F(x)";
val lq3 = (grtereq_lpo order s3 t3,grter_lpo order s3 t3);
val rq3 = (grtereq_rpo order s3 t3,grter_rpo order s3 t3);

val s4 = Read.rdterm "F(x)";
val t4 = Read.rdterm "G(F(x))";
val lq4 = (grtereq_lpo order s4 t4,grter_lpo order s4 t4);
val rq4 = (grtereq_rpo order s4 t4,grter_rpo order s4 t4);

val s5 = Read.rdterm "G(x)";
val t5 = Read.rdterm "H(x,x)";
val lq5 = (grtereq_lpo order s5 t5,grter_lpo order s5 t5);
val rq5 = (grtereq_rpo order s5 t5,grter_rpo order s5 t5);

val s6 = Read.rdterm "G(x,x)";
val t6 = Read.rdterm "H(x)";
val lq6 = (grtereq_lpo order s6 t6,grter_lpo order s6 t6);
val rq6 = (grtereq_rpo order s6 t6,grter_rpo order s6 t6);

val s7 = Read.rdterm "F(B,A,A)";
val t7 = Read.rdterm "F(A,B,B)";
val lq7 = (grtereq_lpo order s7 t7,grter_lpo order s7 t7);
val rq7 = (grtereq_rpo order s7 t7,grter_rpo order s7 t7);

val s8 = Read.rdterm "F(B,F(A,A))";
val t8 = Read.rdterm "F(A,A)";
val lq8 = (grtereq_lpo order s8 t8,grter_lpo order s8 t8);
val rq8 = (grtereq_rpo order s8 t8,grter_rpo order s8 t8);
