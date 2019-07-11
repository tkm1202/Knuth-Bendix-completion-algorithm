(* $Id: contest-trs.sml,v 1.1 2001/06/03 kusakari Exp $
**
**  ver 1.0: 2000/06/06 by KUSAKARI Keiichirou
**  ver 1.1: 2001/06/03 by KUSAKARI Keiichirou
**  ver 1.2: 2018/05/04 by HASHIMOTO Kenji
*)

local

  fun utime () =
      Time.toMilliseconds
        (#usr(Timer.checkCPUTimer(Timer.totalCPUTimer())));

  fun time_t f args =
      let
          val starttime = utime ();
          val result = f args;
          val tm = utime() - starttime;
      in
          print (IntInf.toString tm);
          print " msec.\n";
          IntInf.toInt tm
      end;

  fun time f args =
      let
          val starttime = utime ();
          val result = f args;
      in
          print (IntInf.toString (utime() - starttime));
          print " msec.\n\n";
          result
      end;

  fun prtermn t = (PPrint.prterm t; print "\n");

  fun mkterm 0 = TRS.Node (TRS.FSym "0", [])
    | mkterm n = TRS.Node (TRS.FSym "S", [mkterm (n-1)]);

  val rfib = Read.rdrules
      ["Add(0, y) -> y",
       "Add(S(x), y) -> S(Add(x, y))",
       "Fib(0) -> 0",
       "Fib(S(0)) -> S(0)",
       "Fib(S(S(x))) -> Add(Fib(x), Fib(S(x)))"]

  val rfact = Read.rdrules
      ["Add(0, y) -> y",
       "Add(S(x), y) -> S(Add(x, y))",
       "Mul(0, y) -> 0",
       "Mul(S(x), y) -> Add(Mul(x, y), y)",
       "Fact(0) -> S(0)",
       "Fact(S(x)) -> Mul(S(x), Fact(x))"];

  val t_fib15 = TRS.Node (TRS.FSym "Fib", [mkterm 15]);
  val t_fact6 = TRS.Node (TRS.FSym "Fact", [mkterm 6]);

  val t_fib18 = TRS.Node (TRS.FSym "Fib", [mkterm 18]);
  val t_fib20 = TRS.Node (TRS.FSym "Fib", [mkterm 20]);
  val t_fact7 = TRS.Node (TRS.FSym "Fact", [mkterm 7]);

  val fib18ans  = mkterm 2584; (* Fibonacci 18 = 2584 *)
  val fib20ans  = mkterm 6765; (* Fibonacci 20 = 6765 *)
  val fact6ans = mkterm 720; (* 6! = 720 *)
  val fact7ans = mkterm 5040; (* 7! = 5040 *)

in
  fun fibtest 1 = time_t (Reduce.linf rfib) t_fib20
    | fibtest 2 = time_t (Reduce.ponf rfib) t_fib18
    | fibtest 3 = time_t (Reduce.lonf rfib) t_fib18
    | fibtest _ = 0;

  fun fibcheck 1 = ((Reduce.linf rfib) t_fib20) = fib20ans
    | fibcheck 2 = ((Reduce.ponf rfib) t_fib18) = fib18ans
    | fibcheck 3 = ((Reduce.lonf rfib) t_fib18) = fib18ans
    | fibcheck _ = false;

  fun facttest 1 = time_t (Reduce.linf rfact) t_fact7
    | facttest 2 = time_t (Reduce.ponf rfact) t_fact6
    | facttest 3 = time_t (Reduce.lonf rfact) t_fact6
    | facttest _ = 0;

  fun factcheck 1 = ((Reduce.linf rfact) t_fact7) = fact7ans
    | factcheck 2 = ((Reduce.ponf rfact) t_fact6) = fact6ans
    | factcheck 3 = ((Reduce.lonf rfact) t_fact6) = fact6ans
    | factcheck _ = false;

(* old version *)
  fun fibtest0 1 = PPrint.prterm (time (Reduce.linf rfib) t_fib15)
    | fibtest0 2 = PPrint.prterm (time (Reduce.ponf rfib) t_fib15)
    | fibtest0 3 = PPrint.prterm (time (Reduce.lonf rfib) t_fib15)
    | fibtest0 _ = ();

  fun facttest0 1 = PPrint.prterm (time (Reduce.linf rfact) t_fact6)
    | facttest0 2 = PPrint.prterm (time (Reduce.ponf rfact) t_fact6)
    | facttest0 3 = PPrint.prterm (time (Reduce.lonf rfact) t_fact6)
    | facttest0 _ = ();

end
