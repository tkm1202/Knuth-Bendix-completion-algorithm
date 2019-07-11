(* $Id: contest-trs.sml,v 1.1 2001/06/03 kusakari Exp $
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

in

  fun fibtest 1 = PPrint.prterm (time (Reduce.linf rfib) t_fib15)
    | fibtest 2 = PPrint.prterm (time (Reduce.ponf rfib) t_fib15)
    | fibtest 3 = PPrint.prterm (time (Reduce.lonf rfib) t_fib15)
    | fibtest _ = ();

  fun facttest 1 = PPrint.prterm (time (Reduce.linf rfact) t_fact6)
    | facttest 2 = PPrint.prterm (time (Reduce.ponf rfact) t_fact6)
    | facttest 3 = PPrint.prterm (time (Reduce.lonf rfact) t_fact6)
    | facttest _ = ();

end
