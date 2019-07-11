(* $Id: load.sml,v 1.1 2005/01/16 kusakari Exp $
**
**  ver 1.0: 2004/11/21 by KUSAKARI Keiichirou
**  ver 1.1: 2005/01/16 by KUSAKARI Keiichirou
**  ver 2.0: 2014/05/02 by NISHIDA Naoki
*)
Control.Print.printDepth := 30;
(* load the folloiwng to check the existence of warnings *)
use "util.sml";
use "trs_sig.sml";

(* This is for Version 110.72 *)
CM.make "preset.cm";
(*
use "trs_parse.grm.sig";
use "trs_parse.grm.sml";
use "trs_lex.lex.sml";
use "read.sml";
*)
use "pprint.sml";

use "reduce.sml";
use "reducememo.sml";

use "rename.sml";
use "unify.sml";
use "cpair.sml";
(*
use "cpairmemo.sml";
*)
use "order.sml";
(*
use "ordermemo.sml";
*)
use "kb.sml";
use "KBmemo.sml";
(*use "contest-trs.sml";*)
use "contest-kb.sml";

use "contest-trs-new.sml";
