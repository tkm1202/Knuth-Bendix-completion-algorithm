(* $Id: load.sml,v 1.1 2005/01/16 kusakari Exp $
**
**  ver 1.0: 2004/11/21 by KUSAKARI Keiichirou
**  ver 1.1: 2005/01/16 by KUSAKARI Keiichirou
**  ver 2.0: 2011/04/19 by NISHIDA Naoki
*)

(* This is for Version 110.72 *)
CM.make "preset.cm";

use "util.sml";

use "trs_sig.sml";
use "trs_parse.grm.sig";
use "trs_parse.grm.sml";
use "trs_lex.lex.sml";
use "read.sml";
use "pprint.sml";

use "reduce.sml";
use "rename.sml";
use "unify.sml";
use "cpair.sml";
use "order.sml";
use "kb.sml";

use "contest-trs.sml";
use "contest-kb.sml";
