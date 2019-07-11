(* $Id: load.sml,v 1.1 2005/01/16 kusakari Exp $
**
**  ver 1.0: 2004/11/21 by KUSAKARI Keiichirou
**  ver 1.1: 2005/01/16 by KUSAKARI Keiichirou
*)

local
    fun mkpath file dir = OS.Path.joinDirFile {dir=dir, file=file};
    fun ldlib f =
        let
            fun ld [] = print (f ^ ": description file not found\n")
              | ld (s::ss) =
                if OS.FileSys.access (s, [OS.FileSys.A_READ])
                    then CM.autoload' s
                else ld ss;
        in
            ld (map (mkpath f) (CM.set_path NONE))
        end;
(* Make the following code active for Windows *)
(*
    val _ = CM.autoloading (SOME true);
*)
in
    val _ = ldlib "ml-yacc-lib.cm";
end;

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
