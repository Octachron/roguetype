
(*
external current_environment: unit -> Obj.t = "caml_get_current_environment"
*)


let dir_quit () = Format.printf "Reach the altar of victory to escape@."
    
let _ =
  let open Topcommon in
  Topcommon.add_directive "quit" (Directive_none dir_quit)
    {
      section = "Roguetype directives";
      doc = "Exit the toplevel.";
    }

module Style = Misc.Style

let main () =
  Style.setup (Some Always);
  Clflags.noversion:=true;
  Clflags.noinit:=true;
  Format.printf "%a - escape from the type system @."
    (Format_doc.compat Style.inline_code) "Roguetype";
  let ppf = Format.err_formatter in
  let _program = "roguetype" in
  Topcommon.update_search_path_from_env ();
  if not (Toploop.prepare ppf ()) then raise (Compenv.Exit_with_status 2);
  Compmisc.init_path ();
  Topfind.load_deeply ["roguetype.lib"];
(*  Toploop.use_silently (Toploop.String "let exit () = exit ()"); *)
  Toploop.loop Format.std_formatter

let () =
  match main () with
  | exception Compenv.Exit_with_status n -> exit n
  | () -> exit 0
