
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
  Format.printf
    "@[<v>%a - escape from the type system @,\
     The game levels are defined in the %a module.@,\
     The rules of the game are defined in the %a module.@,\
     Now go forth and prove that the victory type is inhabited@]@."
    (Format_doc.compat Style.inline_code) "Roguetype"
    (Format_doc.compat Style.inline_code) "Game"
    (Format_doc.compat Style.inline_code) "Rules";

  let ppf = Format.err_formatter in
  let _program = "roguetype" in
  Topcommon.update_search_path_from_env ();
  if not (Toploop.prepare ppf ()) then raise (Compenv.Exit_with_status 2);
  Compmisc.init_path ();
  Topfind.load_deeply ["roguetype.lib"];
  let _ = Toploop.use_silently Format.err_formatter
    (Toploop.String "let exit () = exit ();;")
  in
  let _ = Toploop.use_silently
    Format.err_formatter
    (Toploop.String "open Roguetype_lib;;")
  in
  Toploop.loop Format.std_formatter

let () =
  match main () with
  | exception Compenv.Exit_with_status n -> exit n
  | () -> exit 0
