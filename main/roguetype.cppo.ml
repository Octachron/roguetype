let dir_quit () = Format.printf "Reach the altar of victory to escape@."

let _ =
  let open Topcommon in
  Topcommon.add_directive "quit" (Directive_none dir_quit)
    {
      section = "Roguetype directives";
      doc = "Exit the toplevel.";
    }


let inline_code =
#if OCAML_VERSION < (5, 2, 0)
   Format.pp_print_string
#elif OCAML_VERSION < (5, 3, 0)
    Misc.Style.inline_code
#else
    Format_doc.compat Misc.Style.inline_code
#endif

let setup () =
#if OCAML_VERSION < (5, 2, 0)
   ()
#else
  Misc.Style.setup (Some Always)
#endif

#if OCAML_VERSION < (5,3,0)
module Topcommon = struct
  include Topcommon

let update_search_path_from_env () =
  let extra_paths =
    let env = Sys.getenv_opt "OCAMLTOP_INCLUDE_PATH" in
    Option.fold ~none:[] ~some:Misc.split_path_contents env
  in
  Clflags.include_dirs := List.rev_append extra_paths !Clflags.include_dirs

let set_paths ?dir:_ () = set_paths ()

end
#endif

#if OCAML_VERSION < (5,3,0)
module Toploop = struct
  include Toploop
let preload_objects = ref []
let prepare ppf ?input () =
  let dir =
    Option.map (fun inp -> Filename.dirname (filename_of_input inp)) input in
  Topcommon.set_paths ?dir ();
  begin try
    initialize_toplevel_env ()
  with Env.Error _ | Typetexp.Error _ as exn ->
    Location.report_exception ppf exn; raise (Compenv.Exit_with_status 2)
  end;
  try
    let res =
      let objects =
        List.rev (!preload_objects @ !Compenv.first_objfiles)
      in
      List.for_all (Topeval.load_file false ppf) objects
    in
    Topcommon.run_hooks Topcommon.Startup;
    res
  with x ->
    try Location.report_exception ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false
end
#endif

let main () =
  setup ();
  Clflags.noversion:=true;
  Clflags.noinit:=true;
  Clflags.real_paths:= false;
  Format.printf
    "@[<v>%a - escape from the type system @,@,\
     The game levels are defined in the %a module.@,\
     The rules of the game are defined in the %a module.@,@,\
     Now go forth and prove that the victory type is inhabited@]@."
    inline_code "Roguetype"
    inline_code "Game"
    inline_code "Rules";

  let ppf = Format.err_formatter in
  let _program = "roguetype" in
  Topcommon.update_search_path_from_env ();
  if not (Toploop.prepare ppf ()) then raise (Compenv.Exit_with_status 2);
  Compmisc.init_path ();
  Topfind.load_deeply ["roguetype.lib"];
  let _ = Toploop.load_file Format.err_formatter "roguetype_lib.cma" in
  let _ = Toploop.use_silently Format.err_formatter
    (Toploop.String "let exit () = ();;")
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
