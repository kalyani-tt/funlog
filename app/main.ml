open Elpi

let more () =
    prerr_endline "\nmore? (y/n)";
    read_line() <> "n"
;;

let print_solution time = function
| API.Execute.NoMoreSteps ->
   Format.eprintf "interrupted (no more steps)\n%!"
| API.Execute.Failure -> Format.eprintf "failure\n%!"
| API.Execute.Success { API.Data.assignments; constraints; state; pp_ctx; _ } ->
    Format.eprintf "success:@\n%!" ;
    API.Data.StrMap.iter (fun name v ->
        Format.eprintf "  @[<hov 1>%s = %a@]%!" name
            (API.Pp.term pp_ctx) v) assignments
;;

let _ =
    let elpi = API.Setup.init ~builtins:[Builtin.std_builtins] ~file_resolver:(API.Parse.std_resolver ~paths:["src"] ()) () in
    let prog_ast =
        try API.Parse.program elpi ["src/synth.elpi"; "src/parse.elpi"; "src/main.elpi"]
        with API.Parse.ParseError(loc, msg) -> Format.eprintf "%a@;%s\n" API.Ast.Loc.pp loc msg; exit 1 in
    let prog = API.Compile.program ~elpi [prog_ast] in
    let query_ast =
        try API.Parse.goal ~loc:(API.Ast.Loc.initial "(-parse-term)") ~elpi ~text:("runfile \"" ^ Sys.argv.(1) ^ "\" E")
        with API.Parse.ParseError(loc, msg) -> Format.eprintf "%a@;%s\n" API.Ast.Loc.pp loc msg; exit 1 in
    let query = API.Compile.query prog query_ast in
    let exec = API.Compile.optimize query in
    API.Execute.loop ~delay_outside_fragment:true ~more ~pp:print_solution exec