open Elpi
open String

type 'a result
    = Res of 'a
    | Err of string

type 'a parser = string -> ('a * string) result

let ( <|> ) (p : 'a parser) (q : 'a parser) : 'a parser =
    fun s -> match p s with
    | Res x -> Res x
    | Err e1 -> begin match q s with
        | Res x -> Res x
        | Err e2 -> Err (e1 ^ " or...\n" ^ e2)
        end

let ( let* ) (p : 'a parser) (k : 'a -> 'b parser) : 'b parser =
    fun s -> match p s with
    | Res (x, t) -> k x t
    | Err e -> Err e

let ( *> ) (p : 'a parser) (q : 'b parser) : 'b parser =
    let* _ = p in
    q

let pure (x : 'a) : 'a parser =
    fun s -> Res (x, s)

let err (s : string) : 'a parser =
    fun _ -> Err s

let str (i : string) : string parser =
    fun s ->
        if length s < length i then
            Err ("Expected '" ^ i ^ "', got EOF")
        else if starts_with ~prefix:i s then
            Res (i, sub s (length i) (length s))
        else
            Err ("Expected '" ^ i ^ "', got '" ^ sub s 0 (length i) ^ "'")

let is_space = function
    | ' ' | '\n' | '\t' | '\r' -> true
    | _ -> false

let is_alphanum = function
    'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false

let anychar : char parser = fun s ->
    if length s = 0 then
        Err "Expected any char, got EOF"
    else
        Res (s.[0], sub s 1 (length s))

let take_while f s =
    let rec go s = if f s.[0] then 1 + go (sub s 1 (length s)) else 0 in
    (sub s 0 (go s), sub s (go s) (length s))

let name : string parser = fun s -> Res (take_while is_alphanum s)

let eof : unit parser = fun s ->
    if length s = 0 then
        Res ((), "")
    else
        Err "Expected EOF"

let space : unit parser = fun s ->
    let (_, t) = take_while is_space s
    in Res ((), t)

let space1 : unit parser = fun s ->
    let (_, t) = take_while is_space s in
    if length s = length t then
        Err "Expected whitespace"
    else
        Res ((), t)

let rec many p = (let* x = p in let* xs = many p in pure (x::xs)) <|> pure []

let noconsume (p : 'a parser) : 'a parser = fun s -> match p s with
    | Res (x, _) -> Res (x, s)
    | Err e -> Err e

let rec findvar s = function
    | [] -> err ("Unbound variable '" ^ s ^ "'")
    | (x, e)::ss ->
        if x = s then
            pure e
        else
            findvar s ss

let rec prec0 env : string parser = 
    (str "if" *> pure "lam t\\ lam a\\ lam b\\ if t a b") <|>
    (str "pair" *> pure "lam a\\ lam b\\ gen a b") <|>
    (str "Bool" *> pure "two") <|>
    (str "Type" *> pure "typ") <|>
    (str "true" *> pure "tt") <|>
    (str "false" *> pure "ff") <|>
    (let* n = name in findvar n env) <|>
    (str "(" *> space *>
    let* e = prec3 env in
    space *> str ")" *> pure e)
and prec1 env : string parser =
    let* f = prec0 env in
    space1 *>
    let* xs = many (space1 *> prec0 env) in
    pure (List.fold_right (fun arg acc -> "(app " ^ acc ^ " " ^ arg ^ ")") xs f)
and prec2 env : string parser =
    (let* x = prec1 env in
    space *> str "==" *> space *>
    let* y = prec1 env in
    pure ("(eql " ^ x ^ " " ^ y ^ ")")) <|>
    (let* x = prec1 env in
    space *> str "!=" *> space *>
    let* y = prec1 env in
    pure ("(neq " ^ x ^ " " ^ y ^ ")"))
and lams env : string parser =
    (str "." *> space *> prec3 env) <|>
    (let* n = name in
    space1 *>
    let* ls = lams ((n, n)::env) in
    pure ("(lam " ^ n ^ "\\ " ^ ls ^ ")"))
and tbinds bi env : string parser =
    (str "." *> space *> prec3 env) <|>
    (str "(" *> space *>
    let* n = name in
    space *> str ":" *> space *>
    let* a = prec3 env in
    space *> str ")" *> space *>
    let* b = prec3 ((n, n)::env) in
    pure ("(" ^ bi ^ " " ^ a ^ " " ^ n ^ "\\ " ^ b ^ ")"))
and prec3 env : string parser =
    (str "lam" *> space1 *> lams env) <|>
    (str "for" *> space1 *> tbinds "uni" env) <|>
    (str "exi" *> space1 *> tbinds "exi" env)

let rec findnames : (string list) parser = fun s ->
    ((let* n = name in
    space *> str ":=" *>
    let* ns = findnames in
    pure (n::ns)) <|>
    (anychar *> findnames) <|>
    (eof *> pure [])) s

let rec decls env : string parser =
    (let* _ = name in
    space1 *> str ":=" *> space1 *>
    let* e = prec3 env in
    space1 *>
    let* r = decls env in
    pure ("(gen " ^ e ^ " " ^ r ^ ")"))

let parse_prog (s : string) (p : 'a parser) : 'a =
    match p s with
    | Res (_, "") -> failwith "Unconsumed input"
    | Res (x, _) -> x
    | Err e -> failwith e

let rec findmain = function
    | [] -> failwith "No main function"
    | "main"::_ -> fun s -> "(pr1 " ^ s ^ ")"
    | _::ss -> fun s -> findmain ss ("(pr2 " ^ s ^ ")")

let rec fixes r env = function
    | [] -> decls env
    | n::ns -> fixes ("(pr2 " ^ r ^ ")") ((n, "(pr1 " ^ r ^ ")")::env) ns

let toplevel : string parser =
    let* ns = noconsume findnames in
    let pr = findmain ns in
    let* r = fixes "toplevelrec" [] ns in
    pure (pr ("(fix toplevelrec\\ " ^ r ^ ")"))

let read_file filename = 
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true; do
            lines := input_line chan :: !lines
        done;
        failwith "unreachable"
    with End_of_file ->
        close_in chan;
        String.concat "\n" (List.rev !lines)
;;

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
    let input = read_file Sys.argv.(1) in
    let input_ast = parse_prog input toplevel in
    print_endline input_ast;
    let query_ast =
        try API.Parse.goal ~loc:(API.Ast.Loc.initial "(-parse-term)") ~elpi ~text:("synth " ^ input_ast ^ " E")
        with API.Parse.ParseError(loc, msg) -> Format.eprintf "%a@;%s\n" API.Ast.Loc.pp loc msg; exit 1 in
    let query = API.Compile.query prog query_ast in
    let exec = API.Compile.optimize query in
    API.Execute.loop ~delay_outside_fragment:false ~more ~pp:print_solution exec