open Cmdliner

(* Read from stdin or file depending on argument *)
let read_input path =
  match path with
  | "-" -> (
      let buf = Buffer.create 1024 in
      try
        while true do
          let line = input_line stdin in
          Buffer.add_string buf line;
          Buffer.add_char buf '\n'
        done;
        assert false (* unreachable *)
      with
      | End_of_file -> Buffer.contents buf
      | _ -> failwith "Unexpected error while reading input")
  | file ->
      let ic = open_in file in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic;
      content

(* Placeholder implementations for each command *)
let run_hazel path =
  let program = read_input path in
  let parsed = Parse.parse_program program in
  let evaluated = Haz3lcore.DHExp.strip_casts (Run.evaluate parsed) in

  print_endline (Print.print evaluated)

let format_hazel path =
  let program = read_input path in
  let parsed = Parse.parse_program program in
  print_endline (Print.print parsed)

let analyze_hazel path =
  let _program = read_input path in
  (* Printf.printf "Analyzing Hazel program:\n%s\n%!" program; *)
  (* TODO Use statics to output marks *)
  ()

(* Common arg: path or "-" *)
let input_arg =
  let doc = "Path to Hazel source file, or '-' to read from stdin." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INPUT" ~doc)

(* Subcommand terms using Cmd.info *)
let run_cmd =
  let doc = "Run a Hazel program." in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(const run_hazel $ input_arg)

let format_cmd =
  let doc =
    "Reconstructs Hazel code from its abstract syntax tree (AST), producing\n\
    \      concrete syntax without preserving original whitespace or comments.\n\
    \      This process uses a recovering parser and automatically inserts holes\n\
    \      where necessary to ensure syntactic correctness."
  in
  let info = Cmd.info "format" ~doc in
  Cmd.v info Term.(const format_hazel $ input_arg)

  let size_arg =
  let doc = "Size of the generated test program." in
  Arg.(value & opt int 35 & info [ "s"; "size" ] ~docv:"SIZE" ~doc)

let count_arg =
  let doc = "Number of test programs to generate." in
  Arg.(value & opt int 1 & info [ "c"; "count" ] ~docv:"COUNT" ~doc)

let generate_test_program size count =
  let arb = QCheck_Util.arb_exp ~minimal_idents:true size in
  let gen = arb.gen in
  for _ = 1 to count do
    let program = QCheck.Gen.generate1 gen in
    print_endline (Print.print program)
  done

let generate_test_program_cmd =
  let doc = "Generate test programs." in
  let info = Cmd.info "generate-test" ~doc in
  Cmd.v info Term.(const generate_test_program $ size_arg $ count_arg)

let _analyze_cmd =
  let doc = "Perform static analysis on Hazel code." in
  let info = Cmd.info "analyze" ~doc in
  Cmd.v info Term.(const analyze_hazel $ input_arg)

(* Default to help if no subcommand is given *)
let default_cmd =
  let doc = "CLI tool for running and analyzing Hazel programs." in
  let info = Cmd.info "hazel" ~version:"0.1.0" ~doc in
  Cmd.group info [ run_cmd; format_cmd; generate_test_program_cmd ]

let () = exit (Cmd.eval default_cmd)
