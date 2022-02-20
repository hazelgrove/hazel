let usage = "hazelc [-o output] <file>";
let inputs = ref([]);
let output = ref("");

let anon_fun = input => inputs := [input, ...inputs^];
let speclist = [("-o", Arg.Set_string(output), "Set output filename")];

let parse = () => Arg.parse(speclist, anon_fun, usage);

let () = {
  parse();
  let input = List.hd(inputs^);
  let input_file = open_in(input);

  let output =
    if (String.length(output^) == 0) {
      input ++ ".wasm";
    } else {
      output^;
    };

  let res = Compiler.compile_file(input_file, output);
  switch (res) {
  | Ok () => ()
  | Error(err) =>
    switch (err) {
    | Parse(err) => print_endline(err)
    | Elab => print_endline("elaboration error")
    | Grain => print_endline("grain error")
    }
  };
};
