open Hazel_menhir;
open Haz3lcore.DHExp;

let test_file = "/home/green726/coding/hazel/src/menhir-test/test.hazel";

let read_whole_file = (filename): string => {
  let ch = open_in_bin(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s;
};

let file_contents = read_whole_file(test_file);

// print_endline(AST.show_exp(Hazel_menhir.Interface.parse_program(file_contents)));

let prog: AST.exp = Hazel_menhir.Interface.parse_program(file_contents);

let dhexp = of_menhir_ast(prog);
print_endline(show(dhexp))
