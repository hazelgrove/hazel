open Hazel_menhir;

let test_file = "/home/green726/coding/hazel/src/menhir-test/test.hazel"

let read_whole_file = filename: string => {
    let ch = open_in_bin(filename);
    let s = really_input_string(ch, in_channel_length(ch));
    close_in(ch);
    s
}

let file_contents = read_whole_file(test_file);

print_endline(AST.show_exp(Interface.parse_program(file_contents)));
