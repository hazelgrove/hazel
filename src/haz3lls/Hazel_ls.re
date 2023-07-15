open Message_Constructs;

let test_file = "/home/green726/coding/hazel/src/haz3lls/test.json"


let read_whole_file = filename: string => {
    let ch = open_in_bin(filename);
    let s = really_input_string(ch, in_channel_length(ch));
    close_in(ch);
    s
}

let file_contents = read_whole_file(test_file);

let json_contents = Yojson.Safe.from_string(file_contents);
print_endline(Yojson.Safe.show(json_contents));
print_endline("Done with original");

let msg = Message.t_of_yojson(json_contents);
print_endline(msg.jsonrpc);
