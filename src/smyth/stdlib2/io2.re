let read_all: in_channel => string = (
  channel => {
    let contents = really_input_string(channel, in_channel_length(channel));

    close_in(channel);
    contents;
  }:
    in_channel => string
);

let read_file: string => string = (
  path => read_all(open_in(path)): string => string
);

let path: list(string) => string = (
  parts => {
    let standard =
      parts
      |> List2.concat_map(String.split_on_char('/'))
      |> List.filter(s => !String.equal(s, ""))
      |> String.concat("/");

    let combined = parts |> String.concat("") |> String.trim;

    if (String.length(combined) > 0 && combined.[0] == '/') {
      "/" ++ standard;
    } else {
      standard;
    };
  }:
    list(string) => string
);

let read_path: list(string) => string = (
  parts => parts |> path |> read_file: list(string) => string
);

let visible_files: string => list(string) = (
  dir =>
    dir
    |> Sys.readdir
    |> Array.to_list
    |> List.filter(file =>
         !Sys.is_directory(path([dir, file]))
         && String.length(file) > 0
         && file.[0] != '.'
       )
    |> List.sort(String.compare):
    string => list(string)
);
