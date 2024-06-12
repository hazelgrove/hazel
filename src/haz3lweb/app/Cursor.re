type cursor('update) = {
  info: option(Haz3lcore.Info.t),
  selected_text: option(string),
  paste: string => option('update),
};

let map = (f: 'a => 'b, cursor) => {
  ...cursor,
  paste: x => x |> cursor.paste |> Option.map(f),
};

let map_opt = (f: 'a => option('b), cursor) => {
  ...cursor,
  paste: x => x |> cursor.paste |> Option.bind(_, f),
};

let empty = {info: None, selected_text: None, paste: _ => None};

let (let+) = (cursor, f) => map(f, cursor);
