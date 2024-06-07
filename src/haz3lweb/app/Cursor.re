type cursor('update) = {
  info: option(Haz3lcore.Info.t),
  selected_text: option(string),
  paste: option(string => 'update),
};

let map = (f: 'a => 'b, cursor) =>
  switch (cursor.paste) {
  | None => {...cursor, paste: None}
  | Some(paste) => {...cursor, paste: Some(x => x |> paste |> f)}
  };

let empty = {info: None, selected_text: None, paste: None};

let (let+) = (cursor, f) => map(f, cursor);
