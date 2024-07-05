type cursor('update) = {
  info: option(Haz3lcore.Info.t),
  selected_text: option(string),
  paste: string => option('update),
  projector: option(('update => Ui_effect.t(unit)) => Util.Web.Node.t),
};

let map = (f: 'a => 'b, cursor) => {
  ...cursor,
  paste: x => x |> cursor.paste |> Option.map(f),
  projector:
    cursor.projector
    |> Option.map((proj, inject) => proj(u => inject(f(u)))),
};

let map_opt = (f: 'a => option('b), cursor) => {
  ...cursor,
  paste: x => x |> cursor.paste |> Option.bind(_, f),
  projector:
    cursor.projector
    |> Option.map((proj, inject) =>
         proj(u =>
           switch (f(u)) {
           | None => Ui_effect.Ignore
           | Some(u) => inject(u)
           }
         )
       ),
};

let empty = {
  info: None,
  selected_text: None,
  paste: _ => None,
  projector: None,
};

let (let+) = (cursor, f) => map(f, cursor);
