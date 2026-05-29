let rec inline_to_string = (inline: Omd.inline(_)): string =>
  switch (inline) {
  | Omd.Concat(_, items) =>
    String.concat("", List.map(inline_to_string, items))
  | Omd.Text(_, s) => s
  | Omd.Code(_, s) => s
  | Omd.Emph(_, d)
  | Omd.Strong(_, d) => inline_to_string(d)
  | Omd.Link(_, {label, _}) => inline_to_string(label)
  | Omd.Soft_break(_)
  | Omd.Hard_break(_) => " "
  | _ => ""
  };

let split = (doc: Omd.doc): list((option(string), Omd.doc)) => {
  let close_section = (cur_head, cur_body, acc) =>
    switch (cur_head, cur_body) {
    | (Option.None, []) => acc
    | _ => [(cur_head, List.rev(cur_body)), ...acc]
    };
  let rec go = (acc, cur_head, cur_body, blocks) =>
    switch (blocks) {
    | [] => List.rev(close_section(cur_head, cur_body, acc))
    | [Omd.Heading(_, 3, inline), ...rest] =>
      let acc' = close_section(cur_head, cur_body, acc);
      go(acc', Option.Some(inline_to_string(inline)), [], rest);
    | [b, ...rest] => go(acc, cur_head, [b, ...cur_body], rest)
    };
  go([], Option.None, [], doc);
};
