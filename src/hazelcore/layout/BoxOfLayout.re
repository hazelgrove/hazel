open Box;

// Note: tags are inside-out (i.e. List.hd(tags) is the inner-most tag)
let rec wrap_box_tags = (tags: list('tag), box: t('tag)): t('tag) => {
  switch (tags) {
  | [] => box
  | [tag, ...tags] => wrap_box_tags(tags, Tagged(tag, box))
  };
};

let rec box_append = (tags: list('tag), b1: t('tag), b2: t('tag)): t('tag) =>
  if (box_height(b1) <= 1) {
    HBox([wrap_box_tags(tags, b1), b2]);
  } else {
    let rec append_last = (bs1: list(t('tag))): list(t('tag)) => {
      switch (bs1) {
      | [] => failwith(__LOC__) // impossible due to `box_height` guard
      | [b1] => [box_append(tags, b1, b2)]
      | [b1, ...bs1] => [wrap_box_tags(tags, b1), ...append_last(bs1)]
      };
    };
    switch (b1) {
    | Text(_) => failwith(__LOC__) // impossible due to `box_height` gard
    | HBox(bs1) => HBox(append_last(bs1))
    | VBox(bs1) => VBox(append_last(bs1))
    | Tagged(tag, b) => box_append([tag, ...tags], b, b2)
    };
  };

open Sexplib.Std;
[@deriving sexp]
type prebox('tag) = {
  initial_linebreak: bool,
  lines: list(t('tag)),
  final_linebreak: bool,
};

let box_of_prebox = (prebox: prebox('tag)): t('tag) => {
  let prefix =
    if (prebox.initial_linebreak) {
      [Text("")];
    } else {
      [];
    };
  let suffix =
    if (prebox.final_linebreak) {
      [Text("")];
    } else {
      [];
    };
  VBox(prefix @ prebox.lines @ suffix); // TODO: omit VBox if only one line?
};

let rec prebox_of_layout = (layout: Layout.t('tag)): prebox('tag) => {
  switch (layout) {
  | Text(string) => {
      initial_linebreak: false,
      lines: [Text(string)],
      final_linebreak: false,
    }
  | Linebreak => {initial_linebreak: true, lines: [], final_linebreak: true} // TODO: initial vs final vs both
  | Tagged(t, l) => {
      initial_linebreak: false,
      lines: [Tagged(t, box_of_prebox(prebox_of_layout(l)))],
      final_linebreak: false,
    }
  | Align(l) => {
      initial_linebreak: false,
      lines: [box_of_prebox(prebox_of_layout(l))],
      final_linebreak: false,
    }
  | Cat(l1, l2) =>
    let prebox1 = prebox_of_layout(l1);
    let prebox2 = prebox_of_layout(l2);
    let new_lines =
      switch (prebox1.final_linebreak, prebox2.initial_linebreak) {
      | (false, true)
      | (true, false) => prebox1.lines @ prebox2.lines
      | (true, true) => prebox1.lines @ [Text("")] @ prebox2.lines
      | (false, false) =>
        // TODO: assert(Shape.shape_of_layout(l1).lines == 1);
        let rec go =
                (bs1: list(t('tag)), bs2: list(t('tag))): list(t('tag)) => {
          switch (bs1) {
          | [] => bs2
          | [b1] =>
            switch (bs2) {
            | [] => [b1]
            | [b2, ...bs2] => [box_append([], b1, b2), ...bs2]
            }
          | [b1, ...bs1] => [b1, ...go(bs1, bs2)]
          };
        };
        go(prebox1.lines, prebox2.lines);
      };
    {
      initial_linebreak: prebox1.initial_linebreak,
      lines: new_lines,
      final_linebreak: prebox2.final_linebreak,
    };
  };
};

let box_of_layout = (layout: Layout.t('tag)): t('tag) => {
  Printf.printf(
    "box_of_layout:\n%s\n------\n%!",
    Sexplib.Sexp.to_string_hum(
      Layout.sexp_of_t(_ => Sexplib.Std.sexp_of_string("<tag>"), layout),
    ),
  );
  let prebox = prebox_of_layout(layout);
  Printf.printf(
    "prebox:\n%s\n------\n%!",
    Sexplib.Sexp.to_string_hum(
      sexp_of_prebox(_ => Sexplib.Std.sexp_of_string("<tag>"), prebox),
    ),
  );
  let flattened = Box.flatten(box_of_prebox(prebox));
  Printf.printf(
    "flattened:\n%s\n------\n%!",
    Sexplib.Sexp.to_string_hum(
      Box.sexp_of_t(_ => Sexplib.Std.sexp_of_string("<tag>"), flattened),
    ),
  );
  flattened;
};
