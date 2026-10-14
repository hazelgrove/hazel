open Alcotest;
open Haz3lcore;
open ValueOutline;

let u = ProjectorInfo.utility;

let matches = (re: string, s: string): bool =>
  Str.string_match(Str.regexp(re), s, 0);
let contains = (sub: string, s: string): bool =>
  switch (Str.search_forward(Str.regexp_string(sub), s, 0)) {
  | _ => true
  | exception Not_found => false
  };

/* The value the first probe in [code] saw first. */
let value = (code: string): Language.Exp.t =>
  switch (Test_DynamicTypInfer.first_probe_samples_and_ctx(code)) {
  | ([s, ..._], _) => s.value
  | ([], _) => failf("no samples in: %s", code)
  };

let rec lines = (~indent="", o: out): list(string) =>
  switch (o) {
  | Row(label, body, suffix) =>
    let text =
      switch (body) {
      | Text(t)
      | Summary(_, t) => Lazy.force(t)
      | Pretty(_) => "<pretty>"
      | More(_, hidden, _) => count_note(hidden)
      };
    [indent ++ prefix(label) ++ text ++ suffix];
  | Block(b) =>
    [indent ++ prefix(b.label) ++ b.opener]
    @ List.concat_map(lines(~indent=indent ++ "  "), b.kids)
    @ [indent ++ b.closer ++ b.suffix]
  | Packed(rows) =>
    List.map(
      toks =>
        indent
        ++ String.concat(
             " ",
             List.map(
               fun
               | Item(s) => s
               | PackedMore(_, h, _) => count_note(h),
               toks,
             ),
           ),
      rows,
    )
  };

let show = (~width=80, ~rows=15, ~folds=no_folds, exp) =>
  layout(u, exp, ~width, ~rows, ~folds) |> lines;

let beds = {|
let beds = [
  (name="tomato", water=3, sun=true, tags=["vine", "summer", "red"], yield=(kg=4, grade="A")),
  (name="basil", water=2, sun=true, tags=["herb"], yield=(kg=1, grade="B")),
  (name="fern", water=5, sun=false, tags=["shade", "perennial"], yield=(kg=0, grade="C")),
  (name="squash", water=4, sun=true, tags=["vine", "autumn"], yield=(kg=9, grade="A")),
  (name="mint", water=3, sun=false, tags=["herb", "spreads", "perennial"], yield=(kg=2, grade="B")),
  (name="carrot", water=2, sun=true, tags=["root"], yield=(kg=3, grade="A")),
  (name="lettuce", water=4, sun=false, tags=["leafy", "spring"], yield=(kg=2, grade="B")),
  (name="pepper", water=3, sun=true, tags=["summer", "hot"], yield=(kg=2, grade="A"))
] in ^^probe(beds)
|};

let squares = n =>
  Printf.sprintf(
    {|let squares: Int -> [Int] = fun n -> if n == 0 then [] else squares(n - 1) @ [n * n] in ^^probe(squares(%d))|},
    n,
  );

let tree = {|
type Tree = + Leaf + Node(Tree, Int, Tree) in
let insert: (Tree, Int) -> Tree = fun (t, x) ->
  case t
  | Leaf => Node(Leaf, x, Leaf)
  | Node(l, y, r) => if x < y then Node(insert(l, x), y, r) else Node(l, y, insert(r, x))
  end in
let build: (Tree, [Int]) -> Tree = fun (t, xs) ->
  case xs | [] => t | x :: rest => build(insert(t, x), rest) end in
^^probe(build(Leaf, [50, 30, 70, 20, 40, 60, 80, 10, 25, 35, 45, 55, 65, 75, 90, 5, 15, 33, 48, 62, 85, 95, 1, 12, 38]))
|};

let check_lines = (name, expected, actual) =>
  check(list(string), name, expected, actual);

let test_fit_folds_records = () =>
  check_lines(
    "eight records fit as eight summaries",
    [
      "[",
      {|  (name = "tomato", water = 3, sun = true, tags = ["vine", "summer", …+1], …+1),|},
      {|  (name = "basil", water = 2, sun = true, tags = ["herb"], yield = (…)),|},
      {|  (name = "fern", water = 5, sun = false, tags = ["shade", "perennial"], …+1),|},
      {|  (name = "squash", water = 4, sun = true, tags = ["vine", "autumn"], …+1),|},
      {|  (name = "mint", water = 3, sun = false, tags = ["herb", "spreads", …+1], …+1),|},
      {|  (name = "carrot", water = 2, sun = true, tags = ["root"], yield = (…)),|},
      {|  (name = "lettuce", water = 4, sun = false, tags = ["leafy", "spring"], …+1),|},
      {|  (name = "pepper", water = 3, sun = true, tags = ["summer", "hot"], yield = …)|},
      "]",
    ],
    show(value(beds)),
  );

let test_taller_drawer_opens_more = () => {
  let ls = show(~rows=24, value(beds));
  check(int, "rows", 22, List.length(ls));
  check(string, "first record opens", "  (", List.nth(ls, 1));
};

let test_user_folds_layer_on_fit = () => {
  let v = value(beds);
  let opened =
    show(
      ~folds={
        opened: [("0", true)],
        shown: [],
      },
      v,
    );
  check(int, "opening a record adds its rows", 16, List.length(opened));
  check(
    string,
    "the opened record's field",
    {|    tags = ["vine", "summer", "red"],|},
    List.nth(opened, 5),
  );
  let root_folded =
    show(
      ~folds={
        opened: [("", false)],
        shown: [],
      },
      v,
    );
  check(int, "folding the root leaves one row", 1, List.length(root_folded));
};

let test_fill_packs_short_items = () => {
  let ls = show(value(squares(60)));
  check(int, "60 squares pack into 7 rows", 7, List.length(ls));
  check(bool, "nothing is hidden", false, List.exists(contains("…"), ls));
};

let test_long_list_stops_at_count = () => {
  let v = value(squares(600));
  let ls = show(v);
  check(int, "fills the drawer", 15, List.length(ls));
  let last_items = List.nth(ls, 13);
  check(
    bool,
    "the last packed row ends in a count",
    true,
    matches(".*…\\+[0-9]+$", last_items),
  );
  let more =
    show(
      ~folds={
        opened: [],
        shown: [("", 400)],
      },
      v,
    );
  check(bool, "showing more adds rows", true, List.length(more) > 15);
};

let test_constructor_slots = () => {
  let ls = show(value(tree));
  check(string, "root opens", "Node(", List.hd(ls));
  /* the key between two folded subtrees still shows */
  check(
    bool,
    "a folded constructor keeps its slots",
    true,
    List.exists(matches(".*…), 20, Node("), ls),
  );
  check(
    bool,
    "a constructor name is never cut",
    false,
    List.exists(contains("Le…"), ls),
  );
};

/* Every row fits the width, at any width, for every value. */
let test_rows_fit_width = () =>
  List.iter(
    code => {
      let v = value(code);
      List.iter(
        width =>
          List.iter(
            l =>
              if (Util.Unicode.length(l) > width) {
                failf(
                  "%d-column row is %d wide: %s",
                  width,
                  Util.Unicode.length(l),
                  l,
                );
              },
            show(~width, v),
          ),
        [24, 32, 40, 50, 64, 80, 110],
      );
    },
    [beds, squares(60), squares(600), tree],
  );

/* The fit never needs more rows than the cap (from 3 rows up). */
let test_fit_within_cap = () =>
  List.iter(
    code => {
      let v = value(code);
      List.iter(
        rows => {
          let n = List.length(show(~rows, v));
          if (n > rows) {
            failf("cap %d, %d rows", rows, n);
          };
        },
        [3, 5, 8, 12, 15, 20, 30],
      );
    },
    [beds, squares(60), squares(600), tree],
  );

let tests = (
  "ValueOutline",
  [
    test_case("fit folds records", `Quick, test_fit_folds_records),
    test_case(
      "taller drawer opens more",
      `Quick,
      test_taller_drawer_opens_more,
    ),
    test_case(
      "user folds layer on fit",
      `Quick,
      test_user_folds_layer_on_fit,
    ),
    test_case("fill packs short items", `Quick, test_fill_packs_short_items),
    test_case(
      "long list stops at a count",
      `Quick,
      test_long_list_stops_at_count,
    ),
    test_case("constructor slots", `Quick, test_constructor_slots),
    test_case("rows fit the width", `Quick, test_rows_fit_width),
    test_case("fit stays within the cap", `Quick, test_fit_within_cap),
  ],
);
