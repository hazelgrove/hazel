open Sexplib;

module Steps = {
  type t = list(ChildIndex.t);
  let compare = (steps1: t, steps2: t): int => {
    let rec compare' = (s1, s2) => {
      switch (s1, s2) {
      | ([], []) => 0
      | ([], _) => (-1)
      | ([x, ..._xs], [y, ..._ys]) when x < y => (-1)
      | (_, []) => 1
      | ([x, ..._xs], [y, ..._ys]) when x > y => 1
      | ([_, ...xs], [_, ...ys]) => compare'(xs, ys)
      };
    };
    compare'(steps1, steps2);
  };
};

module StepsMap = Map.Make(Steps);

/*[@deriving sexp]*/
type t = (StepsMap.t(string), int);

/* TODO: Hannah - Pick 7 or so distinct colors from the different color generator thing (HSLuv)
   Make sure distinguishable for color blind or greyscale
   - think about related colors for related concepts*/
let child_colors = [
  "rgb(122, 153, 182)",
  "rgb(199, 141, 146)",
  "rgb(153, 199, 141)",
  "rgb(235, 164, 84)",
  "rgb(167, 84, 235)",
  "rgb(235, 200, 84)",
];

let empty = (StepsMap.empty, 0);
let get_color = (steps: CursorPath.steps, (mapping, index): t): (string, t) =>
  switch (StepsMap.find_opt(steps, mapping)) {
  | None =>
    let color = List.nth(child_colors, index mod List.length(child_colors));
    let mapping = StepsMap.add(steps, color, mapping);
    (color, (mapping, index + 1));
  | Some(color) => (color, (mapping, index))
  };

let to_list = ((map, _): t): list((list(ChildIndex.t), string)) => {
  List.of_seq(StepsMap.to_seq(map));
};

let pring_color_map = ((map, _): t): unit => {
  StepsMap.iter(
    (key, v) =>
      print_endline(
        "("
        ++ Sexp.to_string(CursorPath.sexp_of_steps(key))
        ++ ", "
        ++ v
        ++ ")",
      ),
    map,
  );
};
