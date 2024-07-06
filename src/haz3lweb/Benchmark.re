open Haz3lcore;

let sample_1 = {|# Hazel Language Quick Reference #

# Recursive Functions (arrow type annotation required) #
let double_recursively : Int -> Int =
  fun n ->
    if n == 0 then 0
    else double_recursively(n - 1) + 2
in

# Lists #
let empty_list : [Int] = [] in
let non_empty_list : [Int] = 1::2::3::[] in
let list_literals : [Int] = [1, 2, 3] in
let length : [Int] -> Int =
  fun xs ->
    case xs
      | [] => 0
      | hd::tl => 1 + length(tl)
    end
in
let has_at_least_two_elements : [Int] -> Bool =
  fun xs ->
    case xs
      | [] => false
      | hd::[] => false
      | a::b::[] => true
    end
in

# Strings #
let string_lits = "Hello, world!" in
let string_equality = string_lits $== "Hello, world!" in

# Non-empty holes are the red dotted boxes around errors #
# (you can still run programs with non-empty holes) #
let non_empty_hole : Int = true in

2 + 2
|};

let str_to_inserts = (str: string): list(UpdateAction.t) =>
  List.init(
    String.length(str),
    i => {
      let c = String.sub(str, i, 1);
      let c = c == "\n" ? Haz3lcore.Form.linebreak : c;
      UpdateAction.PerformAction(Insert(c));
    },
  );

let actions_1 = str_to_inserts(sample_1) @ [Benchmark(Finish)];

let time = ref(-1.0);

let start = (): unit => {
  time := JsUtil.timestamp();
  print_endline("Benchmark: Starting");
};

let finish = (): unit => {
  switch (time^) {
  | (-1.0) =>
    Printf.sprintf("Benchmark: Error: No benchmark running") |> print_endline

  | _ =>
    let elapsed = (JsUtil.timestamp() -. time^) /. 1000.;
    time := (-1.0);
    Printf.sprintf("Benchmark: Finished: time: %f seconds", elapsed)
    |> print_endline;
  };
};
