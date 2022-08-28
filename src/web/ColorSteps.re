type colorMap = Core.Id.Map.t(string);

/*[@deriving sexp]*/
type t = (colorMap, int);

/* TODO: Hannah - Pick 7 or so distinct colors from the different color generator thing (HSLuv)
   Make sure distinguishable for color blind or greyscale
   - think about related colors for related concepts*/
let child_colors = ["blue", "pink", "teal", "orange", "purple", "yellow"];

let empty = (Core.Id.Map.empty, 0);
let get_color = (id: Core.Id.t, (mapping, index): t): (string, t) =>
  switch (Core.Id.Map.find_opt(id, mapping)) {
  | None =>
    let color = List.nth(child_colors, index mod List.length(child_colors));
    let mapping = Core.Id.Map.add(id, color, mapping);
    (color, (mapping, index + 1));
  | Some(color) => (color, (mapping, index))
  };

let to_list = ((map, _): t): list((int, string)) => {
  List.of_seq(Core.Id.Map.to_seq(map));
};
/*
 let pring_color_map = ((map, _): t): unit => {
   Map.iter(
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
 };*/
