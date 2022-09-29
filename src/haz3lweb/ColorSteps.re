type colorMap = Haz3lcore.Id.Map.t(string);

/*[@deriving sexp]*/
type t = (colorMap, int);

/* TODO: Hannah - Pick 7 or so distinct colors from the different color generator thing (HSLuv)
   Make sure distinguishable for color blind or greyscale
   - think about related colors for related concepts*/
let child_colors = ["blue", "pink", "teal", "orange", "purple", "yellow"];

let empty = (Haz3lcore.Id.Map.empty, 0);
let get_color = (id: Haz3lcore.Id.t, (mapping, index): t): (string, t) =>
  switch (Haz3lcore.Id.Map.find_opt(id, mapping)) {
  | None =>
    let color = List.nth(child_colors, index mod List.length(child_colors));
    let mapping = Haz3lcore.Id.Map.add(id, color, mapping);
    (color, (mapping, index + 1));
  | Some(color) => (color, (mapping, index))
  };

let to_list = (map: colorMap): list((int, string)) => {
  List.of_seq(Haz3lcore.Id.Map.to_seq(map));
};
