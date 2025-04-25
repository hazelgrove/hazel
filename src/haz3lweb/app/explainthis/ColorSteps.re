open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type colorMap = Haz3lcorep.Id.Map.t(string);

/*[@deriving sexp]*/
type t = (colorMap, int);

/* TODO: Hannah - Pick 7 or so distinct colors from the different color generator thing (HSLuv)
   Make sure distinguishable for color blind or greyscale
   - think about related colors for related concepts*/
let child_colors = ["a", "b", "c"];

let empty = (Haz3lcorep.Id.Map.empty, 0);
let get_color = (id: Haz3lcorep.Id.t, (mapping, index): t): (string, t) =>
  switch (Haz3lcorep.Id.Map.find_opt(id, mapping)) {
  | None =>
    let color = List.nth(child_colors, index mod List.length(child_colors));
    let mapping = Haz3lcorep.Id.Map.add(id, color, mapping);
    (color, (mapping, index + 1));
  | Some(color) => (color, (mapping, index))
  };

let to_list = (map: colorMap): list((Haz3lcorep.Id.t, string)) => {
  List.of_seq(Haz3lcorep.Id.Map.to_seq(map));
};
