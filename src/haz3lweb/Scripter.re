open Haz3lcore;
let sketch_slide = 3;
let fill_marker = "FILL_ME";
//TODO(andrew): adjust promptgen to replace this with ??

let is_fill_marker: Piece.t => bool =
  fun
  | Tile({label: [t], _}) => t == fill_marker
  | _ => false;

let mk_script = (sketch_str: string): list(UpdateAction.t) => {
  [
    SwitchSlide(sketch_slide),
    PerformAction(Move(Extreme(Up))),
    PerformAction(Select(Resize(Extreme(Down)))),
    Paste(sketch_str),
    PerformAction(Move(Goal(Piece(is_fill_marker, Left)))),
    PerformAction(Select(Term(Current))),
    Paste(Form.expliciter_hole),
    PerformAction(Select(Term(Current))),
    Agent(Prompt(Filler)),
  ];
};
