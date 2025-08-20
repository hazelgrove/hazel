/* This file is a placeholder, ideally an algorithm would be implemented here that allows
   efficient calculation of the best way to add linebreaks etc, but that hasn't been implemented yet, so
   none of these functions do anything yet. (Matt) */

type pretty = Segment.t;

let p_concat = (pretty2, pretty1) => pretty1 @ pretty2;
let p_or = (_pretty2, pretty1) => pretty1;
let p_orif = (cond, pretty2, pretty1) => if (cond) {pretty1} else {pretty2};
let p_just = segment => segment;

let p_concat = (pretties: list(pretty)) =>
  List.fold_left(p_concat, [], pretties);

let (let+) = (pretty, f) => f(pretty);
let (and+) = (pretty1, pretty2) => (pretty1, pretty2);

let ( let* ) = (pretty, f) => f(pretty);
let ( and* ) = (pretty1, pretty2) => (pretty1, pretty2);

let all = x => x;

let select: pretty => Segment.t = x => x;

let rec format_segment = (~max_width: int, pretty: pretty) => {
  switch (pretty) {
  | [] => []
  | [p] => [format_piece(~max_width, p)]
  | segments =>
    let s = Segment.to_string(~projector_to_segment=_ => [], pretty);
    let segments =
      if (String.length(s) > max_width) {
        // Interleave segments with linebreaks
        List.concat_map(
          (piece): list(Piece.t) => {
            print_endline("Piece" ++ Piece.show(piece));
            print_endline(
              string_of_bool(Piece.is_infix_delimiter_op_prefix(piece)),
            );
            if (Piece.is_tile(piece)
                |> Option.map((t: Piece.tile) => Mold.is_infix_op(t.mold))
                |> Option.value(~default=false)) {
              [piece, Secondary(Secondary.mk_newline(Id.mk()))];
            } else {
              [piece];
            };
          },
          segments,
        );
      } else {
        segments;
      };
    segments |> List.map(format_piece(~max_width, _));
  };
}
and format_piece = (~max_width: int, piece: Piece.t): Piece.t => {
  switch (piece) {
  | Grout(g) => Piece.Grout(g)
  | Tile(t) =>
    let formatted_children =
      List.map(format_segment(~max_width, _), t.children);
    Tile({
      ...t,
      children: formatted_children,
    });
  | Secondary(w) => piece
  | Projector(_) => piece
  };
};
