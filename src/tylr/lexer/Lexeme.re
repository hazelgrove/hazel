type t =
  | S(Space.elem)
  | G(Grout.t)
  | T(Tile.t);
type s = list(t);

let is_porous =
  fun
  | S(_)
  | G(_) => true
  | T(_) => false;

let is_space =
  fun
  | G(_)
  | T(_) => None
  | S(s) => Some(s);

let token =
  fun
  | G(g) => g.prefix
  | T(t) => t.token
  | S(s) => Space.to_string(s);

let s_of_space: Space.t => s = List.map(s => S(s));

let s_of_piece = ({shape, space: (l, r)}: Piece.t) => {
  let l_shape =
    switch (shape) {
    | T(t) => T(t)
    | G(g) => G(g)
    };
  s_of_space(l) @ [l_shape, ...s_of_space(r)];
};
