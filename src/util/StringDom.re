open Sexplib.Std;
open Tree;

type tree('a) = p('a);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Leaf(string)
  | Branch(flex_direction, align_items)
and flex_direction =
  | Row
  | Column
and align_items =
  | Start
  | End;

[@deriving (show({with_path: false}), sexp, yojson)]
type size = {
  w: int,
  h: int,
};

let rec mk_size_tree = (Node(vn, c)) => {
  let children_size = List.map(mk_size_tree, c);
  let size =
    switch (vn) {
    | Leaf(s) =>
      let lines = s |> String.split_on_char('\n');
      {
        w: lines |> List.map(String.length) |> List.fold_left(max, 0),
        h: lines |> List.length,
      };
    | Branch(fd, _) =>
      let get_acc_size = ({w, h}, Node({w: w', h: h'}, _)) =>
        switch (fd) {
        | Row => {w: w + w', h: max(h, h')}
        | Column => {w: max(w, w'), h: h + h'}
        };
      List.fold_left(get_acc_size, {w: 0, h: 0}, children_size);
    };
  Node(size, children_size);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type loc = {
  x: int,
  y: int,
};

let rec mk_loc_tree = (loc, Node(vn, c_vn), Node(sz, c_sz)) =>
  switch (vn) {
  | Leaf(_) => Node(loc, [])
  | Branch(fd, ai) =>
    let get_ref_loc = ({x, y}, {w, h}) =>
      switch (fd, ai) {
      | (Row, Start)
      | (Column, Start) => {x, y}
      | (Row, End) => {x, y: y + h}
      | (Column, End) => {x: x + w, y}
      };
    let get_loc = ({x, y}, {w, h}) =>
      switch (fd, ai) {
      | (Row, Start)
      | (Column, Start) => {x, y}
      | (Row, End) => {x, y: y - h}
      | (Column, End) => {x: x - w, y}
      };
    let update_acc_loc = ({x, y}, {w, h}) =>
      switch (fd) {
      | Row => {x: x + w, y}
      | Column => {x, y: y + h}
      };
    let ref_loc = get_ref_loc(loc, sz);
    let get_acc_loc = ((acc_loc, c_loc), vn, Node(sz, c_sz)) => {
      let loc = get_loc(acc_loc, sz);
      let loc_tree = mk_loc_tree(loc, vn, Node(sz, c_sz));
      let acc_loc = update_acc_loc(acc_loc, sz);
      (acc_loc, c_loc @ [loc_tree]);
    };
    let (_, c_loc) =
      List.fold_left2(get_acc_loc, (ref_loc, []), c_vn, c_sz);
    Node(loc, c_loc);
  };
let mk_loc_tree = (loc, t) => mk_loc_tree(loc, t, mk_size_tree(t));

let mk_loc_list = (loc, t) =>
  Tree.combine((t, mk_loc_tree(loc, t)))
  |> flatten
  |> List.filter_map(
       fun
       | (Leaf(s), loc) => Some((loc, s))
       | (Branch(_), _) => None,
     )
  |> List.sort((({x: x1, y: y1}, _), ({x: x2, y: y2}, _)) =>
       y1 == y2 ? x1 - x2 : y1 - y2
     );

type tile =
  | Inline(int)
  | Block(size)
  | Text(string);

let flatloc2tiles = t => {
  let get_acc_tiles = ((acc, {x, y}), ({x: x', y: y'}, str)) => {
    let acc =
      acc
      @ [
        if (y' - y == 0) {
          Inline(x' - x);
        } else {
          Block({w: x', h: y' - y});
        },
        Text(str),
      ];

    (acc, {x: x' + String.length(str), y: y'});
  };

  t |> List.fold_left(get_acc_tiles, ([], {x: 0, y: 0})) |> fst;
};

let print_tile = tile =>
  switch (tile) {
  | Text(str) => print_string(str)
  | Block({w, h}) =>
    let str = String.make(h, '\n') ++ String.make(w, ' ');
    print_string(str);
  | Inline(w) => print_string(String.make(w, ' '))
  };

let print_tiles = tiles => List.iter(print_tile, tiles);
