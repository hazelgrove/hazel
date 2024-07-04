open Tree

type 'a tree = 'a p
type derive = { jdmt : string; rule : string }

type virtual_node = Leaf of string | Branch of flex_direction * align_items
and flex_direction = Row | Column
and align_items = Start | End

type virtual_node_tree = virtual_node tree

let rec mk_virtual_node_tree (Node ({ jdmt; rule }, c)) =
  Node
    ( Branch (Column, Start),
      [
        Node
          ( Branch (Row, End),
            [
              Node (Branch (Row, End), List.map mk_virtual_node_tree c);
              Node (Leaf rule, []);
            ] );
        Node (Leaf jdmt, []);
      ] )

type size = { w : int; h : int }

let rec mk_size_tree (Node (vn, c)) =
  let children_size = List.map mk_size_tree c in
  let size =
    match vn with
    | Leaf s -> { w = String.length s + 1; h = 1 }
    | Branch (fd, _) ->
        let get_acc_size acc (Node ({ w; h }, _)) =
          match fd with
          | Row -> { w = acc.w + w; h = max acc.h h }
          | Column -> { w = max acc.w w; h = acc.h + h }
        in
        List.fold_left get_acc_size { w = 0; h = 0 } children_size
  in
  Node (size, children_size)

type loc = { x : int; y : int }

let rec mk_loc_tree loc (Node (vn, c_vn)) (Node (sz, c_sz)) =
  match vn with
  | Leaf _ -> Node (loc, [])
  | Branch (fd, ai) ->
      let get_ref_loc { x; y } { w; h } =
        match (fd, ai) with
        | Row, Start | Column, Start -> { x; y }
        | Row, End -> { x; y = y + h }
        | Column, End -> { x = x + w; y }
      in
      let get_loc { x; y } { w; h } =
        match (fd, ai) with
        | Row, Start | Column, Start -> { x; y }
        | Row, End -> { x; y = y - h }
        | Column, End -> { x = x - w; y }
      in
      let update_acc_loc { x; y } { w; h } =
        match fd with Row -> { x = x + w; y } | Column -> { x; y = y + h }
      in
      let ref_loc = get_ref_loc loc sz in
      let get_acc_loc (acc_loc, c_loc) vn (Node (sz, c_sz)) =
        let loc = get_loc acc_loc sz in
        let loc_tree = mk_loc_tree loc vn (Node (sz, c_sz)) in
        let acc_loc = update_acc_loc acc_loc sz in
        (acc_loc, c_loc @ [ loc_tree ])
      in
      let _, c_loc = List.fold_left2 get_acc_loc (ref_loc, []) c_vn c_sz in
      Node (loc, c_loc)

let mk_loc_tree t = mk_loc_tree { x = 0; y = 0 } t (mk_size_tree t)

let mk_loc_list t =
  Tree.combine (t, mk_loc_tree t)
  |> flatten
  |> List.filter_map (function
       | Leaf s, loc -> Some (loc, s)
       | Branch _, _ -> None)
  |> List.sort (fun ({ x = x1; y = y1 }, _) ({ x = x2; y = y2 }, _) ->
         if y1 = y2 then x1 - x2 else y1 - y2)

type tile = Inline of int | Block of size | Text of string

let flatloc2tiles t =
  let get_acc_tiles (acc, { x; y }) ({ x = x'; y = y' }, str) =
    let acc =
      acc
      @ [
          (if y' - y = 0 then Inline (x' - x) else Block { w = x'; h = y' - y });
          Text str;
        ]
    in
    (acc, { x = x' + String.length str; y = y' })
  in
  t |> List.fold_left get_acc_tiles ([], { x = 0; y = 0 }) |> fst

let print_tile tile =
  match tile with
  | Text str -> print_string str
  | Block { w; h } ->
      let str = String.make h '\n' ^ String.make w ' ' in
      print_string str
  | Inline w -> print_string (String.make w ' ')

let print_tiles tiles = List.iter print_tile tiles

let pipeline code_tree =
  code_tree |> mk_virtual_node_tree |> mk_loc_list |> flatloc2tiles
  |> print_tiles
