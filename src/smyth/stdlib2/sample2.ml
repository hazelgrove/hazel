(*******************************************************************************
 * Parameters
 *)

let max_nat : int = 3

let max_nat_list_length : int = 4

let max_nested_nat_list_length : int = 4

let max_nested_nat_inner_list_length : int = 2

let max_bool_list_length : int = 4

let max_nat_tree_size : int = 4

let max_bool_tree_size : int = 6

(*******************************************************************************
 * Enumeration Sampling
 *)

(* Generic *)

type 'a gen = unit -> 'a

let weight : int -> int -> float = fun _elementSize _size -> 1.0

(* or: elementSize ^ size *)

let all :
    'a -> (int -> 'a list) -> int -> int -> (float * 'a) * (float * 'a) list
    =
 fun base shapes element_size max_size ->
  ( (1.0, base)
  , List2.concat_map
      (fun size ->
        size |> shapes
        |> List.map (fun shape -> (weight element_size size, shape)))
      (List2.range ~low:1 ~high:max_size) )

let constant : 'a -> 'a gen = fun x () -> x

let from : 'a * 'a list -> 'a gen =
 fun (x, xs) () ->
  let choice = Random.int (List.length xs + 1) in
  if Int.equal choice 0 then x else List.nth xs (choice - 1)

let pair : 'a gen -> 'b gen -> ('a * 'b) gen = fun x y () -> (x (), y ())

let triple : 'a gen -> 'b gen -> 'c gen -> ('a * 'b * 'c) gen =
 fun x y z () -> (x (), y (), z ())

(* Semi-Generic *)

type list_shape = Nil | Cons of list_shape

let list_base : list_shape = Nil

let rec list_shapes : int -> list_shape list =
 fun n ->
  if n = 0 then [list_base]
  else List.map (fun s -> Cons s) (list_shapes (n - 1))

let rec list_fill : 'a gen -> list_shape -> 'a list =
 fun gen shape ->
  match shape with Nil -> [] | Cons rest -> gen () :: list_fill gen rest

let list : int -> int -> 'a gen -> 'a list gen =
 fun max_list_size element_size element_gen () ->
  all list_base list_shapes element_size max_list_size
  |> Pervasives2.uncurry Random2.weighted
  |> list_fill element_gen

let nested_list : int -> int -> int -> 'a gen -> 'a list list gen =
 fun max_list_length max_inner_list_length element_size element_gen () ->
  let inner_list_size =
    List2.range ~low:0 ~high:max_inner_list_length
    |> List.map (fun len -> Int2.pow element_size len)
    |> List2.sum
  in
  all list_base list_shapes inner_list_size max_list_length
  |> Pervasives2.uncurry Random2.weighted
  |> list_fill (list max_inner_list_length element_size element_gen)

type tree_shape = Leaf | Node of tree_shape * tree_shape

let tree_base : tree_shape = Leaf

let rec tree_shapes : int -> tree_shape list =
 fun n ->
  if n = 0 then [tree_base]
  else
    List2.concat_map
      (fun k ->
        List.map (fun (left, right) -> Node (left, right))
        @@ List2.cartesian_product (tree_shapes k)
             (tree_shapes @@ (n - 1 - k)))
      (List2.range ~low:0 ~high:(n - 1))

let rec tree_fill : 'a gen -> tree_shape -> 'a Tree2.t =
 fun gen t ->
  match t with
  | Leaf -> Tree2.Leaf
  | Node (left, right) ->
      Tree2.Node (tree_fill gen left, gen (), tree_fill gen right)

let tree : int -> int -> 'a gen -> 'a Tree2.t gen =
 fun max_tree_size element_size element_gen () ->
  all tree_base tree_shapes element_size max_tree_size
  |> Pervasives2.uncurry Random2.weighted
  |> tree_fill element_gen

(* Particular *)

let nat : int gen = fun () -> Random.int (max_nat + 1)

let bool : bool gen = Random.bool

let nat_list : int list gen = list max_nat_list_length (max_nat + 1) nat

let nested_nat_list : int list list gen =
  nested_list max_nested_nat_list_length max_nested_nat_inner_list_length
    (max_nat + 1) nat

let bool_list : bool list gen = list max_bool_list_length 2 bool

let nat_tree : int Tree2.t gen = tree max_nat_tree_size (max_nat + 1) nat

let bool_tree : bool Tree2.t gen = tree max_bool_tree_size 2 bool

(*******************************************************************************
 * IO Sampling
 *)

let io : ('a -> 'b) -> 'a gen -> ('a * 'b) gen =
 fun f gen () ->
  let x = gen () in
  (x, f x)

let io_trial :
       n:int
    -> k:int
    -> ('a -> 'b)
    -> 'a gen
    -> 'a gen option
    -> ('a * 'b) list list =
 fun ~n ~k ref input base_case_opt ->
  List.init n (fun _ ->
      let amounts =
        ( match base_case_opt with
        | None -> []
        | Some base_case -> [(1, io ref base_case)] )
        @ [(k, io ref input)]
      in
      Random2.sample_unique amounts)
