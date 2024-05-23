open Util;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type infer = {expected_ty: option(Typ.t)};

[@deriving (show({with_path: false}), sexp, yojson)]
type fold = unit;

type proj_type =
  | Fold(ref(fold))
  | Infer(ref(infer));

module type P = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;
  let proj_type: proj_type;
  let data: ref(t);

  let placeholder_length: unit => int; //Projector

  let can_project: Piece.t => bool; //ProjectorAction
  let update: option(Info.t) => unit; // ProjectorsUpdate
  //let toggle = (id: Id.t, z: Zipper.t, piece, d, rel) //ProjectorAction
  let get: unit => t;
};

let mkFold = (data): (module P) =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type t = unit;
     let data = ref(data);
     let proj_type = Fold(data);
     let can_project = Piece.is_convex;
     let placeholder_length = () => 2;
     let update = _ => ();
     let get = () => ();
   });

let mkFInfer = (data: infer): (module P) =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type t = infer;
     let data = ref(data);
     let proj_type = Infer(data);
     let can_project = (p: Piece.t): bool =>
       Piece.is_convex(p)
       && (
         switch (p) {
         | Tile(t) => t.mold.out == Exp || t.mold.out == Pat
         | _ => false
         }
       );
     let placeholder_length = _ =>
       switch (data^) {
       | {expected_ty: None, _} => "-" |> String.length
       | {expected_ty: Some(expected_ty), _} =>
         /* NOTE: This assumes pretty_print handles whitespace the same as view */
         //TODO(andrew): cleanup
         print_endline("placeholder_ty: " ++ Typ.pretty_print(expected_ty));
         print_endline(
           "placeholder_length "
           ++ (
             expected_ty |> Typ.pretty_print |> String.length |> string_of_int
           ),
         );
         expected_ty |> Typ.pretty_print |> String.length;
       };
     let update = (ci: option(Info.t)): unit => {
       print_endline("updating infer projector");
       let expected_ty =
         switch (ci) {
         | Some(InfoExp({mode, _}) | InfoPat({mode, _})) =>
           Mode.ty_of(mode)
         | _ => Typ.Float
         };

       data := {expected_ty: Some(expected_ty)};
     };
     let get = () => data^;
   });

[@deriving (show({with_path: false}), sexp, yojson)]
module Map = {
  //[@deriving (show({with_path: false}), sexp, yojson)]
  type p = (module P);
  open Id.Map;
  //[@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(p);
  let empty = empty;
  let find = find_opt;
  let mem = mem;
  let mapi = mapi;
  let update = update;
  let t_of_sexp = _ => Id.Map.empty;
  let sexp_of_t = _ => Sexplib.Sexp.Atom("OPAQUE");
  let yojson_of_t = _ => Yojson.Safe.from_string("OPAQUE");
  let t_of_yojson = _ => Id.Map.empty;
};

let placeholder = (p: (module P), id: Id.t) => {
  let (module P) = p;
  Piece.Tile({
    id,
    label: [String.make(P.placeholder_length(), ' ')],
    mold: Mold.mk_op(Any, []),
    shards: [0],
    children: [],
  });
};
let placehold = (ps: Map.t, p: Piece.t) =>
  switch (Map.find(Piece.id(p), ps)) {
  | None => p
  | Some(pr) => placeholder(pr, Piece.id(p))
  };

let rec of_segment = (projectors, seg: Segment.t): Segment.t => {
  seg |> List.map(placehold(projectors)) |> List.map(of_piece(projectors));
}
and of_piece = (projectors, p: Piece.t): Piece.t => {
  switch (p) {
  | Tile(t) => Tile(of_tile(projectors, t))
  | Grout(_) => p
  | Secondary(_) => p
  };
}
and of_tile = (projectors, t: Tile.t): Tile.t => {
  {...t, children: List.map(of_segment(projectors), t.children)};
};

let piece_is = (projectors: Map.t, p: option(Piece.t)) =>
  switch (p) {
  | Some(p) when Map.mem(Piece.id(p), projectors) =>
    Map.mem(Piece.id(p), projectors) ? Some(Piece.id(p)) : None
  | _ => None
  };

let neighbor_is = (projectors, s: Siblings.t): (option(Id.t), option(Id.t)) => (
  piece_is(projectors, Siblings.left_neighbor(s)),
  piece_is(projectors, Siblings.right_neighbor(s)),
);

let selection_sides_is =
    (projectors, s: Selection.t): (option(Id.t), option(Id.t)) => (
  piece_is(projectors, ListUtil.hd_opt(s.content)),
  piece_is(projectors, ListUtil.last_opt(s.content)),
);
