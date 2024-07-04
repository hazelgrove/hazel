open ZipperBase;
open Util.StringDom;
open Util.Tree;

type derive = {
  jdmt: string,
  rule: string,
};

let rec mk_virtual_node_tree = (Node({jdmt, rule}, c)) =>
  Node(
    Branch(Column, Start),
    [
      Node(
        Branch(Row, End),
        [
          Node(Branch(Row, End), List.map(mk_virtual_node_tree, c)),
          Node(Leaf(rule), []),
        ],
      ),
      Node(Leaf(jdmt), []),
    ],
  );

let serialize = a =>
  a |> ZipperBase.sexp_of_derivearea_action |> Sexplib.Sexp.to_string;

let deserialize = a =>
  a |> Sexplib.Sexp.of_string |> ZipperBase.derivearea_action_of_sexp;

/* Function to escape linebreaks */
let escape_linebreaks = (str: string): string => {
  Re.Str.global_replace(Re.Str.regexp("\n"), "\\n", str);
};

/* Function to unescape linebreaks */
let unescape_linebreaks = (str: string): string => {
  Re.Str.global_replace(Re.Str.regexp("\\\\n"), "\n", str);
};

let of_mono = (syntax: Piece.t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(unescape_linebreaks(l))
  | _ => None
  };

let mk_mono = (sort: Sort.t, string: string): Piece.t =>
  string |> escape_linebreaks |> Form.mk_atomic(sort) |> Piece.mk_tile(_, []);

let state_of = (piece: Piece.t): option(string) => piece |> of_mono;

let get = (piece: Piece.t): string =>
  switch (piece |> of_mono) {
  | None => failwith("TextArea: not string literal")
  | Some(s) => s
  };

let put = (s: string): Piece.t => s |> mk_mono(Exp);

let mk = (syntax, _model): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = ZipperBase.derivearea;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = ZipperBase.derivearea_action;
     let model = ();
     let projector = DeriveArea(model);
     let can_project = _ => true;
     //TODO(andrew): cleanup
     let row = Util.StringUtil.num_linebreaks(get(syntax));
     /* +2 for left and right padding */
     let col =
       2
       + List.fold_left(
           max,
           0,
           List.map(
             String.length,
             Re.Str.split(Re.Str.regexp("\n"), get(syntax)),
           ),
         );
     let placeholder = () => ZipperBase.Block({row, col});
     let auto_update = _: projector => DeriveArea();
     let update = _action => DeriveArea();
   });
