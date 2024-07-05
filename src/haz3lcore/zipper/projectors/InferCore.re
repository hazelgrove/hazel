open ZipperBase;

let display_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Unknown(Internal) |> Typ.temp
  };

let expected_ty = (info: option(Info.t)) =>
  switch (info) {
  | Some(InfoExp({mode, _}) | InfoPat({mode, _})) => Mode.ty_of(mode)
  | _ => Unknown(Internal) |> Typ.temp
  };

let mk = (model: infer): projector_core =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type model = infer;
     [@deriving (show({with_path: false}), sexp, yojson)]
     type action = infer_action;
     let model = model;
     let projector = Infer(model);

     let can_project = (p: Piece.t): bool =>
       switch (p) {
       | Tile(t) => t.mold.out == Exp || t.mold.out == Pat
       | _ => false
       };

     let placeholder_length = () =>
       3
       + (display_ty(model.expected_ty) |> Typ.pretty_print |> String.length);

     let auto_update = ({info, _}): projector => {
       print_endline("updating infer projector");
       Infer({expected_ty: Some(expected_ty(info))});
     };
     let update = _action => Infer(model);
   });
