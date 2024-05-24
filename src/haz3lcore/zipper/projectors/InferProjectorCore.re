open ZipperBase;

let display_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Unknown(Internal)
  };

let mk = (data: infer): projector_module =>
  (module
   {
     [@deriving (show({with_path: false}), sexp, yojson)]
     type t = infer;
     let data = data;
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
       display_ty(data.expected_ty) |> Typ.pretty_print |> String.length;

     let update = (ci: option(Info.t)): proj_type => {
       print_endline("updating infer projector");
       let expected_ty =
         switch (ci) {
         | Some(InfoExp({mode, _}) | InfoPat({mode, _})) =>
           Mode.ty_of(mode)
         | _ => Typ.Float
         };
       Infer({expected_ty: Some(expected_ty)});
     };
   });
