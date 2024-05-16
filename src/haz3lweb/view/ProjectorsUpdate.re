open Haz3lcore;
open Projector;

let update = (info_map: Statics.Map.t, p: t): t =>
  switch (p) {
  | Fold => Fold
  | Infer({id, _}) =>
    print_endline("updating infer projector id:" ++ Id.show(id));
    let expected_ty =
      switch (Id.Map.find_opt(id, info_map)) {
      | Some(InfoExp({mode, _}) | InfoPat({mode, _})) =>
        print_endline("infer: type found");
        Mode.ty_of(mode);
      | _ =>
        print_endline("infer: type not found");
        Typ.Float;
      };
    Infer({id, expected_ty: Some(expected_ty)});
  };

let update_all = ({settings, editors, statics, _} as model: Model.t): Model.t => {
  let statics = Editors.lookup_statics(~settings, ~statics, editors);
  let editors = Editors.map_projectors(editors, update(statics.info_map));
  {...model, editors};
};
