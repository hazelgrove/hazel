open Haz3lcore;
open Projector;

let update = (ci: option(Info.t), p: t): t =>
  switch (p) {
  | Fold => Fold
  | Infer(_) =>
    print_endline("updating infer projector");
    let expected_ty =
      switch (ci) {
      | Some(InfoExp({mode, _}) | InfoPat({mode, _})) => Mode.ty_of(mode)
      | _ => Typ.Float
      };
    Infer({expected_ty: Some(expected_ty)});
  };

let update_all = ({settings, editors, statics, _} as model: Model.t): Model.t => {
  let statics = Editors.lookup_statics(~settings, ~statics, editors);
  let editors =
    Editors.map_projectors(editors, (id, p) =>
      update(Id.Map.find_opt(id, statics.info_map), p)
    );
  {...model, editors};
};
