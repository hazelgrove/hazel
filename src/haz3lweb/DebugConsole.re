open Haz3lcore;

/* This is a place to add ad-hoc debugging print actions.
   It was originally directly in Keyboard, but that added a handler
   dependency on the model, which is technically against architecture */

let print = ({settings, editors, _}: Model.t, key: string): unit => {
  let z = Editors.get_zipper(editors);
  let print = str => str |> print_endline;
  let settings = settings;
  let term = z => z |> MakeTerm.from_zip_for_view |> fst;
  let ctx_init = Editors.get_ctx_init(~settings, editors);
  switch (key) {
  | "F1" => z |> Zipper.show |> print
  | "F2" => z |> Zipper.unselect_and_zip |> Segment.show |> print
  | "F3" => z |> term |> TermBase.UExp.show |> print
  | "F4" =>
    z
    |> term
    |> Interface.Statics.mk_map_ctx(settings.core, ctx_init)
    |> Statics.Map.show
    |> print
  | "F5" =>
    let env_init = Editors.get_env_init(~settings, editors);
    Interface.eval_z(~settings=settings.core, ~env_init, ~ctx_init, z)
    |> ProgramResult.show
    |> print;
  | "F6" =>
    let index = Indicated.index(z);
    let map =
      z |> term |> Interface.Statics.mk_map_ctx(settings.core, ctx_init);
    switch (index) {
    | Some(index) =>
      switch (Haz3lcore.Id.Map.find_opt(index, map)) {
      | Some(ci) => print(Info.show(ci))
      | None => print("DEBUG: No CI found for index")
      }
    | None => print("DEBUG: No indicated index")
    };

  | _ => print("DEBUG: No action for key: " ++ key)
  };
};
