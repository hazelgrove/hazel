open Haz3lcore;

print_endline("Building common context");

let pz: PersistentZipper.t =
  List.assoc("Common", Init.startup.examples |> snd);

let z: Zipper.t = PersistentZipper.unpersist(pz);

let ctx_init = Builtins.ctx_init;

let ctx =
  z
  |> Zipper.zip
  |> MakeTerm.go
  |> fst
  |> Interface.Statics.mk_map_ctx(CoreSettings.on, ctx_init)
  |> Id.Map.find_opt(Hyper.export_id)
  |> (
    fun
    | None => ctx_init
    | Some(info) => Info.ctx_of(info)
  );

let env_init = Builtins.env_init;

let env: Environment.t =
  z
  |> Interface.eval_z(~settings=CoreSettings.on, ~env_init, ~ctx_init)
  |> ProgramResult.get_state
  |> EvaluatorState.get_tests
  |> TestMap.lookup(Hyper.export_id)
  |> (
    fun
    | None
    | Some([]) => env_init
    | Some([(_, _, env), ..._]) => env
  );

print_endline("Common context built");
