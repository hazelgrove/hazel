open Web;
open Js_of_ocaml;

type result = {
  total_actions: int,
  completed_actions: int,
  update_time_ms: float,
  calculate_time_ms: float,
  dynamics_count: int,
  error: option(string),
};

let parse_log = (log_data: string): list((float, Page.Update.t)) => {
  let of_data = (data: string): list((float, Page.Update.t)) =>
    data
    |> Sexplib.Sexp.of_string
    |> Log.Entry.s_of_sexp_opt
    |> List.filter_map(x => x);
  log_data |> of_data |> Log.flatten_imports(~of_data);
};

let init_model = (initial_state: option(Export.full_state)): History.Model.t => {
  switch (initial_state) {
  | Some(state) =>
    Export.import_full_state(
      state,
      ~exercise_specs=ExerciseSettings.exercises,
      ~tutorial_specs=TutorialSettings.lessons,
    );
    History.Model.load();
  | None => History.Model.reset()
  };
};

let now = () => Js.to_float(Js.Unsafe.js_expr("Date.now()"));

let replay = (log_export: Export.log_export): result => {
  let actions = parse_log(log_export.log);
  let total = List.length(actions);
  let model = ref(init_model(log_export.initial_state));
  let noop = _ => ();
  let noop_get_log_and = (_f: string => unit): unit => ();
  let noop_import_log = (_s: string): unit => ();

  // Initial calculate to move fields out of Calc.Pending
  model :=
    History.Update.calculate(
      ~schedule_action=noop,
      ~is_edited=true,
      ~dynamics=false,
      ~force_sync_eval=true,
      model^,
    );

  let update_time = ref(0.0);
  let calc_time = ref(0.0);
  let dynamics_count = ref(0);
  let completed = ref(0);
  let error = ref(None);

  let rec loop = remaining =>
    switch (remaining) {
    | [] => ()
    | [(_, action), ...rest] =>
      let t0 = now();
      let updated =
        History.Update.update(
          ~import_log=noop_import_log,
          ~get_log_and=noop_get_log_and,
          ~schedule_action=noop,
          action,
          model^,
        );
      let t1 = now();
      update_time := update_time^ +. (t1 -. t0);
      model := updated.model;

      switch (action) {
      | EvalComplete =>
        dynamics_count := dynamics_count^ + 1;
        let t2 = now();
        model :=
          History.Update.calculate(
            ~schedule_action=noop,
            ~is_edited=true,
            ~dynamics=true,
            ~force_sync_eval=true,
            model^,
          );
        let t3 = now();
        calc_time := calc_time^ +. (t3 -. t2);
      | _ => ()
      };

      completed := completed^ + 1;
      if (completed^ mod 100 == 0) {
        Printf.printf("  Action %d/%d\n%!", completed^, total);
      };
      loop(rest);
    };

  switch (loop(actions)) {
  | () => ()
  | exception exn =>
    error :=
      Some(
        Printf.sprintf(
          "Action %d/%d: %s",
          completed^,
          total,
          Printexc.to_string(exn),
        ),
      );
  };

  {
    total_actions: total,
    completed_actions: completed^,
    update_time_ms: update_time^,
    calculate_time_ms: calc_time^,
    dynamics_count: dynamics_count^,
    error: error^,
  };
};

let () =
  Js.export(
    "LogReplay",
    Js.wrap_callback(data => {
      let log_export =
        Js.to_string(data)
        |> Yojson.Safe.from_string
        |> Export.log_export_of_yojson;
      let result = replay(log_export);
      let json =
        Printf.sprintf(
          {|{"total_actions":%d,"completed_actions":%d,"update_time_ms":%.1f,"calculate_time_ms":%.1f,"dynamics_count":%d,"error":%s}|},
          result.total_actions,
          result.completed_actions,
          result.update_time_ms,
          result.calculate_time_ms,
          result.dynamics_count,
          switch (result.error) {
          | None => "null"
          | Some(e) => "\"" ++ String.escaped(e) ++ "\""
          },
        );
      Js.string(json);
    }),
  );
