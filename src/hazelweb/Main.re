Logger.init_log();

let initial_model =
  Model.init()
  |> (
    Permalink.get_current()
    |> Permalink.get_program
    |> Option.map(Model.put_program)
    |> Option.value(~default=model => model)
  );

// Start the main app.
// See <https://github.com/janestreet/incr_dom/blob/master/src/start_app.mli>.
Incr_dom.Start_app.start(
  (module Hazel),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model,
);
