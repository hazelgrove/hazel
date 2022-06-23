Logger.init_log();

let initial_model =
  Permalink.get_current() |> Permalink.get_model(~initial_model=Model.init());

// Start the main app.
// See <https://github.com/janestreet/incr_dom/blob/master/src/start_app.mli>.
Incr_dom.Start_app.start(
  (module Hazel),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model,
);
