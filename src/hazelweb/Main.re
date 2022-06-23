Logger.init_log();

let initial_model = Model.init();
let initial_model =
  switch (Permalink.get_current()) {
  | Some(url) => Permalink.get_model(~initial_model, url)
  | None => initial_model
  };

// Start the main app.
// See <https://github.com/janestreet/incr_dom/blob/master/src/start_app.mli>.
Incr_dom.Start_app.start(
  (module Hazel),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model,
);
