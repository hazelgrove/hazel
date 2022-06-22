Logger.init_log();

let initial_model =
  switch (List.assoc_opt("program", Js_of_ocaml.Url.Current.arguments)) {
  | Some(sexp) =>
    let program = sexp |> Sexplib.Sexp.of_string |> Program.t_of_sexp;
    Model.init() |> Model.put_program(program);
  | None => Model.init()
  };

// Start the main app.
// See <https://github.com/janestreet/incr_dom/blob/master/src/start_app.mli>.
Incr_dom.Start_app.start(
  (module Hazel),
  ~debug=false,
  ~bind_to_element_with_id="container",
  ~initial_model,
);
