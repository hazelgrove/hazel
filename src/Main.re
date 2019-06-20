open Js_of_ocaml;
open Incr_dom;
let () =
  Start_app.start(
    (module Hazel),
    ~bind_to_element_with_id="container",
    ~initial_model=Hazel.Model.init(),
  );
