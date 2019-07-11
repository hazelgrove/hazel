open Incr_dom;
let () =
  Start_app.start(
    (module Hazel),
    ~debug=false,
    ~bind_to_element_with_id="container",
    ~initial_model=Model.init(),
  );
