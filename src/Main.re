open Incr_dom;
let () =
  Start_app.start(
    (module Hazel),
    ~debug=true,
    ~bind_to_element_with_id="container",
    ~initial_model=Model.init(),
  );
