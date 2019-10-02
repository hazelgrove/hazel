open Incr_dom;
open Front_end;
let () = {
  Logger.init_log();
  Start_app.start(
    (module Hazel),
    ~debug=false,
    ~bind_to_element_with_id="container",
    ~initial_model=Model.init(),
  );
};
