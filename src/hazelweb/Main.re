Logger.init_log();

let () = {
  // https://github.com/janestreet/incr_dom/blob/master/example/monitor
  let monitor = Async_kernel.Monitor.create(~name="My monitor", ());
  let _ = Async_kernel.Monitor.detach_and_iter_errors(monitor, ~f=_ => {()});
  // Start the main app.
  // See <https://github.com/janestreet/incr_dom/blob/master/src/start_app.mli>.
  Incr_dom.Start_app.start(
    (module Hazel),
    ~debug=false,
    ~bind_to_element_with_id="container",
    ~initial_model=Model.init(),
  );
};
