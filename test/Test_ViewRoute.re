/* (^ticket(18)) the eval worker runs only the livelit views the page
   names, and the page names only its costly ones */
open Alcotest;
open Language;

let sample = (id, value) =>
  Sample.mk(
    ~step_start=0,
    ~step_end=0,
    id,
    value,
    Environment.empty,
    [],
    Sample.empty_capture_spec,
  );

let tests = [
  (
    "ViewRoute",
    [
      test_case(
        "the page names only its costly views",
        `Quick,
        () => {
          Hashtbl.reset(Haz3lcore.ViewRoute.cost);
          let (cheap, costly) = (Id.mk(), Id.mk());
          Haz3lcore.ViewRoute.note(
            cheap,
            Haz3lcore.ViewRoute.costly_ms /. 4.,
          );
          Haz3lcore.ViewRoute.note(
            costly,
            Haz3lcore.ViewRoute.costly_ms *. 2.,
          );
          let ids = Haz3lcore.ViewRoute.ids();
          check(bool, "the costly one", true, List.mem(costly, ids));
          check(bool, "not the cheap one", false, List.mem(cheap, ids));
        },
      ),
      test_case(
        "the worker runs only the views it is asked to",
        `Quick,
        () => {
          let (named, other) = (Id.mk(), Id.mk());
          let view = ViewCmdRunner.pure(Exp.fresh(EmptyHole));
          WorkerServer.current_view_ids := [named];
          let s = sample(other, view);
          check(
            bool,
            "another view: left for the page",
            true,
            WorkerServer.run_view(s) === s,
          );
          check(
            bool,
            "and not run",
            false,
            Hashtbl.mem(WorkerServer.last_view, other),
          );
          ignore(WorkerServer.run_view(sample(named, view)));
          check(
            bool,
            "the named view: run here",
            true,
            Hashtbl.mem(WorkerServer.last_view, named),
          );
          WorkerServer.current_view_ids := [];
        },
      ),
    ],
  ),
];
