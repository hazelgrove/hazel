open Tezt;
open Tezt.Base;
open Lwt.Infix;
open Lwt.Syntax;

module Lwt_observable = {
  open Lwtutil.Lwt_observable;

  let register_test = (title, tags, f) =>
    Test.register(
      ~__FILE__, ~title, ~tags=["lwtutil", "observable"] @ tags, () =>
      f()
      >>= (
        ok => ok ? unit : Test.fail("observable functionality was incorrect")
      )
    );

  let ns_empty = () => ref([]);
  let ns_push = (ns, n) => ns := [n, ...ns^];
  let ns_equal = (ns, ns') => ns^ == ns';
  /* let ns_print = ns => { */
  /* ns^ |> List.iter(n => print_string(string_of_int(n) ++ " ")); */
  /* print_newline(); */
  /* }; */

  let () =
    register_test(
      "single observable",
      [],
      () => {
        let ns = ns_empty();
        let (o, next, complete) = create();
        let _ = subscribe'(o, ns_push(ns));

        next(0);
        next(1);
        next(2);
        complete();

        let+ () = wait(o);
        ns_equal(ns, [2, 1, 0]);
      },
    );

  let () =
    register_test(
      "double observables",
      [],
      () => {
        let ns = ns_empty();
        let (o, next, complete) = create();
        let (o', next', complete') = create();

        let _ = subscribe'(o, ns_push(ns));
        let _ = subscribe'(o', ns_push(ns));

        next(0);
        next'(1);
        next(2);
        next'(3);
        complete();
        complete'();

        let+ () = wait(o);
        ns_equal(ns, [3, 2, 1, 0]);
      },
    );

  let () =
    register_test(
      "linked observables",
      [],
      () => {
        let ns = ns_empty();
        let (o, next, complete) = create();
        let (o', next', complete') = create();

        let _ = subscribe(o, next', () => complete'());
        let _ = subscribe'(o', ns_push(ns));

        next(0);
        next(1);
        next(2);
        complete();

        let+ () = wait(o');
        ns_equal(ns, [2, 1, 0]);
      },
    );

  type t_ =
    | A
    | B
    | C;
  let () =
    register_test(
      "unsubscribe observable",
      [],
      () => {
        let flag = ref(C);
        let (o, next, complete) = create();
        let s = subscribe(o, _ => flag := A, () => flag := B);

        next(0);
        let () = unsubscribe(s);
        complete();

        let+ () = wait(o);
        flag^ == A;
      },
    );

  let () =
    register_test(
      "pipe map observable",
      [],
      () => {
        let ns = ns_empty();
        let (o, next, complete) = create();
        let o' = o |> pipe(Lwt_stream.map(n => n + 1));
        let _ = subscribe'(o', ns_push(ns));

        next(0);
        next(1);
        next(2);
        complete();

        let+ () = wait(o');
        ns_equal(ns, [3, 2, 1]);
      },
    );

  let () =
    register_test(
      "pipe map observable twice",
      [],
      () => {
        let ns = ns_empty();
        let (o, next, complete) = create();
        let o' = o |> pipe(Lwt_stream.map(n => n + 1));
        let o'' = o' |> pipe(Lwt_stream.map(n => n * n));
        let _ = subscribe'(o'', ns_push(ns));

        next(0);
        next(1);
        next(2);
        complete();

        let+ () = wait(o'');
        ns_equal(ns, [9, 4, 1]);
      },
    );

  let () =
    register_test(
      "pipe filter observable",
      [],
      () => {
        let ns = ns_empty();
        let (o, next, complete) = create();
        let o' = o |> pipe(Lwt_stream.filter(n => n >= 0));
        let _ = subscribe'(o', ns_push(ns));

        next(-1);
        next(0);
        next(1);
        next(0);
        next(-1);
        complete();

        let+ () = wait(o');
        ns_equal(ns, [0, 1, 0]);
      },
    );
};

include Lwt_observable;

let () = Test.run();
