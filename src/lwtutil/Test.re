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
        let o = create();
        let _ = subscribe'(o, ns_push(ns));

        next(o, 0);
        next(o, 1);
        next(o, 2);
        complete(o);

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
        let o = create();
        let o' = create();

        let _ = subscribe'(o, ns_push(ns));
        let _ = subscribe'(o', ns_push(ns));

        next(o, 0);
        next(o', 1);
        next(o, 2);
        next(o', 3);
        complete(o);

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
        let o = create();
        let o' = create();

        let _ = subscribe(o, o' |> next, () => complete(o'));
        let _ = subscribe'(o', ns_push(ns));

        next(o, 0);
        next(o, 1);
        next(o, 2);
        complete(o);

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
        let o = create();
        let s = subscribe(o, _ => flag := A, () => flag := B);

        next(o, 0);
        let () = unsubscribe(s);
        complete(o);

        let+ () = wait(o);
        flag^ == A;
      },
    );
};

include Lwt_observable;

let () = Test.run();
