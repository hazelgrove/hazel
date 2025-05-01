open Test_Statics_Prelude;

let qcheck_statics_does_not_crash =
  QCheck.Test.make(
    ~name="Statics does not crash",
    ~count=10000,
    QCheck_Util.arb_exp(~minimal_idents=true, 50),
    exp => {
    switch (statics(exp)) {
    | _m => true
    | exception Stack_overflow => true // TODO https://github.com/hazelgrove/hazel/issues/1622
    | exception (Failure(f) as e) =>
      switch (f) {
      | "Type join of ap" => true // TODO https://github.com/hazelgrove/hazel/issues/1459
      | "normalize exceeded 1000 recursive calls" => true // TODO https://github.com/hazelgrove/hazel/issues/1622?issue=hazelgrove%7Chazel%7C1623
      | "weak_head_normalize exceeded 1000 recursive calls" => true // TODO https://github.com/hazelgrove/hazel/issues/1621
      | "Recursion limit exceeded in all_ctrs_of_typ" => true // TODO https://github.com/hazelgrove/hazel/issues/1624
      | _
          when
            String.starts_with(
              ~prefix="all_ctrs_of_type called with a non-normalized type:",
              f,
            ) =>
        true // https://github.com/hazelgrove/hazel/issues/1626
      | _ => raise(e)
      }
    }
  });

let tests = [QCheck_alcotest.to_alcotest(qcheck_statics_does_not_crash)];
