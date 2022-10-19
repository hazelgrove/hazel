type error =
  | ParseError(list(Bark.dead_end(Parse.context, Parse.problem)))
  | TypeError((Lang.exp, Type.error))
  | EvalError(string)
  | TimedOut(float)
  | NoSolutions
  | PartialNotSubsetFull;

type response('a) = result('a, error);

/* Program Helpers */

let parse_program: string => response(Desugar.program) = (
  Bark.run(Parse.program) >> Result.map_error(e => ParseError(e)):
    string => response(Desugar.program)
);

/* Solve */

type solve_result = {
  hole_fillings: list(list((Lang.hole_name, Lang.exp))),
  time_taken: float,
};

let synthesis_pipeline = (delta, sigma, assertions) =>
  assertions
  |> Uneval.simplify_assertions(delta, sigma)
  |> Solve.solve_any(delta, sigma);

let solve_program: Desugar.program => response(solve_result) = (
  program => {
    let (exp, sigma) = Desugar.program(program);

    switch (Type.check(sigma, Type_ctx.empty, exp, Lang.TTuple([]))) {
    | Error(e) => Error(TypeError(e))
    | Ok(delta) =>
      switch (Eval.eval(Env.empty, exp)) {
      | Error(e) => Error(EvalError(e))

      | Ok((_, assertions)) =>
        let () = Term_gen.clear_cache();

        let () =
          delta
          |> List.map(fst)
          |> List2.maximum
          |> Option2.with_default(0)
          |> Fresh.set_largest_hole;

        let () = Uneval.minimal_uneval := true;

        let (synthesis_result, time_taken, timed_out) = {
          let (
            minimal_synthesis_result,
            minimal_time_taken,
            minimal_timed_out,
          ) =
            Timer.itimer_timeout(
              "minimal_synthesis_result",
              Params.max_total_time^,
              synthesis_pipeline(delta, sigma),
              assertions,
              Nondet.none,
            );

          if (minimal_timed_out || !Nondet.is_empty(minimal_synthesis_result)) {
            (minimal_synthesis_result, minimal_time_taken, minimal_timed_out);
          } else {
            let (
              non_minimal_synthesis_result,
              non_minimal_time_taken,
              non_minimal_timed_out,
            ) = {
              let () = Uneval.minimal_uneval := false;

              Timer.itimer_timeout(
                "synthesis_result",
                Params.max_total_time^ -. minimal_time_taken,
                synthesis_pipeline(delta, sigma),
                assertions,
                Nondet.none,
              );
            };

            (
              non_minimal_synthesis_result,
              minimal_time_taken +. non_minimal_time_taken,
              non_minimal_timed_out,
            );
          };
        };

        if (timed_out) {
          Error(TimedOut(time_taken));
        } else {
          Ok({
            hole_fillings:
              synthesis_result
              |> Nondet.map(fst >> Clean.clean(delta))
              |> Nondet.collapse_option
              |> Nondet.to_list,
            time_taken,
          });
        };
      }
    };
  }:
    Desugar.program => response(solve_result)
);

let solve = (~sketch) =>
  sketch |> parse_program |> Result2.and_then(solve_program);

/* Test */

type test_result = {
  specification_assertion_count: int,
  assertion_count: int,
  top_success: bool,
  top_recursive_success: bool,
  time_taken: float,
};

let check:
  (Desugar.program, list((Lang.hole_name, Lang.exp))) => response(bool) = (
  (program, hole_filling) => {
    let (exp_with_holes, sigma) = Desugar.program(program);

    let exp =
      List.fold_left(
        (acc, binding) => Exp.fill_hole(binding, acc),
        exp_with_holes,
        hole_filling,
      );

    switch (Type.check(sigma, Type_ctx.empty, exp, Lang.TTuple([]))) {
    | Error(e) => Error(TypeError(e))

    | Ok(_) =>
      switch (Eval.eval(Env.empty, exp)) {
      | Error(_) => Ok(false)
      | Ok((_, assertions)) => Ok(List2.is_empty(assertions))
      }
    };
  }:
    (Desugar.program, list((Lang.hole_name, Lang.exp))) => response(bool)
);

let test_assertions = (~specification, ~sketch, ~assertions) => {
  open Desugar;
  open Result2.Syntax;
  let* full_assertions =
    specification |> parse_program |> Result.map(prog => prog.assertions);
  let* sketch_program = parse_program(sketch);
  let* {hole_fillings, time_taken} =
    solve_program({...sketch_program, assertions});
  let ranked_hole_fillings = Rank.sort(hole_fillings);

  let* top_success =
    ranked_hole_fillings
    |> List2.hd_opt
    |> Option.to_result(~none=NoSolutions)
    |> Result2.and_then(
         check({...sketch_program, assertions: full_assertions}),
       );
  let+ top_recursive_success =
    ranked_hole_fillings
    |> Rank.first_recursive
    |> Option.map(check({...sketch_program, assertions: full_assertions}))
    |> Option2.with_default(Ok(false));
  {
    time_taken,
    specification_assertion_count: List.length(full_assertions),
    assertion_count: List.length(assertions),
    top_success,
    top_recursive_success,
  };
};

let test = (~specification, ~sketch, ~examples) =>
  Desugar.(
    examples
    |> parse_program
    |> Result.map(prog => prog.assertions)
    |> Result2.and_then(a =>
         test_assertions(~specification, ~sketch, ~assertions=a)
       )
  );

/* Assertion Info */

let extract_arg_list: Lang.exp => list(Lang.exp) = (
  {
    let rec helper = acc =>
      fun
      | Lang.EApp(_, head, Lang.EAExp(arg)) => helper([arg, ...acc], head)

      | _ => acc;

    helper([]);
  }:
    Lang.exp => list(Lang.exp)
);

let assertion_info = (~specification, ~assertions) => {
  open Desugar;
  open Result2.Syntax;
  let* full_assertions =
    specification |> parse_program |> Result.map(prog => prog.assertions);
  let* partial_assertions =
    assertions |> parse_program |> Result.map(prog => prog.assertions);
  let+ _ =
    Result2.guard(
      PartialNotSubsetFull,
      List.for_all(
        io => List.find_opt((==)(io), full_assertions) != None,
        partial_assertions,
      ),
    );
  List.map(
    ((input, output)) =>
      (
        List.find_opt((==)((input, output)), partial_assertions) != None,
        extract_arg_list(Post_parse.exp(input)),
        Post_parse.exp(output),
      ),
    full_assertions,
  );
};
