type error =
  | ParseError of (Parse.context, Parse.problem) Bark.dead_end list
  | TypeError of (Lang.exp * Type.error)
  | EvalError of string
  | TimedOut of float
  | NoSolutions
  | PartialNotSubsetFull

type 'a response = ('a, error) result

(* Program Helpers *)

let parse_program : string -> Desugar.program response =
  Bark.run Parse.program >> Result.map_error (fun e -> ParseError e)

(* Solve *)

type solve_result =
  {hole_fillings: (Lang.hole_name * Lang.exp) list list; time_taken: float}

type solve_result_with_constraints =
  { hole_fillings: (Lang.hole_name * Lang.exp) list list
  ; time_taken: float
  ; constraints: Lang.output_constraints }

let synthesis_pipeline delta sigma assertions =
  assertions
  |> Uneval.simplify_assertions delta sigma
  |> Solve.solve_any delta sigma

let solve_program : Desugar.program -> solve_result response =
 fun program ->
  let exp, sigma = Desugar.program program in
  match Type.check sigma Type_ctx.empty exp (Lang.TTuple []) with
  | Error e -> Error (TypeError e)
  | Ok delta -> (
    match Eval.eval Env.empty exp with
    | Error e -> Error (EvalError e)
    | Ok (_, assertions) ->
        let () = Term_gen.clear_cache () in
        let () =
          delta |> List.map fst |> List2.maximum |> Option2.with_default 0
          |> Fresh.set_largest_hole
        in
        let () = Uneval.minimal_uneval := true in
        let synthesis_result, time_taken, timed_out =
          let minimal_synthesis_result, minimal_time_taken, minimal_timed_out
              =
            Timer.itimer_timeout "minimal_synthesis_result"
              !Params.max_total_time
              (synthesis_pipeline delta sigma)
              assertions Nondet.none
          in
          if
            minimal_timed_out
            || not (Nondet.is_empty minimal_synthesis_result)
          then
            (minimal_synthesis_result, minimal_time_taken, minimal_timed_out)
          else
            let ( non_minimal_synthesis_result
                , non_minimal_time_taken
                , non_minimal_timed_out ) =
              let () = Uneval.minimal_uneval := false in
              Timer.itimer_timeout "synthesis_result"
                (!Params.max_total_time -. minimal_time_taken)
                (synthesis_pipeline delta sigma)
                assertions Nondet.none
            in
            ( non_minimal_synthesis_result
            , minimal_time_taken +. non_minimal_time_taken
            , non_minimal_timed_out )
        in
        if timed_out then Error (TimedOut time_taken)
        else
          Ok
            { hole_fillings=
                synthesis_result
                |> Nondet.map (fst >> Clean.clean delta)
                |> Nondet.collapse_option |> Nondet.to_list
            ; time_taken } )

let solve ~sketch = sketch |> parse_program |> Result2.and_then solve_program

let synthesis_pipeline_hole holes delta sigma assertions =
  (*let h = match holes with [] -> 0 | x :: _ -> x in*)
  assertions
  |> Uneval.simplify_assertions delta sigma
  |> Solve.solve_once holes delta sigma

let solve_program_hole (program : Desugar.program)
    (holes : Lang.hole_name list) : solve_result_with_constraints response =
  let exp, sigma = Desugar.program program in
  match Type.check sigma Type_ctx.empty exp (Lang.TTuple []) with
  | Error e -> Error (TypeError e)
  | Ok delta -> (
    match Eval.eval Env.empty exp with
    | Error e -> Error (EvalError e)
    | Ok (_, assertions) ->
        let () = Term_gen.clear_cache () in
        let () =
          delta |> List.map fst |> List2.maximum |> Option2.with_default 0
          |> Fresh.set_largest_hole
        in
        let () = Uneval.minimal_uneval := true in
        let synthesis_result, time_taken, timed_out =
          let minimal_synthesis_result, minimal_time_taken, minimal_timed_out
              =
            Timer.itimer_timeout "minimal_synthesis_result"
              !Params.max_total_time
              (synthesis_pipeline_hole holes delta sigma)
              assertions Nondet.none
          in
          if
            minimal_timed_out
            || not (Nondet.is_empty minimal_synthesis_result)
          then
            (minimal_synthesis_result, minimal_time_taken, minimal_timed_out)
          else
            let ( non_minimal_synthesis_result
                , non_minimal_time_taken
                , non_minimal_timed_out ) =
              let () = Uneval.minimal_uneval := false in
              Timer.itimer_timeout "synthesis_result"
                (!Params.max_total_time -. minimal_time_taken)
                (synthesis_pipeline_hole holes delta sigma)
                assertions Nondet.none
            in
            ( non_minimal_synthesis_result
            , minimal_time_taken +. non_minimal_time_taken
            , non_minimal_timed_out )
        in
        if timed_out then Error (TimedOut time_taken)
        else
          Ok
            { hole_fillings=
                synthesis_result
                |> Nondet.map ((fun (a, _, _) -> a) >> Clean.clean delta)
                |> Nondet.collapse_option |> Nondet.to_list
            ; time_taken
            ; constraints=
                synthesis_result
                |> Nondet.map (fun (_, _, c) -> c)
                |> Nondet.to_list } )

(* Test *)

type test_result =
  { specification_assertion_count: int
  ; assertion_count: int
  ; top_success: bool
  ; top_recursive_success: bool
  ; time_taken: float }

let check :
    Desugar.program -> (Lang.hole_name * Lang.exp) list -> bool response =
 fun program hole_filling ->
  let exp_with_holes, sigma = Desugar.program program in
  let exp =
    List.fold_left
      (fun acc binding -> Exp.fill_hole binding acc)
      exp_with_holes hole_filling
  in
  match Type.check sigma Type_ctx.empty exp (Lang.TTuple []) with
  | Error e -> Error (TypeError e)
  | Ok _ -> (
    match Eval.eval Env.empty exp with
    | Error _ -> Ok false
    | Ok (_, assertions) -> Ok (List2.is_empty assertions) )

let test_assertions ~specification ~sketch ~assertions =
  let open Desugar in
  let open Result2.Syntax in
  let* full_assertions =
    specification |> parse_program
    |> Result.map (fun prog -> prog.assertions)
  in
  let* sketch_program = parse_program sketch in
  let* {hole_fillings; time_taken} =
    solve_program {sketch_program with assertions}
  in
  let ranked_hole_fillings = Rank.sort hole_fillings in
  let* top_success =
    ranked_hole_fillings |> List2.hd_opt
    |> Option.to_result ~none:NoSolutions
    |> Result2.and_then
         (check {sketch_program with assertions= full_assertions})
  in
  let+ top_recursive_success =
    ranked_hole_fillings |> Rank.first_recursive
    |> Option.map (check {sketch_program with assertions= full_assertions})
    |> Option2.with_default (Ok false)
  in
  { time_taken
  ; specification_assertion_count= List.length full_assertions
  ; assertion_count= List.length assertions
  ; top_success
  ; top_recursive_success }

let test ~specification ~sketch ~examples =
  let open Desugar in
  examples |> parse_program
  |> Result.map (fun prog -> prog.assertions)
  |> Result2.and_then (fun a ->
         test_assertions ~specification ~sketch ~assertions:a)

(* Assertion Info *)

let extract_arg_list : Lang.exp -> Lang.exp list =
  let rec helper acc = function
    | Lang.EApp (_, head, Lang.EAExp arg) -> helper (arg :: acc) head
    | _ -> acc
  in
  helper []

let assertion_info ~specification ~assertions =
  let open Desugar in
  let open Result2.Syntax in
  let* full_assertions =
    specification |> parse_program
    |> Result.map (fun prog -> prog.assertions)
  in
  let* partial_assertions =
    assertions |> parse_program |> Result.map (fun prog -> prog.assertions)
  in
  let+ _ =
    Result2.guard PartialNotSubsetFull
      (List.for_all
         (fun io -> List.find_opt (( = ) io) full_assertions <> None)
         partial_assertions)
  in
  List.map
    (fun (input, output) ->
      ( List.find_opt (( = ) (input, output)) partial_assertions <> None
      , extract_arg_list (Post_parse.exp input)
      , Post_parse.exp output ))
    full_assertions
