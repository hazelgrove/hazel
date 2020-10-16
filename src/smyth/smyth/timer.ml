let now () = Unix.gettimeofday ()

module Single = struct
  type t = Total | Eval

  let total, eval = (ref 0.0, ref 0.0)

  let info timer =
    match timer with
    | Total -> (total, !Params.max_total_time)
    | Eval -> (eval, !Params.max_eval_time)

  let start timer =
    let initial, _ = info timer in
    initial := now ()

  let elapsed timer =
    let initial, _ = info timer in
    now () -. !initial

  let check timer =
    let _, cutoff = info timer in
    elapsed timer < cutoff
end

module Multi = struct
  type t = Guess

  let guess = ref 0.0

  let info timer = match timer with Guess -> (guess, !Params.max_guess_time)

  let reset timer =
    let time_taken, _ = info timer in
    time_taken := 0.0

  let accumulate timer computation =
    let initial_time = now () in
    let output = computation () in
    let final_time = now () in
    let time_taken, _ = info timer in
    time_taken := !time_taken +. (final_time -. initial_time) ;
    output

  let check timer =
    let time_taken, max_time = info timer in
    !time_taken < max_time
end

exception Timeout of string

(* Does NOT nest!!! Does NOT work with JavaScript!!! *)
let itimer_timeout unique_id cutoff f arg default_value =
  if Compilation2.is_js then (f arg, 0.0, false)
  else if cutoff <= 0.0 then (default_value, 0.0, true)
  else (
    Sys.set_signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise (Timeout unique_id))) ;
    ignore
      (Unix.setitimer Unix.ITIMER_REAL
         Unix.{it_value= cutoff; it_interval= 0.0}) ;
    try
      let res = f arg in
      let time_elapsed = cutoff -. Unix.((getitimer ITIMER_REAL).it_value) in
      (res, time_elapsed, false)
    with exc ->
      if exc = Timeout unique_id then
        let time_elapsed =
          cutoff -. Unix.((getitimer ITIMER_REAL).it_value)
        in
        (default_value, time_elapsed, true)
      else raise exc )
