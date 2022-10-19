/** Debugging utilities.

    Preferences for this module can be set in the {!Params} module. */;

let debug = func =>
  if (Params.debug_mode^) {
    func();
  } else {
    ();
  };

let println = s => debug @@ (_ => prerr_endline(s));

let print_sep = () =>
  debug @@
  (
    _ =>
      println(
        "--------------------------------------------------------------------------------",
      )
  );

let pause = () =>
  debug @@
  (
    _ => {
      let _ = read_line();
      ();
    }
  );

/*

 let print_hf hf = debug @@ fun _ ->
   hf
     |> Hole_map.bindings
     |> List.map
          ( fun (h, e) ->
              "??"
                ^ (string_of_int h)
                ^ ": "
                ^ Yojson.Safe.to_string (exp_to_yojson e)
          )
     |> String.concat "\n"
     |> println

 let print_worlds worlds = debug @@ fun _ ->
   let s =
    String.concat "\n, " @@
     List.map
       ( fun (_env, ex) ->
           (* Yojson.Safe.to_string (env_to_yojson env)
             ^ "\n    ~ "
             ^ Yojson.Safe.to_string (example_to_yojson ex) *)
           Yojson.Safe.to_string (example_to_yojson ex)
       )
       worlds
   in
     println @@ "{ " ^ s ^ "\n}"

 let print_unsolved_constraints us = debug @@ fun _ ->
   us
     |> Hole_map.bindings
     |> List.map
          ( fun (h, ws) ->
              println @@
                "??"
                  ^ (string_of_int h)
                  ^ ": ";
               print_worlds ws
          )
     |> (fun _ -> ())

 let print_exp exp = debug @@ fun _ ->
   println @@ Yojson.Safe.to_string (exp_to_yojson exp)

 let print_res res = debug @@ fun _ ->
   println @@ Yojson.Safe.to_string (res_to_yojson res)

 let print_typ typ = debug @@ fun _ ->
   println @@ Yojson.Safe.to_string (typ_to_yojson typ)

 let print_type_ctx (gamma, _) = debug @@ fun _ ->
   let s =
    String.concat "\n, " @@
     List.map
       ( fun (name, (tau, bs)) ->
           name
             ^ " : "
             ^ (Yojson.Safe.to_string @@ typ_to_yojson tau)
             ^ " { "
             ^ (Yojson.Safe.to_string @@ bind_spec_to_yojson bs)
             ^ " }"
       )
       gamma
   in
     println @@ "< " ^ s ^ "\n>"

 let print_bind_spec b = debug @@ fun _ ->
   println @@
     match b with
       | NoSpec -> "NoSpec"
       | Rec r -> "Rec(" ^ r ^ ")"
       | Arg a -> "Arg(" ^ a ^ ")"
       | Dec d -> "Dec(" ^ d ^ ")"

 */

let print_int = n => debug @@ (_ => println(string_of_int(n)));

let print_float = f => debug @@ (_ => println(Float2.to_string(f)));

let print_nondet_len = nd =>
  debug @@
  (_ => nd |> Nondet.to_list |> List.length |> string_of_int |> println);

let print_len = xs =>
  debug @@ (_ => println(string_of_int @@ List.length(xs)));
