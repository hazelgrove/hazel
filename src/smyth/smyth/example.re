open Lang;

let rec from_value = v =>
  switch (v) {
  | VTuple(comps) => ExTuple(List.map(from_value, comps))

  | VCtor(name, v_arg) => ExCtor(name, from_value(v_arg))
  };

let rec res_satisfies = (hf, res, ex) =>
  switch (res, ex) {
  | (_, ExTop) => true

  | (RTuple(res_components), ExTuple(ex_components)) =>
    List.length(res_components) == List.length(ex_components)
    && List.for_all2(res_satisfies(hf), res_components, ex_components)

  | (RCtor(r_name, r_arg), ExCtor(ex_name, ex_arg)) =>
    String.equal(r_name, ex_name) && res_satisfies(hf, r_arg, ex_arg)

  | (_, ExInputOutput(input, output)) =>
    switch (Eval.resume(hf, RApp(res, RARes(Res.from_value(input))))) {
    | Ok((r_out, [])) => res_satisfies(hf, r_out, output)

    | _ => false
    }

  | _ => false
  };

let exp_satisfies = (hf, exp, worlds) =>
  List.for_all(
    ((env, ex)) =>
      switch (Eval.eval(env, exp)) {
      | Ok((r, [])) =>
        switch (Eval.resume(hf, r)) {
        | Ok((r', [])) => res_satisfies(hf, r', ex)
        | _ => false
        }

      | _ => false
      },
    worlds,
  );
