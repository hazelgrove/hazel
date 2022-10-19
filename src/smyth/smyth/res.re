open Lang;

let rec final = r => determinate(r) || indeterminate(r)

and final_env = (env: env): bool =>
  env |> Env.all_res |> List.for_all(((_x, e)) => final(e))

and determinate = r =>
  switch (r) {
  | RFix(env, _, _, _) => final_env(env)

  | RTuple(comps) => List.for_all(final, comps)

  | RCtor(_, arg) => final(arg)

  | _ => false
  }

and indeterminate = r =>
  switch (r) {
  | RHole(env, _) => final_env(env)

  | RApp(r1, RARes(r2)) => indeterminate(r1) && final(r2)

  | RApp(r1, RAType(_)) => indeterminate(r1)

  | RProj(_, _, arg) => indeterminate(arg)

  | RCase(env, scrutinee, _) => final_env(env) && indeterminate(scrutinee)

  | _ => false
  };

let rec to_value = r =>
  switch (r) {
  | RTuple(comps) =>
    comps
    |> List.map(to_value)
    |> Option2.sequence
    |> Option2.map(vcomps => VTuple(vcomps))

  | RCtor(name, arg) => Option2.map(v => VCtor(name, v), to_value(arg))

  | _ => None
  };

let rec from_value = v =>
  switch (v) {
  | VTuple(comps) => RTuple(List.map(from_value, comps))

  | VCtor(name, v_arg) => RCtor(name, from_value(v_arg))
  };

let rec consistent = (r1, r2) =>
  if (r1 == r2) {
    Some([]);
  } else {
    switch (r1, r2) {
    | (RTuple(comps1), RTuple(comps2)) =>
      if (List.length(comps1) != List.length(comps2)) {
        None;
      } else {
        List.map2(consistent, comps1, comps2)
        |> Option2.sequence
        |> Option2.map(List.concat);
      }

    | (RCtor(name1, arg1), RCtor(name2, arg2)) =>
      if (String.equal(name1, name2)) {
        consistent(arg1, arg2);
      } else {
        None;
      }

    | _ =>
      switch (to_value(r1)) {
      | Some(v1) => Some([(r2, v1)])

      | None =>
        switch (to_value(r2)) {
        | Some(v2) => Some([(r1, v2)])

        | None => None
        }
      }
    };
  };
