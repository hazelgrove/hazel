open Language;

type validation_error =
  | EmptyPattern
  | InvalidMetavariable(string)
  | TargetOnlyMetavariable(string)
  | UnsupportedPattern(string);

let validation_error_message =
  fun
  | EmptyPattern => "Both rewrite patterns are required."
  | InvalidMetavariable(name) => "Invalid metavariable name: $" ++ name
  | TargetOnlyMetavariable(name) =>
    "Target metavariable $" ++ name ++ " does not occur in the source."
  | UnsupportedPattern(side) =>
    side
    ++ " pattern is outside the supported arithmetic/trigonometric rewrite syntax.";

let id_prefix = "session.untrusted.";
let meta_prefix = "hazel_session_meta_";

let is_identifier_start =
  fun
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '_' => true
  | _ => false;

let is_identifier_char =
  fun
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' => true
  | _ => false;

let metavariables = pattern => {
  let length = String.length(pattern);
  let rec identifier_end = index =>
    index < length && is_identifier_char(pattern.[index])
      ? identifier_end(index + 1) : index;
  let rec loop = (index, names) =>
    if (index >= length) {
      List.rev(names);
    } else if (pattern.[index] == '$') {
      let start = index + 1;
      if (start >= length || !is_identifier_start(pattern.[start])) {
        loop(start, ["", ...names]);
      } else {
        let finish = identifier_end(start + 1);
        let name = String.sub(pattern, start, finish - start);
        loop(finish, List.mem(name, names) ? names : [name, ...names]);
      };
    } else {
      loop(index + 1, names);
    };
  loop(0, []);
};

let replace_metavariables = (pattern, names) =>
  names
  |> List.sort((left, right) =>
       Int.compare(String.length(right), String.length(left))
     )
  |> List.fold_left(
       (result, name) =>
         Str.global_replace(
           Str.regexp_string("$" ++ name),
           meta_prefix ++ name,
           result,
         ),
       pattern,
     );

let meta_name = name => {
  let prefix_length = String.length(meta_prefix);
  String.length(name) > prefix_length
  && String.sub(name, 0, prefix_length) == meta_prefix
    ? Some(
        String.sub(name, prefix_length, String.length(name) - prefix_length),
      )
    : None;
};

let rec pat_of_exp = exp => {
  let exp = TrigRewrite.strip(exp);
  switch (exp.term) {
  | Var(name) =>
    switch (meta_name(name)) {
    | Some(name) => Some(TrigRewrite.Meta(name))
    | None when name == "pi" => Some(TrigRewrite.Pi)
    | None => Some(TrigRewrite.VarName(name))
    }
  | Atom(Int(value))
  | Atom(Nat(value)) =>
    Bigint.to_int(value) |> Option.map(value => TrigRewrite.IntLit(value))
  | Atom(SInt(value)) => Some(TrigRewrite.IntLit(value))
  | Atom(Float(value)) when value == Float.round(value) =>
    Some(TrigRewrite.IntLit(int_of_float(value)))
  | Ap(Operators.Forward, fn, arg) =>
    switch (TrigRewrite.function_name(fn), pat_of_exp(arg)) {
    | (Some(name), Some(arg)) => Some(TrigRewrite.App(name, arg))
    | _ => None
    }
  | BinOp(op, left, right) =>
    let kind =
      if (TrigRewrite.op_matches(TrigRewrite.Add, op)) {
        Some(TrigRewrite.Add);
      } else if (TrigRewrite.op_matches(TrigRewrite.Sub, op)) {
        Some(TrigRewrite.Sub);
      } else if (TrigRewrite.op_matches(TrigRewrite.Mul, op)) {
        Some(TrigRewrite.Mul);
      } else if (TrigRewrite.op_matches(TrigRewrite.Div, op)) {
        Some(TrigRewrite.Div);
      } else if (TrigRewrite.op_matches(TrigRewrite.Pow, op)) {
        Some(TrigRewrite.Pow);
      } else {
        None;
      };
    switch (kind, pat_of_exp(left), pat_of_exp(right)) {
    | (Some(kind), Some(left), Some(right)) =>
      Some(TrigRewrite.Bin(kind, left, right))
    | _ => None
    };
  | UnOp(
      Operators.Int(Operators.Minus) | SInt(Minus) |
      Operators.Float(Operators.Minus),
      inner,
    ) =>
    pat_of_exp(inner) |> Option.map(inner => TrigRewrite.Neg(inner))
  | Parens(inner)
  | Asc(inner, _) => pat_of_exp(inner)
  | _ => None
  };
};

let compile_pattern = (pattern, names) =>
  replace_metavariables(pattern, names)
  |> Haz3lcore.Parser.to_term(~root=Sort.Exp)
  |> Option.bind(_, pat_of_exp);

let validate_patterns = (~source_pattern, ~target_pattern) => {
  let source_pattern = String.trim(source_pattern);
  let target_pattern = String.trim(target_pattern);
  if (source_pattern == "" || target_pattern == "") {
    Error(EmptyPattern);
  } else {
    let source_names = metavariables(source_pattern);
    let target_names = metavariables(target_pattern);
    switch (List.find_opt(name => name == "", source_names @ target_names)) {
    | Some(name) => Error(InvalidMetavariable(name))
    | None =>
      switch (
        List.find_opt(name => !List.mem(name, source_names), target_names)
      ) {
      | Some(name) => Error(TargetOnlyMetavariable(name))
      | None =>
        switch (
          compile_pattern(source_pattern, source_names),
          compile_pattern(target_pattern, source_names),
        ) {
        | (None, _) => Error(UnsupportedPattern("Source"))
        | (_, None) => Error(UnsupportedPattern("Target"))
        | (Some(_), Some(_)) => Ok(source_names)
        }
      }
    };
  };
};

let make = (~id, ~source_pattern, ~target_pattern) =>
  switch (validate_patterns(~source_pattern, ~target_pattern)) {
  | Error(_) as error => error
  | Ok(metavariables) =>
    Ok(
      Axioms.{
        id,
        label: "Untrusted session rewrite",
        source_pattern: String.trim(source_pattern),
        target_pattern: String.trim(target_pattern),
        metavariables,
        direction: BothDirections,
      },
    )
  };

let is_session_rule_id = id => String.starts_with(~prefix=id_prefix, id);

let rewrites_at_root = (definition: Axioms.session_rewrite, exp) =>
  switch (
    compile_pattern(definition.source_pattern, definition.metavariables),
    compile_pattern(definition.target_pattern, definition.metavariables),
  ) {
  | (Some(left), Some(right)) =>
    let spec: TrigRewrite.spec = {
      rule_id: definition.id,
      label: definition.label,
      left,
      right,
    };
    let apply = (before, after) =>
      TrigRewrite.apply_spec_direction(spec, before, after, exp);
    switch (definition.direction) {
    | Axioms.Forward => apply(left, right)
    | Backward => apply(right, left)
    | BothDirections => apply(left, right) @ apply(right, left)
    };
  | _ => []
  };

let transition_direction = (definition, before_exp, after_exp) =>
  rewrites_at_root(
    {
      ...definition,
      direction: Axioms.Forward,
    },
    before_exp,
  )
  |> List.exists((rewrite: TrigRewrite.rewrite) =>
       TrigRewrite.exp_same(rewrite.after_exp, after_exp)
     )
    ? Some(Axioms.Forward)
    : rewrites_at_root(
        {
          ...definition,
          direction: Axioms.Backward,
        },
        before_exp,
      )
      |> List.exists((rewrite: TrigRewrite.rewrite) =>
           TrigRewrite.exp_same(rewrite.after_exp, after_exp)
         )
        ? Some(Axioms.Backward) : None;
