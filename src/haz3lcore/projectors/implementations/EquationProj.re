open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open Js_of_ocaml;
open OptUtil.Syntax;

module Fresh = IdTagged.FreshGrammar;

let debug = true;
let log = msg =>
  if (debug) {
    print_endline("[EquationProj] " ++ msg);
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type equation_model = {
  math: string,
  variable: string,
  status: option(string),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type equation_action =
  | SetMath(string)
  | SetVariable(string)
  | SetStatus(option(string));

let replace = (pat, repl, s) => StringUtil.plain_replace(pat, s, repl);
let empty_math_sentinel = "__hazel_equation_empty__";
let input_id = id => Id.cls(id) ++ "-equation-input";
let is_hole_math = (math: string): bool => {
  let math = String.trim(math);
  math == "" || math == "?";
};
let store_math = (math: string): string =>
  String.trim(math) == "" ? empty_math_sentinel : math;
let load_math = (math: string): string =>
  math == empty_math_sentinel ? "" : math;

let normalize_math = (s: string): string =>
  s
  |> replace("\\\\left", "")
  |> replace("\\\\right", "")
  |> replace("\\\\cdot", "*")
  |> replace("\\\\times", "*")
  |> replace("\\\\sin", "sin")
  |> replace("\\\\cos", "cos")
  |> replace("\\\\tan", "tan")
  |> replace("\\\\exp", "exp")
  |> replace("\\\\log", "log")
  |> replace("\\{", "(")
  |> replace("\\}", ")")
  |> String.trim;

let hazel_to_math = (s: string): string =>
  s |> replace("\\*\\*", "^") |> String.trim;

let math_to_hazel = (s: string): string =>
  s |> normalize_math |> replace("\\^", "**");

let math_to_algebrite = (s: string): string =>
  s |> normalize_math |> replace("\\*\\*", "^");

let algebrite_to_hazel = (s: string): string =>
  s
  |> normalize_math
  |> replace("\\^", "**")
  |> replace("([0-9]) ([a-zA-Z_])", "$1*$2")
  |> replace("\\) \\(", ")*(");

let current_math = (model: equation_model, info: info): string =>
  if (model.math == "") {
    let backing = info.utility.seg_to_string(info.syntax) |> hazel_to_math;
    is_hole_math(backing) ? "" : backing;
  } else {
    load_math(model.math);
  };

module MiniParser = {
  exception ParseError;

  type t = {
    s: string,
    mutable i: int,
  };

  let mk = s => {
    s: s |> math_to_algebrite,
    i: 0,
  };

  let at_end = p => p.i >= String.length(p.s);
  let peek = p => at_end(p) ? None : Some(p.s.[p.i]);
  let bump = p => p.i = p.i + 1;

  let is_space =
    fun
    | ' '
    | '\n'
    | '\r'
    | '\t' => true
    | _ => false;

  let rec skip_spaces = p =>
    switch (peek(p)) {
    | Some(c) when is_space(c) =>
      bump(p);
      skip_spaces(p);
    | _ => ()
    };

  let eat = (p, c) => {
    skip_spaces(p);
    switch (peek(p)) {
    | Some(c') when c == c' =>
      bump(p);
      true;
    | _ => false
    };
  };

  let parse_number = p => {
    let start = p.i;
    let seen_dot = ref(false);
    let rec loop = () =>
      switch (peek(p)) {
      | Some('.') when ! seen_dot^ =>
        seen_dot := true;
        bump(p);
        loop();
      | Some(c) when c >= '0' && c <= '9' =>
        bump(p);
        loop();
      | _ => ()
      };
    loop();
    if (p.i == start) {
      raise(ParseError);
    };
    let raw = String.sub(p.s, start, p.i - start);
    if (seen_dot^) {
      Fresh.Exp.float(float_of_string(raw));
    } else {
      Fresh.Exp.big_int(Bigint.of_string(raw));
    };
  };

  let parse_ident = p => {
    let is_first = c =>
      c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_';
    let is_rest = c => is_first(c) || c >= '0' && c <= '9';
    let start = p.i;
    switch (peek(p)) {
    | Some(c) when is_first(c) => bump(p)
    | _ => raise(ParseError)
    };
    let rec loop = () =>
      switch (peek(p)) {
      | Some(c) when is_rest(c) =>
        bump(p);
        loop();
      | _ => ()
      };
    loop();
    String.sub(p.s, start, p.i - start);
  };

  let bin = (op, l, r) => Fresh.Exp.bin_op(Int(op), l, r);
  let starts_atom =
    fun
    | '('
    | '-' => true
    | c when c >= '0' && c <= '9' => true
    | c when c >= 'a' && c <= 'z' => true
    | c when c >= 'A' && c <= 'Z' => true
    | '_' => true
    | _ => false;

  let rec expr = p => add(p)
  and add = p => {
    let rec loop = acc => {
      skip_spaces(p);
      switch (peek(p)) {
      | Some('+') =>
        bump(p);
        loop(bin(Plus, acc, mul(p)));
      | Some('-') =>
        bump(p);
        loop(bin(Minus, acc, mul(p)));
      | _ => acc
      };
    };
    loop(mul(p));
  }
  and mul = p => {
    let rec loop = acc => {
      skip_spaces(p);
      switch (peek(p)) {
      | Some('*') =>
        bump(p);
        loop(bin(Times, acc, pow(p)));
      | Some('/') =>
        bump(p);
        loop(bin(Divide, acc, pow(p)));
      | Some(c) when starts_atom(c) => loop(bin(Times, acc, pow(p)))
      | _ => acc
      };
    };
    loop(pow(p));
  }
  and pow = p => {
    let base = atom(p);
    skip_spaces(p);
    switch (peek(p)) {
    | Some('^') =>
      bump(p);
      bin(Power, base, pow(p));
    | _ => base
    };
  }
  and atom = p => {
    skip_spaces(p);
    switch (peek(p)) {
    | Some('(') =>
      bump(p);
      let e = expr(p);
      if (!eat(p, ')')) {
        raise(ParseError);
      };
      e;
    | Some('-') =>
      bump(p);
      Fresh.Exp.un_op(Int(Minus), atom(p));
    | Some(c) when c >= '0' && c <= '9' => parse_number(p)
    | Some(_) =>
      let name = parse_ident(p);
      if (eat(p, '(')) {
        let arg = expr(p);
        if (!eat(p, ')')) {
          raise(ParseError);
        };
        Fresh.Exp.ap(Forward, Fresh.Exp.var(name), arg);
      } else {
        Fresh.Exp.var(name);
      };
    | None => raise(ParseError)
    };
  };

  let parse = s =>
    try({
      let p = mk(s);
      let e = expr(p);
      skip_spaces(p);
      if (!at_end(p)) {
        raise(ParseError);
      };
      Some(e);
    }) {
    | ParseError
    | Failure(_) => None
    };
};

let parse_exp = MiniParser.parse;

let empty_hole_segment = (info: info): Base.segment =>
  info.utility.term_to_seg(~inline=true, Exp(Fresh.Exp.empty_hole()));

let segment_of_hazel = (info: info, hazel: string): option(Base.segment) =>
  if (is_hole_math(hazel)) {
    Some(empty_hole_segment(info));
  } else {
    switch (parse_exp(hazel)) {
    | Some(exp) => Some(info.utility.term_to_seg(~inline=true, Exp(exp)))
    | None => None
    };
  };

let segment_of_math = (info: info, math: string): option(Base.segment) =>
  if (is_hole_math(math)) {
    Some(empty_hole_segment(info));
  } else {
    switch (parse_exp(math)) {
    | Some(exp) => Some(info.utility.term_to_seg(~inline=true, Exp(exp)))
    | None => None
    };
  };

let run_algebrite = (command: string, math: string): option(string) =>
  if (is_hole_math(math)) {
    None;
  } else {
    try({
      let algebrite = Js.Unsafe.global##.Algebrite;
      let input = command ++ "(" ++ math_to_algebrite(math) ++ ")";
      Some(algebrite##run(Js.string(input)) |> Js.to_string);
    }) {
    | _ => None
    };
  };

let run_derivative = (math: string, variable: string): option(string) =>
  if (is_hole_math(math)) {
    None;
  } else {
    try({
      let algebrite = Js.Unsafe.global##.Algebrite;
      let input = "d(" ++ math_to_algebrite(math) ++ "," ++ variable ++ ")";
      Some(algebrite##run(Js.string(input)) |> Js.to_string);
    }) {
    | _ => None
    };
  };

let bind_derivative =
    (info: info, math: string, variable: string)
    : option((string, Base.segment)) => {
  let* body = parse_exp(math_to_hazel(math));
  let* derivative = run_derivative(math, variable);
  let* derivative_exp = parse_exp(algebrite_to_hazel(derivative));
  let exp =
    Fresh.Exp.(
      let_(
        Fresh.Pat.var("f"),
        fn(Fresh.Pat.var(variable), body, None, None),
        let_(
          Fresh.Pat.var("f_prime"),
          fn(Fresh.Pat.var(variable), derivative_exp, None, None),
          tuple([var("f"), var("f_prime")]),
        ),
      )
    );
  Some((derivative, info.utility.term_to_seg(~inline=true, Exp(exp))));
};

let stop_keydown = _evt => Effect.Stop_propagation;

let mathlive_available = (): bool =>
  try(
    Js.Unsafe.global##.customElements##get(Js.string("math-field"))
    |> Js.Optdef.test
  ) {
  | _ => false
  };

let mathfield_value_from_element = (target, fallback: string): string =>
  try({
    let value = Js.Unsafe.meth_call(target, "getValue", [||]) |> Js.to_string;
    value;
  }) {
  | _ => fallback
  };

let mathfield_value = (evt, fallback: string): string => {
  let target = Js.Unsafe.coerce(evt)##.target;
  let value = mathfield_value_from_element(target, fallback);
  log("input math-field getValue=" ++ value ++ " fallback=" ++ fallback);
  value;
};

let read_math = (id, fallback: string): string =>
  switch (JsUtil.get_elem_by_id_opt(input_id(id))) {
  | Some(el) =>
    let target = Js.Unsafe.coerce(el);
    let value =
      if (mathlive_available()) {
        mathfield_value_from_element(target, fallback);
      } else {
        try(Js.to_string(target##.value)) {
        | _ => fallback
        };
      };
    log("read DOM math=" ++ value ++ " fallback=" ++ fallback);
    value;
  | None =>
    log("read DOM missing; fallback=" ++ fallback);
    fallback;
  };

let math_input = (id, math, local) => {
  let base_attrs = [Attr.id(input_id(id)), Attr.on_keydown(stop_keydown)];
  if (mathlive_available()) {
    (Node.create("math-field"))(
      ~attrs=
        base_attrs
        @ [
          Attr.on_input((evt, value) => {
            let math = mathfield_value(evt, value);
            local(SetMath(math));
          }),
          Attr.string_property("value", math),
          Attr.create("virtual-keyboard-mode", "manual"),
          Attr.create("smart-mode", "true"),
        ],
      [],
    );
  } else {
    Node.textarea(
      ~attrs=
        base_attrs
        @ [
          Attr.on_input((_, value) => {
            log("input textarea value=" ++ value);
            local(SetMath(value));
          }),
          Attr.string_property("value", math),
        ],
      [],
    );
  };
};

let button = (label, effect) =>
  Node.div(
    ~attrs=[
      Attr.classes(["equation-action"]),
      Attr.on_pointerdown(_ => Effect.Stop_propagation),
      Attr.on_click(_ => Effect.Many([effect(), Effect.Stop_propagation])),
    ],
    [Node.text(label)],
  );

let apply_result =
    (~info, ~local, ~parent, ~status_prefix, result: option(string))
    : Ui_effect.t(unit) =>
  switch (result) {
  | None =>
    log(status_prefix ++ " failed");
    local(SetStatus(Some(status_prefix ++ " failed")));
  | Some(math) =>
    let hazel = algebrite_to_hazel(math);
    log(status_prefix ++ " result math=" ++ math ++ " hazel=" ++ hazel);
    switch (parse_exp(math)) {
    | Some(exp) =>
      let seg = info.utility.term_to_seg(~inline=true, Exp(exp));
      let display = hazel |> hazel_to_math;
      Effect.Many([
        local(SetMath(display)),
        local(SetStatus(Some(status_prefix ++ ": " ++ hazel))),
        parent(SetSyntax(seg)),
      ]);
    | None =>
      log(status_prefix ++ " parse failed for math=" ++ math);
      Effect.Many([
        local(SetMath(math)),
        local(
          SetStatus(
            Some(status_prefix ++ " produced unsupported Hazel syntax"),
          ),
        ),
      ]);
    };
  };

module M:
  Projector with type model = equation_model and type action = equation_action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = equation_model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = equation_action;

  let init = (any: Any.t) =>
    switch (any) {
    | Exp(_) =>
      Some({
        math: "",
        variable: "x",
        status: None,
      })
    | _ => None
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (model, info) => {
    let width =
      current_math(model, info)
      |> Unicode.Width.columns_of_string
      |> max(24, _)
      |> min(58, _);
    ProjectorCore.Shape.{
      vertical: Inline,
      horizontal: width + 52,
    };
  };
  let update = (model, _, action) =>
    switch (action) {
    | SetMath(math) =>
      log("SetMath " ++ math);
      {
        ...model,
        math: store_math(math),
        status: None,
      };
    | SetVariable(variable) => {
        ...model,
        variable,
      }
    | SetStatus(status) => {
        ...model,
        status,
      }
    };
  let error = (_, _): option(ProjectorBase.error) => None;

  let view =
      ({model, info, local, parent, _}: View.args(model, action)): View.t => {
    let math = current_math(model, info);
    let current_dom_math = () => read_math(info.id, math);
    let apply = () => {
      let math = current_dom_math();
      log("Apply clicked math=" ++ math);
      switch (segment_of_math(info, math)) {
      | Some(seg) =>
        log("Apply parsed math=" ++ math);
        Effect.Many([
          local(SetMath(math |> math_to_hazel |> hazel_to_math)),
          local(SetStatus(Some("applied"))),
          parent(SetSyntax(seg)),
        ]);
      | None =>
        log("Apply parse failed math=" ++ math);
        local(SetStatus(Some("could not parse as Hazel expression")));
      };
    };
    let expand = () =>
      apply_result(
        ~info,
        ~local,
        ~parent,
        ~status_prefix="expanded",
        run_algebrite("expand", current_dom_math()),
      );
    let simplify = () =>
      apply_result(
        ~info,
        ~local,
        ~parent,
        ~status_prefix="simplified",
        run_algebrite("simplify", current_dom_math()),
      );
    let derivative = () =>
      apply_result(
        ~info,
        ~local,
        ~parent,
        ~status_prefix="derivative",
        run_derivative(current_dom_math(), model.variable),
      );
    let bind_f_prime = () => {
      let math = current_dom_math();
      switch (bind_derivative(info, math, model.variable)) {
      | Some((_derivative, seg)) =>
        log("Create f_prime replacing projector math=" ++ math);
        Effect.Many([parent(ReplaceWithSyntax(seg))]);
      | None =>
        log("Create f_prime failed math=" ++ math);
        local(SetStatus(Some("could not create f_prime binding")));
      };
    };
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["equation-inner"])],
        [
          math_input(info.id, math, local),
          Node.input(
            ~attrs=[
              Attr.classes(["equation-var"]),
              Attr.create("title", "Differentiation variable"),
              Attr.on_keydown(stop_keydown),
              Attr.on_input((_, value) => local(SetVariable(value))),
              Attr.string_property("value", model.variable),
            ],
            (),
          ),
          Node.div(
            ~attrs=[Attr.classes(["equation-actions"])],
            [
              button("Apply", apply),
              button("Expand", expand),
              button("Simplify", simplify),
              button("d/dx", derivative),
              button("Create f'", bind_f_prime),
            ],
          ),
          switch (model.status) {
          | Some(status) =>
            Node.div(
              ~attrs=[Attr.classes(["equation-status"])],
              [Node.text(status)],
            )
          | None => Node.div(~attrs=[Attr.classes(["equation-status"])], [])
          },
        ],
      ),
    );
  };
};
