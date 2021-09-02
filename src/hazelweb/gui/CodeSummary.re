open Virtual_dom.Vdom;

/* TODO: Hannah - Pick 7 or so distinct colors from the different color generator thing (HSLuv)
   Make sure distinguishable for color blind */
let child_colors = [
  "rgb(122, 153, 182)",
  "rgb(199, 141, 146)",
  "rgb(153, 199, 141)",
  "rgb(235, 164, 84)",
  "rgb(167, 84, 235)",
  "rgb(235, 200, 84)",
];
let highlight = (shortened_msg: string, child_index: int): Node.t => {
  Node.span(
    [
      Attr.style(
        Css_gen.(
          create(
            ~field="background",
            ~value=
              List.nth(
                child_colors,
                child_index mod List.length(child_colors),
              ),
          )
        ),
      ),
    ],
    [Node.text(shortened_msg)],
  );
};

let _max_elems = 7;
let int_to_word_number = (n: int): string => {
  switch (n) {
  | 1 => "first"
  | 2 => "second"
  | 3 => "third"
  | 4 => "fourth"
  | 5 => "fifth"
  | 6 => "sixth"
  | 7 => "seventh"
  | _ => ""
  };
};
let comma_separated_list = (items: list(string)): string => {
  let _ = List.map(item => print_endline(item), items);
  let length = List.length(items);
  let items =
    List.mapi(
      (index, item) => {
        let separator =
          if (index == length - 1) {
            length > 2 ? ", and" : " and";
          } else if (index == 0) {
            "";
          } else {
            ",";
          };
        separator ++ " " ++ item;
      },
      items,
    );
  List.fold_left((acc, item) => acc ++ item, "", items);
};

/*
 Markdown like thing:
 highlighty thing : [thing to highlight](color index)
 bulleted list: - list item
                - list item
 */
let build_msg = (text: string): list(Node.t) => {
  let parse_line = (line: string): list(Node.t) => {
    let regex = Re.Str.regexp({|\[\|\]\|(\|)|});
    let pieces = Re.Str.full_split(regex, line);
    let rec parse_line' =
            (split_results: list(Re.Str.split_result)): list(Node.t) => {
      switch (split_results) {
      | [] => []
      | [x, ...xs] =>
        switch (x) {
        | Delim("[") =>
          switch (xs) {
          | [
              Text(msg),
              Delim("]"),
              Delim("("),
              Text(index),
              Delim(")"),
              ...xs,
            ] => [
              highlight(msg, int_of_string(index)),
              ...parse_line'(xs),
            ]
          | _ => [Node.text("["), ...parse_line'(xs)]
          }
        | Delim(t)
        | Text(t) => [Node.text(t), ...parse_line'(xs)]
        }
      };
    };
    /*let _ =
      List.mapi(
        (i, item) => {
          switch (item) {
          | Re.Str.Text(t) =>
            print_endline(string_of_int(i) ++ " Text: " ++ t)
          | Delim(d) => print_endline(string_of_int(i) ++ " Delim: " ++ d)
          }
        },
        pieces,
      );*/
    parse_line'(pieces);
  };
  let pieces = Re.Str.split(Re.Str.regexp("\n"), text);
  let rec parse = (lines: list(string)): list(Node.t) => {
    switch (lines) {
    | [] => []
    | [line, ...lines] =>
      let trim = String.trim(line);
      if (String.length(trim) > 0 && trim.[0] == '-') {
        let rec parse_bullets =
                (lines: list(string)): (list(Node.t), list(string)) => {
          switch (lines) {
          | [] => ([], [])
          | [line, ...lines] =>
            let trim = String.trim(line);
            if (String.length(trim) > 0 && trim.[0] == '-') {
              let trim = String.sub(trim, 1, String.length(trim) - 1);
              let (tail_bullets, lines) = parse_bullets(lines);
              ([Node.li([], parse_line(trim)), ...tail_bullets], lines);
            } else {
              ([], [line, ...lines]);
            };
          };
        };
        let (bullets, lines) = parse_bullets([line, ...lines]);
        [Node.ol([], bullets), ...parse(lines)];
      } else {
        print_endline("Not a bullet: " ++ trim);
        parse_line(trim) @ parse(lines);
      };
    };
  };
  /*let _ =
    List.mapi(
      (i, item) => {
        print_endline("Line " ++ string_of_int(i) ++ ": " ++ item)
      },
      pieces,
    );*/
  parse(pieces);
};

let let_line_msg =
    (pattern_info: ExplanationInfo.pattern_info, def: UHExp.t, body: UHExp.t)
    : list(Node.t) => {
  let _ = def;
  let _ = body;
  switch (pattern_info) {
  | Operand(operand, _type) =>
    switch (operand) {
    | EmptyHole(n) =>
      build_msg(
        "Bind the [definition](1) to the pattern that fills [hole "
        ++ string_of_int(n)
        ++ "](0) and evaluate the [body](2)",
      )
    | Wild(_) =>
      build_msg(
        "Evaluate the [definition](1), [throw away the result](0), and then evaluate the [body](2)",
      )
    | TypeAnn(_) =>
      failwith("Pattern info should handle type annotations directly")
    | InvalidText(_, text) =>
      build_msg(
        "[Invalid text "
        ++ text
        ++ "](0) will stand for the [definition](1) in the [body](2) when the invalid text is corrected",
      )
    | Var(_, _, var) =>
      build_msg(
        "[Variable "
        ++ var
        ++ "](0) will stand for the [definition](1) in the [body](2)",
      )
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | ListNil(_)
    | Inj(_, _, _) =>
      build_msg(
        "Evaluate the [definition](1), [throw away the result](0), and then evaluate the [body](2)",
      )
    | Parenthesized(_) =>
      failwith("Pattern info should handle parentheses directly")
    }
  | CommaOperator(pats, _type) =>
    let num_pats = List.length(pats);
    let pattern_parts =
      List.mapi(
        (index, _) => {
          let word_num = int_to_word_number(index + 1);
          "the ["
          ++ word_num
          ++ " pattern]("
          ++ string_of_int(index)
          ++ ") will stand for the "
          ++ word_num
          ++ " element";
        },
        pats,
      );
    let pattern_msg = comma_separated_list(pattern_parts);
    build_msg(
      "In the [body]("
      ++ string_of_int(num_pats + 1)
      ++ "), "
      ++ pattern_msg
      ++ " of the [definition tuple]("
      ++ string_of_int(num_pats)
      ++ ")",
    );
  | BinOperator(operator, _lpat, _rpat, _type) =>
    switch (operator) {
    | Comma => failwith("Pattern info should handle commas directly")
    | Cons =>
      build_msg(
        "In the [body](3), the [head pattern](0) will stand for the head and the [tail pattern](1) will stand for the tail of the [definition list](2)",
      )
    | Space =>
      build_msg(
        "Bind the [definition](1) to the pattern that corrects the [invalid pattern](0) and evaluate the [body](2) (function application pattern is not valid)",
      )
    }
  };
};
/* TODO: Hannah - don't highlight terms when on the base operands (like an empty hole where it is highlighted as the current term anyway)*/
let lambda_msg =
    (pattern_info: ExplanationInfo.pattern_info, body: UHExp.t): list(Node.t) => {
  let _ = body;
  let begin_msg = "Function literal that returns the value of the [body](1) when applied to an argument ";
  switch (pattern_info) {
  | Operand(operand, _type) =>
    switch (operand) {
    | EmptyHole(n) =>
      build_msg(
        begin_msg
        ++ "when [hole "
        ++ string_of_int(n)
        ++ "](0) is filled with a valid pattern",
      )
    | Wild(_) => build_msg(begin_msg ++ "that is [ignored](0)")
    | TypeAnn(_) =>
      failwith("Pattern info should handle type annotations directly")
    | InvalidText(_, text) =>
      build_msg(
        begin_msg ++ " when the [invalid text " ++ text ++ "](0) is corrected",
      )
    | Var(_, _, var) => build_msg(begin_msg ++ "[ " ++ var ++ "](0)")
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | ListNil(_)
    | Inj(_, _, _) => build_msg(begin_msg ++ "that is [ignored](0)")
    | Parenthesized(_) =>
      failwith("Pattern info should handle parentheses directly")
    }
  | CommaOperator(_pats, _type) => build_msg(begin_msg ++ " [tuple](0)")
  | BinOperator(operator, _lpat, _rpat, _type) =>
    switch (operator) {
    | Comma => failwith("Pattern info should handle commas directly")
    | Cons => build_msg(begin_msg ++ "list with [head](0) and [tail](1)") /* TODO: Hannah - the indexing when there are subpatterns gets thrown off from the tree structure of the program - this happens other places too in this code */
    | Space =>
      build_msg(
        begin_msg
        ++ "when the [pattern](0) is corrected (function application of a pattern is not valid)",
      )
    }
  };
};

let rule_msg =
    (
      index: int,
      scrut: UHExp.t,
      pattern_info: ExplanationInfo.pattern_info,
      clause: UHExp.t,
    )
    : list(Node.t) => {
  let is_first_rule = index == 0;
  let if_scrut_msg = "If the [scrutinee](0) ";
  let not_matched_msg = "has not matched any of the proceeding rules";
  let _ = scrut;
  let _ = clause;
  switch (pattern_info) {
  | Operand(operand, _type) =>
    switch (operand) {
    | EmptyHole(n) =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atching on this rule is delayed until [hole "
        ++ string_of_int(n)
        ++ "](1) is filled with a valid pattern",
      );
    | Wild(_) =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atch on the [wildcard pattern](1) and evaluate the [clause](2)",
      );
    | TypeAnn(_) =>
      failwith("Pattern info should handle type annotations directly")
    | InvalidText(_, text) =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atching on this rule is delayed until [invalid text "
        ++ text
        ++ "](1) is corrected",
      );
    | Var(_, _, var) =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atch on this rule, where [variable "
        ++ var
        ++ "](1) will stand for the [scrutinee](0) in the [clause](2)",
      );
    | IntLit(_, lit)
    | FloatLit(_, lit) =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
      build_msg(
        begin_msg
        ++ "matches the [pattern "
        ++ lit
        ++ "](1), evaluate the [clause](2)",
      );
    | BoolLit(_, lit) =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
      build_msg(
        begin_msg
        ++ "matches the [pattern "
        ++ string_of_bool(lit)
        ++ "](1), evaluate the [clause](2)",
      );
    | ListNil(_) =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
      build_msg(
        begin_msg
        ++ "matches the [empty list pattern []](1), evaluate the [clause](2)",
      );
    | Inj(_, side, _arg) =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
      let side_str =
        switch (side) {
        | L => "left"
        | R => "right"
        };
      build_msg(
        begin_msg
        ++ "matches the ["
        ++ side_str
        ++ " injection](1) of the [argument pattern](2), evaluate the [clause](3)",
      );
    | Parenthesized(_) =>
      failwith("Pattern info should handle parentheses directly")
    }
  | CommaOperator(_pats, _type) =>
    let begin_msg =
      if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
    build_msg(
      begin_msg ++ "matches the [tuple pattern](1), evaluate the [clause](2)",
    );
  | BinOperator(operator, _lpat, _rpat, _type) =>
    switch (operator) {
    | Comma => failwith("Pattern info should handle commas directly")
    | Cons =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
      build_msg(
        begin_msg
        ++ "matches the list pattern with [head](1) and [tail](2), evaluate the [clause](3)",
      );
    | Space =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atching on this rule is delayed until [the pattern](1) is corrected (function application of a pattern is not valid)",
      );
    }
  };
};

let pattern_msg = (pattern_info: ExplanationInfo.pattern_info): list(Node.t) => {
  switch (pattern_info) {
  | Operand(operand, _type) =>
    switch (operand) {
    | EmptyHole(n) =>
      build_msg(
        "Empty [pattern hole](0) with id "
        ++ string_of_int(n + 1)
        ++ ". Matching on this pattern is delayed until the hole is filled",
      )
    | Wild(_) =>
      build_msg(
        "[Wildcard](0). Matches any value without binding it to a variable",
      )
    | TypeAnn(_) =>
      /* TODO: Hannah - check this case */
      build_msg(
        "[Type annotated pattern](0). Matches values of type ty that match the annotated pattern",
      )
    | InvalidText(_, text) =>
      build_msg(
        "[Invalid text "
        ++ text
        ++ "](0) is not a valid name or literal pattern",
      )
    | Var(_, _, var) =>
      build_msg(
        "[Variable pattern](0). Mathces any valud and binds it to the variable "
        ++ var,
      )
    | IntLit(_, n) =>
      build_msg(
        "[Integer literal pattern " ++ n ++ "](0). Matches the value " ++ n,
      )
    | FloatLit(_, n) =>
      build_msg(
        "[Floating point literal pattern "
        ++ n
        ++ "](0). Matches the value "
        ++ n,
      )
    | BoolLit(_, b) =>
      build_msg(
        "[Boolean literal pattern "
        ++ string_of_bool(b)
        ++ "](0). Matches the value "
        ++ string_of_bool(b),
      )
    | ListNil(_) =>
      build_msg(
        "[Empty list (nil) pattern](0). Matches the empty list value []",
      )
    | Inj(_, side, _) =>
      let (cap, low) =
        switch (side) {
        | L => ("Left", "left")
        | R => ("Right", "right")
        };
      build_msg(
        "["
        ++ cap
        ++ " injection pattern with argument pattern](0). Matches any "
        ++ low
        ++ " injection value where the arguent matches argument pattern",
      );
    | Parenthesized(_) =>
      failwith("Pattern info should handle parentheses directly")
    }
  | CommaOperator(pats, _type) =>
    let n = string_of_int(List.length(pats) + 1);
    build_msg(
      "["
      ++ n
      ++ "-tuple pattern](0). Matches any "
      ++ n
      ++ "-tuple value where the elemets match in order the corresponding element patterns",
    );
  | BinOperator(operator, _lpat, _rpat, _type) =>
    switch (operator) {
    | Comma => failwith("Pattern info should handle commas directly")
    | Cons =>
      build_msg(
        "Non-empty list (cons) pattern. Matches any list value with head matching [head pattern](0) and tail matching [tail pattern](1)",
      )
    | Space =>
      build_msg(
        "[Function application](0) is not a vlaid pattern. No values match this pattern",
      )
    }
  };
};
/* TODO: Hannah - Display the types */
let type_msg = (type_info: ExplanationInfo.type_info): list(Node.t) => {
  switch (type_info) {
  | Operand(op) =>
    switch (op) {
    | Hole =>
      build_msg(
        "[Type hole (unknown type)](0), which is consistent with any type of expression",
      )
    | Unit =>
      build_msg(
        "[Unit type](0), which classifies expressions that evaluate to the trivial value ()",
      )
    | Int =>
      build_msg(
        "[Integer type](0), which classifies expressions that evaluate to integer values",
      )
    | Float =>
      build_msg(
        "[Floating point type](0), which classifies expressions that evaluate to floating point values",
      )
    | Bool =>
      build_msg(
        "[Boolean type](0), which classifies expressions that evaluate to true or false",
      )
    | Parenthesized(_) =>
      failwith("Type info should handle parentheses directly")
    | List(_) =>
      build_msg(
        "List type with [element type](0), which classifies expressions that evaluate to list values with elements of the element type",
      )
    }
  | CommaOperator(typs) =>
    let n = string_of_int(List.length(typs) + 1);
    build_msg(
      n
      ++ "-tuple type with element types, which classifies expressions that evaluate to tuple values with elements of the element types (in order)",
    );
  | BinOperator(op, _, _) =>
    switch (op) {
    | Arrow =>
      build_msg(
        "Arrow type with [argument type](0) and [return type](1), which classifies expressions that evaluate to function values that take arguments of the argument type and return values of the return type",
      )
    | Prod => failwith("Type info should handle products directly")
    | Sum =>
      build_msg(
        "Sum type of [left summand type](0) and [right summand type](1), which classifies expressions that evaluate to either left injection values (Inj[L](v)) with argument v of the left summand type or right injection values (Inj[R](v)) with argument v of the right summand type",
      )
    }
  };
};

let summary_msg =
    (explanation_info: ExplanationInfo.explanation_info): list(Node.t) => {
  switch (explanation_info) {
  | EmptyLine => [Node.text("Empty line")]
  | CommentLine => [Node.text("Comment")]
  | LetLine(pattern_info, def, body) => let_line_msg(pattern_info, def, body)
  | ExpBaseOperand(operand) =>
    switch (operand) {
    /* TODO: Hannah - should these really be highlighted when they these simple operands? */
    | EmptyHole(n) =>
      build_msg(
        "[Empty expression hole](0) with id " ++ string_of_int(n + 1),
      )
    | InvalidText(_, t) =>
      build_msg(
        "[Invalid text "
        ++ t
        ++ "](0) is not a valid name, keyword, or literal",
      )
    | Var(_, _, v) => build_msg("[Variable " ++ v ++ "](0)")
    | IntLit(_, n) => build_msg("[Integer literal " ++ n ++ "](0)")
    | FloatLit(_, n) => build_msg("[Floating point literal " ++ n ++ "](0)")
    | BoolLit(_, b) =>
      build_msg("[Boolean literal " ++ string_of_bool(b) ++ "](0)")
    | ListNil(_) => build_msg("[Empty list []](0)") /*TODO: Hannah - the way the parser is implemented now this doesn't work to do highlighting - maybe I need to escape brackets or something when I want to actually show the brackets? */
    | Lam(_) => failwith("Explanation info should handle lambdas directly")
    | Inj(_, side, _exp) =>
      let side_str =
        switch (side) {
        | L => "Left"
        | R => "Right"
        };
      build_msg("[" ++ side_str ++ " injection](0) of the [argument](1)");
    | Case(_, _scrut, rules) =>
      let rule_parts =
        List.mapi(
          (index, _) => {
            let word_num = int_to_word_number(index + 1);
            let start =
              if (index == 0) {
                "";
              } else {
                "otherwise, ";
              };
            "\n-"
            ++ start
            ++ "the ["
            ++ word_num
            ++ " pattern]("
            ++ string_of_int(index * 2 + 1)
            ++ "), evaluate to the ["
            ++ word_num
            ++ " clause]("
            ++ string_of_int((index + 1) * 2)
            ++ ")";
          },
          rules,
        );
      let tail = List.fold_left((acc, item) => acc ++ item, "", rule_parts); /* TODO: I bet there is a cleaner way to combine the two list operations (the map and fold) */
      /* TODO: Add info about exhaustivness */
      build_msg(
        "Match the value of the [scrutinee](0) to one of the "
        ++ string_of_int(List.length(rules))
        ++ " patterns. If the [scrutinee](0) matches"
        ++ tail,
      );
    | Parenthesized(_) =>
      failwith("Explanation info should handle parentheses directly")
    | ApPalette(_) => failwith("Not implemented")
    }
  | Lambda(pattern_info, body) => lambda_msg(pattern_info, body)
  | Rule(index, scrutinee, pattern_info, clause) =>
    rule_msg(index, scrutinee, pattern_info, clause)
  | ExpCommaOperator(exps) =>
    let num_exps = List.length(exps);
    let tuple_parts =
      List.mapi(
        (index, _) => {
          let word_num = int_to_word_number(index + 1);
          "[" ++ word_num ++ " element](" ++ string_of_int(index) ++ ")";
        },
        exps,
      );
    let tuple_msg = comma_separated_list(tuple_parts);
    build_msg(string_of_int(num_exps + 1) ++ "-tuple with " ++ tuple_msg);
  | ExpBinOperator(operator, _lexp, _rexp) =>
    let lop_msg = "[left operand](0)";
    let rop_msg = "[right operand](1)";
    switch (operator) {
    | Space => build_msg("Apply [function](0) to [argument](1)")
    | Plus =>
      build_msg("Integer addition of " ++ lop_msg ++ " to " ++ rop_msg)
    | Minus =>
      build_msg("Integer subtraction of " ++ rop_msg ++ " from " ++ lop_msg)
    | Times =>
      build_msg(
        "Integer multiplication of " ++ lop_msg ++ " with " ++ rop_msg,
      )
    | Divide =>
      build_msg("Integer division of " ++ lop_msg ++ " by " ++ rop_msg)
    | FPlus =>
      build_msg("Floating point addition of " ++ lop_msg ++ " to " ++ rop_msg)
    | FMinus =>
      build_msg(
        "Floating point subtraction of " ++ rop_msg ++ " from " ++ lop_msg,
      )
    | FTimes =>
      build_msg(
        "Floating point multiplication of " ++ lop_msg ++ " with " ++ rop_msg,
      )
    | FDivide =>
      build_msg("Floating point division of " ++ lop_msg ++ " by " ++ rop_msg)
    | LessThan =>
      build_msg(
        "Integer comparison of if " ++ lop_msg ++ " is less than " ++ rop_msg,
      )
    | GreaterThan =>
      build_msg(
        "Integer comparison of if "
        ++ lop_msg
        ++ " is greater than "
        ++ rop_msg,
      )
    | Equals =>
      build_msg(
        "Integer comparison of if " ++ lop_msg ++ " is equal to" ++ rop_msg,
      )
    | FLessThan =>
      build_msg(
        "Floating point comparison of if "
        ++ lop_msg
        ++ " is less than "
        ++ rop_msg,
      )
    | FGreaterThan =>
      build_msg(
        "Floating point comparison of if "
        ++ lop_msg
        ++ " is greater than "
        ++ rop_msg,
      )
    | FEquals =>
      build_msg(
        "Floating point comparison of if "
        ++ lop_msg
        ++ " is equal to "
        ++ rop_msg,
      )
    | Comma => failwith("Explanation info should handle commas directly")
    | Cons =>
      build_msg("Cons operator to make list with [head](0) and [tail](1)")
    | And => build_msg("Logical and of " ++ lop_msg ++ " with " ++ rop_msg)
    | Or => build_msg("Logical or of " ++ lop_msg ++ " with " ++ rop_msg)
    };
  | Pattern(pattern_info) => pattern_msg(pattern_info)
  | Typ(type_info) => type_msg(type_info)
  };
};

/* TODO: Hannah - work on coordinating the colors better -
   try also taking in paths here (not just terms and subterms)
   and then returning the view of the message as well as a mapping between
   the colors and paths - pass that to the decorations
   - pick the color just based on the order the path appears in the message (keeping the mapping
   in case the path appears more than once) */
let view = (explanation_info: ExplanationInfo.explanation_info): Node.t => {
  let summary_view = {
    Node.div(
      [Attr.classes(["the-summary"])],
      [Node.div([], summary_msg(explanation_info))],
    );
  };

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Summary"),
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [summary_view],
      ),
    ],
  );
};
