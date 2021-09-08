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
let build_msg = (text: string, show_highlight: bool): list(Node.t) => {
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
            ] =>
            let msg_node =
              if (show_highlight) {
                highlight(msg, int_of_string(index));
              } else {
                Node.text(msg);
              };
            [msg_node, ...parse_line'(xs)];
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
    (
      pattern_info: ExplanationInfo.pattern_info,
      def: UHExp.t,
      body: UHExp.t,
      show_highlight: bool,
    )
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
        show_highlight,
      )
    | Wild(_) =>
      build_msg(
        "Evaluate the [definition](1), [throw away the result](0), and then evaluate the [body](2)",
        show_highlight,
      )
    | TypeAnn(_) =>
      failwith("Pattern info should handle type annotations directly")
    | InvalidText(_, text) =>
      build_msg(
        "[Invalid text "
        ++ text
        ++ "](0) will stand for the [definition](1) in the [body](2) when the invalid text is corrected",
        show_highlight,
      )
    | Var(_, _, var) =>
      build_msg(
        "[Variable "
        ++ var
        ++ "](0) will stand for the [definition](1) in the [body](2)",
        show_highlight,
      )
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | ListNil(_)
    | Inj(_, _, _) =>
      build_msg(
        "Evaluate the [definition](1), [throw away the result](0), and then evaluate the [body](2)",
        show_highlight,
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
      show_highlight,
    );
  | BinOperator(operator, _lpat, _rpat, _type) =>
    switch (operator) {
    | Comma => failwith("Pattern info should handle commas directly")
    | Cons =>
      build_msg(
        "In the [body](3), the [head pattern](0) will stand for the head and the [tail pattern](1) will stand for the tail of the [definition list](2)",
        show_highlight,
      )
    | Space =>
      build_msg(
        "Bind the [definition](1) to the pattern that corrects the [invalid pattern](0) and evaluate the [body](2) (function application pattern is not valid)",
        show_highlight,
      )
    }
  };
};
/* TODO: Hannah - don't highlight terms when on the base operands (like an empty hole where it is highlighted as the current term anyway)*/
let lambda_msg =
    (
      pattern_info: ExplanationInfo.pattern_info,
      body: UHExp.t,
      show_highlight: bool,
    )
    : list(Node.t) => {
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
        show_highlight,
      )
    | Wild(_) =>
      build_msg(begin_msg ++ "that is [ignored](0)", show_highlight)
    | TypeAnn(_) =>
      failwith("Pattern info should handle type annotations directly")
    | InvalidText(_, text) =>
      build_msg(
        begin_msg ++ " when the [invalid text " ++ text ++ "](0) is corrected",
        show_highlight,
      )
    | Var(_, _, var) =>
      build_msg(begin_msg ++ "[ " ++ var ++ "](0)", show_highlight)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _)
    | ListNil(_)
    | Inj(_, _, _) =>
      build_msg(begin_msg ++ "that is [ignored](0)", show_highlight)
    | Parenthesized(_) =>
      failwith("Pattern info should handle parentheses directly")
    }
  | CommaOperator(_pats, _type) =>
    build_msg(begin_msg ++ " [tuple](0)", show_highlight)
  | BinOperator(operator, _lpat, _rpat, _type) =>
    switch (operator) {
    | Comma => failwith("Pattern info should handle commas directly")
    | Cons =>
      build_msg(
        begin_msg ++ "list with [head](0) and [tail](1)",
        show_highlight,
      )
    | Space =>
      build_msg(
        begin_msg
        ++ "when the [pattern](0) is corrected (function application of a pattern is not valid)",
        show_highlight,
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
      show_highlight: bool,
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
        show_highlight,
      );
    | Wild(_) =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atch on the [wildcard pattern](1) and evaluate the [clause](2)",
        show_highlight,
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
        show_highlight,
      );
    | Var(_, _, var) =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atch on this rule, where [variable "
        ++ var
        ++ "](1) will stand for the [scrutinee](0) in the [clause](2)",
        show_highlight,
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
        show_highlight,
      );
    | BoolLit(_, lit) =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
      build_msg(
        begin_msg
        ++ "matches the [pattern "
        ++ string_of_bool(lit)
        ++ "](1), evaluate the [clause](2)",
        show_highlight,
      );
    | ListNil(_) =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
      build_msg(
        begin_msg
        ++ "matches the [empty list pattern []](1), evaluate the [clause](2)",
        show_highlight,
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
        show_highlight,
      );
    | Parenthesized(_) =>
      failwith("Pattern info should handle parentheses directly")
    }
  | CommaOperator(_pats, _type) =>
    let begin_msg =
      if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
    build_msg(
      begin_msg ++ "matches the [tuple pattern](1), evaluate the [clause](2)",
      show_highlight,
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
        show_highlight,
      );
    | Space =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atching on this rule is delayed until [the pattern](1) is corrected (function application of a pattern is not valid)",
        show_highlight,
      );
    }
  };
};

let pattern_msg =
    (pattern_info: ExplanationInfo.pattern_info, show_highlight: bool)
    : list(Node.t) => {
  switch (pattern_info) {
  | Operand(operand, _type) =>
    switch (operand) {
    | EmptyHole(n) =>
      build_msg(
        "Empty [pattern hole](0) with id "
        ++ string_of_int(n + 1)
        ++ ". Matching on this pattern is delayed until the hole is filled",
        show_highlight,
      )
    | Wild(_) =>
      build_msg(
        "[Wildcard](0). Matches any value without binding it to a variable",
        show_highlight,
      )
    | TypeAnn(_) =>
      /* TODO: Hannah - check this case */
      build_msg(
        "[Type annotated pattern](0). Matches values of type ty that match the annotated pattern",
        show_highlight,
      )
    | InvalidText(_, text) =>
      build_msg(
        "[Invalid text "
        ++ text
        ++ "](0) is not a valid name or literal pattern",
        show_highlight,
      )
    | Var(_, _, var) =>
      build_msg(
        "[Variable pattern](0). Mathces any valud and binds it to the variable "
        ++ var,
        show_highlight,
      )
    | IntLit(_, n) =>
      build_msg(
        "[Integer literal pattern " ++ n ++ "](0). Matches the value " ++ n,
        show_highlight,
      )
    | FloatLit(_, n) =>
      build_msg(
        "[Floating point literal pattern "
        ++ n
        ++ "](0). Matches the value "
        ++ n,
        show_highlight,
      )
    | BoolLit(_, b) =>
      build_msg(
        "[Boolean literal pattern "
        ++ string_of_bool(b)
        ++ "](0). Matches the value "
        ++ string_of_bool(b),
        show_highlight,
      )
    | ListNil(_) =>
      build_msg(
        "[Empty list (nil) pattern](0). Matches the empty list value []",
        show_highlight,
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
        show_highlight,
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
      show_highlight,
    );
  | BinOperator(operator, _lpat, _rpat, _type) =>
    switch (operator) {
    | Comma => failwith("Pattern info should handle commas directly")
    | Cons =>
      build_msg(
        "Non-empty list (cons) pattern. Matches any list value with head matching [head pattern](0) and tail matching [tail pattern](1)",
        show_highlight,
      )
    | Space =>
      build_msg(
        "[Function application](0) is not a vlaid pattern. No values match this pattern",
        show_highlight,
      )
    }
  };
};
/* TODO: Hannah - Display the types */
let type_msg =
    (type_info: ExplanationInfo.type_info, show_highlight: bool)
    : list(Node.t) => {
  switch (type_info) {
  | Operand(op) =>
    switch (op) {
    | Hole =>
      build_msg(
        "[Type hole (unknown type)](0), which is consistent with any type of expression",
        show_highlight,
      )
    | Unit =>
      build_msg(
        "[Unit type](0), which classifies expressions that evaluate to the trivial value ()",
        show_highlight,
      )
    | Int =>
      build_msg(
        "[Integer type](0), which classifies expressions that evaluate to integer values",
        show_highlight,
      )
    | Float =>
      build_msg(
        "[Floating point type](0), which classifies expressions that evaluate to floating point values",
        show_highlight,
      )
    | Bool =>
      build_msg(
        "[Boolean type](0), which classifies expressions that evaluate to true or false",
        show_highlight,
      )
    | Parenthesized(_) =>
      failwith("Type info should handle parentheses directly")
    | List(_) =>
      build_msg(
        "List type with [element type](0), which classifies expressions that evaluate to list values with elements of the element type",
        show_highlight,
      )
    }
  | CommaOperator(typs) =>
    let n = string_of_int(List.length(typs) + 1);
    build_msg(
      n
      ++ "-tuple type with element types, which classifies expressions that evaluate to tuple values with elements of the element types (in order)",
      show_highlight,
    );
  | BinOperator(op, _, _) =>
    switch (op) {
    | Arrow =>
      build_msg(
        "Arrow type with [argument type](0) and [return type](1), which classifies expressions that evaluate to function values that take arguments of the argument type and return values of the return type",
        show_highlight,
      )
    | Prod => failwith("Type info should handle products directly")
    | Sum =>
      build_msg(
        "Sum type of [left summand type](0) and [right summand type](1), which classifies expressions that evaluate to either left injection values (Inj[L](v)) with argument v of the left summand type or right injection values (Inj[R](v)) with argument v of the right summand type",
        show_highlight,
      )
    }
  };
};

let summary_msg =
    (explanation_info: ExplanationInfo.explanation_info, show_highlight: bool)
    : list(Node.t) => {
  switch (explanation_info) {
  | EmptyLine => [Node.text("Empty line")]
  | CommentLine => [Node.text("Comment")]
  | LetLine(pattern_info, def, body) =>
    let_line_msg(pattern_info, def, body, show_highlight)
  | ExpBaseOperand(operand) =>
    switch (operand) {
    /* TODO: Hannah - should these really be highlighted when they these simple operands? */
    | EmptyHole(n) =>
      build_msg(
        "[Empty expression hole](0) with id " ++ string_of_int(n + 1),
        show_highlight,
      )
    | InvalidText(_, t) =>
      build_msg(
        "[Invalid text "
        ++ t
        ++ "](0) is not a valid name, keyword, or literal",
        show_highlight,
      )
    | Var(_, _, v) => build_msg("[Variable " ++ v ++ "](0)", show_highlight)
    | IntLit(_, n) =>
      build_msg("[Integer literal " ++ n ++ "](0)", show_highlight)
    | FloatLit(_, n) =>
      build_msg("[Floating point literal " ++ n ++ "](0)", show_highlight)
    | BoolLit(_, b) =>
      build_msg(
        "[Boolean literal " ++ string_of_bool(b) ++ "](0)",
        show_highlight,
      )
    | ListNil(_) => build_msg("[Empty list []](0)", show_highlight) /*TODO: Hannah - the way the parser is implemented now this doesn't work to do highlighting - maybe I need to escape brackets or something when I want to actually show the brackets? */
    | Lam(_) => failwith("Explanation info should handle lambdas directly")
    | Inj(_, side, _exp) =>
      let side_str =
        switch (side) {
        | L => "Left"
        | R => "Right"
        };
      build_msg(
        "[" ++ side_str ++ " injection](0) of the [argument](1)",
        show_highlight,
      );
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
        show_highlight,
      );
    | Parenthesized(_) =>
      failwith("Explanation info should handle parentheses directly")
    | ApPalette(_) => failwith("Not implemented")
    }
  | Lambda(pattern_info, body) =>
    lambda_msg(pattern_info, body, show_highlight)
  | Rule(index, scrutinee, pattern_info, clause) =>
    rule_msg(index, scrutinee, pattern_info, clause, show_highlight)
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
    build_msg(
      string_of_int(num_exps + 1) ++ "-tuple with " ++ tuple_msg,
      show_highlight,
    );
  | ExpBinOperator(operator, _lexp, _rexp) =>
    let lop_msg = "[left operand](0)";
    let rop_msg = "[right operand](1)";
    switch (operator) {
    | Space =>
      build_msg("Apply [function](0) to [argument](1)", show_highlight)
    | Plus =>
      build_msg(
        "Integer addition of " ++ lop_msg ++ " to " ++ rop_msg,
        show_highlight,
      )
    | Minus =>
      build_msg(
        "Integer subtraction of " ++ rop_msg ++ " from " ++ lop_msg,
        show_highlight,
      )
    | Times =>
      build_msg(
        "Integer multiplication of " ++ lop_msg ++ " with " ++ rop_msg,
        show_highlight,
      )
    | Divide =>
      build_msg(
        "Integer division of " ++ lop_msg ++ " by " ++ rop_msg,
        show_highlight,
      )
    | FPlus =>
      build_msg(
        "Floating point addition of " ++ lop_msg ++ " to " ++ rop_msg,
        show_highlight,
      )
    | FMinus =>
      build_msg(
        "Floating point subtraction of " ++ rop_msg ++ " from " ++ lop_msg,
        show_highlight,
      )
    | FTimes =>
      build_msg(
        "Floating point multiplication of " ++ lop_msg ++ " with " ++ rop_msg,
        show_highlight,
      )
    | FDivide =>
      build_msg(
        "Floating point division of " ++ lop_msg ++ " by " ++ rop_msg,
        show_highlight,
      )
    | LessThan =>
      build_msg(
        "Integer comparison of if " ++ lop_msg ++ " is less than " ++ rop_msg,
        show_highlight,
      )
    | GreaterThan =>
      build_msg(
        "Integer comparison of if "
        ++ lop_msg
        ++ " is greater than "
        ++ rop_msg,
        show_highlight,
      )
    | Equals =>
      build_msg(
        "Integer comparison of if " ++ lop_msg ++ " is equal to" ++ rop_msg,
        show_highlight,
      )
    | FLessThan =>
      build_msg(
        "Floating point comparison of if "
        ++ lop_msg
        ++ " is less than "
        ++ rop_msg,
        show_highlight,
      )
    | FGreaterThan =>
      build_msg(
        "Floating point comparison of if "
        ++ lop_msg
        ++ " is greater than "
        ++ rop_msg,
        show_highlight,
      )
    | FEquals =>
      build_msg(
        "Floating point comparison of if "
        ++ lop_msg
        ++ " is equal to "
        ++ rop_msg,
        show_highlight,
      )
    | Comma => failwith("Explanation info should handle commas directly")
    | Cons =>
      build_msg(
        "Cons operator to make list with [head](0) and [tail](1)",
        show_highlight,
      )
    | And =>
      build_msg(
        "Logical and of " ++ lop_msg ++ " with " ++ rop_msg,
        show_highlight,
      )
    | Or =>
      build_msg(
        "Logical or of " ++ lop_msg ++ " with " ++ rop_msg,
        show_highlight,
      )
    };
  | Pattern(pattern_info) => pattern_msg(pattern_info, show_highlight)
  | Typ(type_info) => type_msg(type_info, show_highlight)
  };
};

/* TODO: Hannah - work on coordinating the colors better -
   try also taking in paths here (not just terms and subterms)
   and then returning the view of the message as well as a mapping between
   the colors and paths - pass that to the decorations
   - pick the color just based on the order the path appears in the message (keeping the mapping
   in case the path appears more than once) */
let view =
    (
      ~inject: ModelAction.t => Event.t,
      explanation_info: ExplanationInfo.explanation_info,
      show_highlight: bool,
    )
    : Node.t => {
  let button_bar_view =
    Node.div(
      [Attr.classes(["history-button-bar"])],
      [
        SettingsPanel.labeled_checkbox(
          ~id="show_highlight_summary",
          ~label="Show Highlight",
          ~on_change=() => inject(ToggleHighlightSummary),
          show_highlight,
        ),
      ],
    );

  let summary_view = {
    Node.div(
      [Attr.classes(["the-summary"])],
      [Node.div([], summary_msg(explanation_info, show_highlight))],
    );
  };

  Node.div(
    [Attr.classes(["panel", "context-inspector-panel"])],
    [
      Panel.view_of_main_title_bar("Summary"),
      button_bar_view,
      Node.div(
        [Attr.classes(["panel-body", "context-inspector-body"])],
        [summary_view],
      ),
    ],
  );
};
