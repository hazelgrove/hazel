open Virtual_dom.Vdom;
//open Sexplib;

/* TODO - Hannah - this is used (or something pretty similar) other places and should probably be refactored to somewhere
   centeralized like AssistantView_common - or maybe the different uses are different enough... */
let code_node = text =>
  Node.span([Attr.classes(["code"])], [Node.text(text)]);

let highlight =
    (msg: list(Node.t), steps: CursorPath.steps, mapping: ColorSteps.t)
    : (Node.t, ColorSteps.t) => {
  let (c, mapping) = ColorSteps.get_color(steps, mapping);
  /*print_endline(
      "Color chosen at highlight: ("
      ++ Sexp.to_string(CursorPath.sexp_of_steps(steps))
      ++ ", "
      ++ c
      ++ ")",
    );*/
  (
    Node.span(
      [Attr.style(Css_gen.(create(~field="background-color", ~value=c)))],
      msg,
    ),
    mapping,
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
  /*let _ = List.map(item => print_endline(item), items);*/
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

let print_markdown = doc => {
  print_endline("-----------------BEGIN PRINTING------------------");
  let rec print_markdown' = doc => {
    let _ =
      List.mapi(
        (index, element) => {
          print_endline(string_of_int(index));
          switch (element) {
          | Omd.Paragraph(d) =>
            print_endline("------Paragraph---------");
            print_markdown'(d);
          | Ul(_items) => print_endline("Ul")
          | Ulp(_items) => print_endline("Ul  PPPPPP")
          | Text(_) => print_endline("Text")
          | Url(_, d, _) =>
            print_endline("URL");
            print_markdown'(d);
          | Code(_) => print_endline("Code")
          | _ => print_endline("Something else")
          };
        },
        doc,
      );
    ();
  };
  print_markdown'(doc);
  print_endline("---------------------END PRINTING-----------------");
};

/*
 Markdown like thing:
 highlighty thing : [thing to highlight](int indices)
 bulleted list: - list item
                - list item
 code: `code`
 */
let build_msg =
    (text: string, _show_highlight: bool): (list(Node.t), ColorSteps.t) => {
  let omd = Omd.of_string(text);
  print_markdown(omd);
  let rec translate =
          (doc: Omd.t, mapping: ColorSteps.t): (list(Node.t), ColorSteps.t) =>
    List.fold_left(
      ((msg, mapping), elem) => {
        switch (elem) {
        | Omd.Paragraph(d) => translate(d, mapping)
        | Text(t) => (List.append(msg, [Node.text(t)]), mapping)
        | Ul(items) =>
          print_endline("IN THE LIST THINGY");
          let (bullets, mapping) =
            List.fold_left(
              ((nodes, mapping), d) => {
                let (n, mapping) = translate(d, mapping);
                (List.append(nodes, [Node.li([], n)]), mapping);
              },
              ([], mapping),
              items,
            );
          (List.append(msg, [Node.ul([], bullets)]), mapping); /* TODO Hannah - Should this be an ordered list instead of an unordered list? */
        | Code(_name, t) => (List.append(msg, [code_node(t)]), mapping)
        | Url(path, d, _title) =>
          let (d, mapping) = translate(d, mapping);
          let path =
            List.map(
              int_of_string,
              Re.Str.split(Re.Str.regexp({| |}), path),
            );
          let (inner_msg, mapping) = highlight(d, path, mapping);
          (List.append(msg, [inner_msg]), mapping);
        | _ =>
          print_endline("OTHER");
          (msg, mapping);
        }
      },
      ([], mapping),
      doc,
    );
  translate(omd, ColorSteps.empty);
};

let let_line_msg =
    (
      pattern_info: ExplanationInfo.pattern_info,
      def: UHExp.t,
      body: UHExp.t,
      show_highlight: bool,
    )
    : (list(Node.t), ColorSteps.t) => {
  let _ = def;
  let _ = body;
  switch (pattern_info) {
  | Operand(operand, _type) =>
    switch (operand) {
    | EmptyHole(n) =>
      build_msg(
        "Bind the [definition](1) to the pattern that fills [hole "
        ++ string_of_int(n + 1)
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
        "[Invalid text `"
        ++ text
        ++ "`](0) will stand for the [definition](1) in the [body](2) when the invalid text is corrected",
        show_highlight,
      )
    | Var(_, _, var) =>
      build_msg(
        "[Variable `"
        ++ var
        ++ "`](0) will stand for the [definition](1) in the [body](2)",
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
    let pattern_parts =
      List.mapi(
        (index, _) => {
          let word_num = int_to_word_number(index + 1);
          "the ["
          ++ word_num
          ++ " pattern](0 "
          ++ string_of_int(index)
          ++ ") will stand for the "
          ++ word_num
          ++ " element";
        },
        pats,
      );
    let pattern_msg = comma_separated_list(pattern_parts);
    build_msg(
      "In the [body](2), " ++ pattern_msg ++ " of the [definition tuple](1)",
      show_highlight,
    );
  | BinOperator(operator, _lpat, _rpat, _type) =>
    switch (operator) {
    | Comma => failwith("Pattern info should handle commas directly")
    | Cons =>
      build_msg(
        "In the [body](2), the [head pattern](0 0) will stand for the head and the [tail pattern](0 1) will stand for the tail of the [definition list](1)",
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
let fun_msg =
    (
      pattern_info: ExplanationInfo.pattern_info,
      body: UHExp.t,
      show_highlight: bool,
    )
    : (list(Node.t), ColorSteps.t) => {
  let _ = body;
  let begin_msg = "Function literal that returns the value of the [body](1) when applied to an argument ";
  switch (pattern_info) {
  | Operand(operand, _type) =>
    switch (operand) {
    | EmptyHole(n) =>
      build_msg(
        begin_msg
        ++ "when [hole "
        ++ string_of_int(n + 1)
        ++ "](0) is filled with a valid pattern",
        show_highlight,
      )
    | Wild(_) =>
      build_msg(begin_msg ++ "that is [ignored](0)", show_highlight)
    | TypeAnn(_) =>
      failwith("Pattern info should handle type annotations directly")
    | InvalidText(_, text) =>
      build_msg(
        begin_msg
        ++ " when the [invalid text `"
        ++ text
        ++ "`](0) is corrected",
        show_highlight,
      )
    | Var(_, _, var) =>
      build_msg(begin_msg ++ "[`" ++ var ++ "`](0)", show_highlight)
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
        begin_msg ++ "list with [head](0 0) and [tail](0 1)",
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
    : (list(Node.t), ColorSteps.t) => {
  let is_first_rule = index == 0;
  let if_scrut_msg = "If the [scrutinee](-1 0) ";
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
        ++ string_of_int(n + 1)
        ++ "](0) is filled with a valid pattern",
        show_highlight,
      );
    | Wild(_) =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atch on the [wildcard pattern](0) and evaluate the [clause](1)",
        show_highlight,
      );
    | TypeAnn(_) =>
      failwith("Pattern info should handle type annotations directly")
    | InvalidText(_, text) =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atching on this rule is delayed until [invalid text `"
        ++ text
        ++ "`](0) is corrected",
        show_highlight,
      );
    | Var(_, _, var) =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atch on this rule, where [variable `"
        ++ var
        ++ "`](0) will stand for the [scrutinee](-1 0) in the [clause](1)",
        show_highlight,
      );
    | IntLit(_, lit)
    | FloatLit(_, lit) =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
      build_msg(
        begin_msg
        ++ "matches the [pattern `"
        ++ lit
        ++ "`](0), evaluate the [clause](1)",
        show_highlight,
      );
    | BoolLit(_, lit) =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
      build_msg(
        begin_msg
        ++ "matches the [pattern `"
        ++ string_of_bool(lit)
        ++ "`](0), evaluate the [clause](1)",
        show_highlight,
      );
    | ListNil(_) =>
      let begin_msg =
        if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", "); /* TODO: The parsing doesn't work with the empty list pattern showing up as text */
      build_msg(
        begin_msg
        ++ "matches the [empty list pattern `[]`](0), evaluate the [clause](1)",
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
        ++ " injection](0) of the [argument pattern](0 0), evaluate the [clause](1)",
        show_highlight,
      ); /* TODO: Figre out if can do the overlapping highlight... */
    | Parenthesized(_) =>
      failwith("Pattern info should handle parentheses directly")
    }
  | CommaOperator(_pats, _type) =>
    let begin_msg =
      if_scrut_msg ++ (is_first_rule ? "" : not_matched_msg ++ ", ");
    build_msg(
      begin_msg ++ "matches the [tuple pattern](0), evaluate the [clause](1)",
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
        ++ "matches the list pattern with [head](0 0) and [tail](0 1), evaluate the [clause](1)",
        show_highlight,
      );
    | Space =>
      let begin_msg =
        is_first_rule ? "M" : if_scrut_msg ++ not_matched_msg ++ ", m";
      build_msg(
        begin_msg
        ++ "atching on this rule is delayed until [the pattern](0) is corrected (function application of a pattern is not valid)",
        show_highlight,
      );
    }
  };
};

let pattern_msg =
    (pattern_info: ExplanationInfo.pattern_info, show_highlight: bool)
    : (list(Node.t), ColorSteps.t) => {
  let typ_annot_step =
    switch (pattern_info) {
    | Operand(_, Some(_))
    | CommaOperator(_, Some(_))
    | BinOperator(_, _, _, Some(_)) => "0"
    | Operand(_, None)
    | CommaOperator(_, None)
    | BinOperator(_, _, _, None) => ""
    };
  switch (pattern_info) {
  | Operand(operand, _type) =>
    switch (operand) {
    | EmptyHole(n) =>
      build_msg(
        "Empty [pattern hole]("
        ++ typ_annot_step
        ++ ") with id "
        ++ string_of_int(n + 1)
        ++ ". Matching on this pattern is delayed until the hole is filled",
        show_highlight,
      )
    | Wild(_) =>
      build_msg(
        "[Wildcard]("
        ++ typ_annot_step
        ++ "). Matches any value without binding it to a variable",
        show_highlight,
      )
    | TypeAnn(_) =>
      /* TODO: Hannah - check this case and add the typing information (instead of having the filler "ty"*/
      build_msg(
        "[Type annotated pattern]("
        ++ typ_annot_step
        ++ "). Matches values of type ty that match the annotated pattern",
        show_highlight,
      )
    | InvalidText(_, text) =>
      build_msg(
        "[Invalid text `"
        ++ text
        ++ "`]("
        ++ typ_annot_step
        ++ ") is not a valid name or literal pattern",
        show_highlight,
      )
    | Var(_, _, var) =>
      build_msg(
        "[Variable pattern]("
        ++ typ_annot_step
        ++ "). Matches any value and binds it to the variable "
        ++ var,
        show_highlight,
      )
    | IntLit(_, n) =>
      build_msg(
        "[Integer literal pattern `"
        ++ n
        ++ "`]("
        ++ typ_annot_step
        ++ "). Matches the value "
        ++ n,
        show_highlight,
      )
    | FloatLit(_, n) =>
      build_msg(
        "[Floating point literal pattern `"
        ++ n
        ++ "`]("
        ++ typ_annot_step
        ++ "). Matches the value "
        ++ n,
        show_highlight,
      )
    | BoolLit(_, b) =>
      build_msg(
        "[Boolean literal pattern `"
        ++ string_of_bool(b)
        ++ "`]("
        ++ typ_annot_step
        ++ "). Matches the value "
        ++ string_of_bool(b),
        show_highlight,
      )
    | ListNil(_) =>
      build_msg(
        "[Empty list (nil) pattern]("
        ++ typ_annot_step
        ++ "). Matches the empty list value `[]`",
        show_highlight,
      )
    | Inj(_, side, _) =>
      /* TODO: Is the highlightinf for this what we want? */
      let (cap, low) =
        switch (side) {
        | L => ("Left", "left")
        | R => ("Right", "right")
        };
      build_msg(
        "["
        ++ cap
        ++ " injection pattern with argument pattern]("
        ++ typ_annot_step
        ++ " 0). Matches any "
        ++ low
        ++ " injection value where the arguent matches argument pattern",
        show_highlight,
      );
    | Parenthesized(_) =>
      failwith("Pattern info should handle parentheses directly")
    }
  | CommaOperator(pats, _type) =>
    let n = string_of_int(List.length(pats));
    build_msg(
      "["
      ++ n
      ++ "-tuple pattern]("
      ++ typ_annot_step
      ++ "). Matches any "
      ++ n
      ++ "-tuple value where the elements match in order the corresponding element patterns",
      show_highlight,
    );
  | BinOperator(operator, _lpat, _rpat, _type) =>
    switch (operator) {
    | Comma => failwith("Pattern info should handle commas directly")
    | Cons =>
      build_msg(
        "Non-empty list (cons) pattern. Matches any list value with head matching [head pattern]("
        ++ typ_annot_step
        ++ " 0) and tail matching [tail pattern]("
        ++ typ_annot_step
        ++ " 1)",
        show_highlight,
      )
    | Space =>
      build_msg(
        "[Function application]("
        ++ typ_annot_step
        ++ ") is not a valid pattern. No values match this pattern",
        show_highlight,
      )
    }
  };
};
/* TODO: Hannah - Display the types */
let type_msg =
    (type_info: ExplanationInfo.type_info, show_highlight: bool)
    : (list(Node.t), ColorSteps.t) => {
  switch (type_info) {
  | Operand(op) =>
    switch (op) {
    | Hole =>
      build_msg(
        "[Type hole (unknown type)](), which is consistent with any type of expression",
        show_highlight,
      ) /* TODO: Get the markup thingy working with this */
    | Unit =>
      build_msg(
        "[Unit type](), which classifies expressions that evaluate to the trivial value ()",
        show_highlight,
      )
    | Int =>
      build_msg(
        "[Integer type](), which classifies expressions that evaluate to integer values",
        show_highlight,
      )
    | Float =>
      build_msg(
        "[Floating point type](), which classifies expressions that evaluate to floating point values",
        show_highlight,
      )
    | Bool =>
      build_msg(
        "[Boolean type](), which classifies expressions that evaluate to `true` or `false`",
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
    let n = string_of_int(List.length(typs));
    build_msg(
      "["
      ++ n
      ++ "-tuple type]() with element types, which classifies expressions that evaluate to tuple values with elements of the element types (in order)",
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
        "Sum type of [left summand type](0) and [right summand type](1), which classifies expressions that evaluate to either left injection values (`Inj[L](v)`) with argument `v` of the left summand type or right injection values (`Inj[R](v)`) with argument `v` of the right summand type",
        show_highlight,
      )
    }
  };
};

let summary_msg =
    (explanation_info: ExplanationInfo.explanation_info, show_highlight: bool)
    : (list(Node.t), ColorSteps.t) => {
  switch (explanation_info) {
  | EmptyLine => ([Node.text("Empty line")], ColorSteps.empty)
  | CommentLine => ([Node.text("Comment")], ColorSteps.empty)
  | Block(_all_but_last, last_index, _last) =>
    build_msg(
      "Code [block](), where each line of is evaluated in order. The value of the entire block is the value of the [last line]("
      ++ string_of_int(last_index)
      ++ ")",
      show_highlight,
    )
  | LetLine(pattern_info, def, _start_index, body) =>
    let_line_msg(pattern_info, def, body, show_highlight)
  | ExpBaseOperand(operand) =>
    switch (operand) {
    /* TODO: Hannah - Should just be a text node if nothing fancy in them? */
    | EmptyHole(n) =>
      build_msg(
        "Empty expression hole with id " ++ string_of_int(n + 1),
        show_highlight,
      )
    | InvalidText(_, t) =>
      build_msg(
        "Invalid text `" ++ t ++ "` is not a valid name, keyword, or literal",
        show_highlight,
      )
    | Var(_, _, v) => build_msg("Variable `" ++ v ++ "`", show_highlight)
    | IntLit(_, n) =>
      build_msg("Integer literal `" ++ n ++ "`", show_highlight)
    | FloatLit(_, n) =>
      build_msg("Floating point literal `" ++ n ++ "`", show_highlight)
    | BoolLit(_, b) =>
      build_msg(
        "Boolean literal `" ++ string_of_bool(b) ++ "`",
        show_highlight,
      )
    | ListNil(_) => build_msg("Empty list `[]`", show_highlight)
    | Fun(_) => failwith("Explanation info should handle Funs directly")
    | Inj(_, side, _exp) =>
      let side_str =
        switch (side) {
        | L => "Left"
        | R => "Right"
        };
      build_msg(
        side_str ++ " injection of the [argument](0)",
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
            "\n- "
            ++ start
            ++ "the ["
            ++ word_num
            ++ " pattern]("
            ++ string_of_int(index + 1)
            ++ " 0), evaluate to the ["
            ++ word_num
            ++ " clause]("
            ++ string_of_int(index + 1)
            ++ " 1)";
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
    }
  | Fun(pattern_info, body) => fun_msg(pattern_info, body, show_highlight)
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
      string_of_int(num_exps) ++ "-tuple with " ++ tuple_msg,
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

let get_mapping =
    (explanation_info: ExplanationInfo.explanation_info, show_highlight: bool)
    : ColorSteps.t => {
  let (_, mapping) = summary_msg(explanation_info, show_highlight);
  mapping;
};

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

  let (summary, _) = summary_msg(explanation_info, show_highlight);

  let summary_view = {
    Node.div([Attr.classes(["the-summary"])], [Node.div([], summary)]);
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
