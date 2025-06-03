module Sexp = Sexplib.Sexp;

let self = [
  "<calculatorExample>",
  "USER MESSAGE: please write a simple calculator program",
  "SYSTEM MESSAGE: PROGRAM SKETCH: ```?```
    STATIC ERRORS:
    No static errors found
    SELECTED CODE: None. Use a goto_* command to select a code segment.",
  "LLM MESSAGE: Sure! I notice there's no need to select a code segment since
    the program sketch is empty. I'll begin by writing a simple stub for a calculator program:

    {{{edit type Expr =
    + Num(Int)
    + Add(Expr, Expr)
    + Sub(Expr, Expr)
    + Mul(Expr, Expr)
    + Div(Expr, Expr)}}}

    type Expr =
        + Num(Int)
        + Add(Expr, Expr)
        + Sub(Expr, Expr)
        + Mul(Expr, Expr)
        + Div(Expr, Expr)
    in

    # Note: \"eval\" is a taken keyword in Hazel. Let's use \"eval_expr\" instead.#
    # Input: A simple expression to-be calculated #
    # Returns: The result of the evaluated input expression #
    let eval_expr: Expr -> Int =
    ?
    in
    ?
    }}}

    I'll want to navigate to the body of `eval_expr` to implement it later. Let's do that now.

    {{{goto_def eval_expr}}}

    Let's view the sketch to confirm the changes and actions are taking place as expected.

    {{{view_sketch}}}
    ",
  "SYSTEM MESSAGE: You have requested to view the sketch. Please review and continue with completing the user-specified task.
    PROGRAM SKETCH: ```type Expr =
    + Num(Int)
    + Add(Expr, Expr)
    + Sub(Expr, Expr)
    + Mul(Expr, Expr)
    + Div(Expr, Expr)
    in

    # Note: \"eval\" is a taken keyword in Hazel. Let's use \"eval_expr\" instead.#
    # Input: A simple expression to-be calculated #
    # Returns: The result of the evaluated input expression #
    let eval_expr: Expr -> Int =
    ?
    in
    ?```
    STATIC ERRORS:
    No static errors found
    SELECTED CODE: 'let eval_expr: Expr -> Int =
    ?
    in'
    ",
  "LLM MESSAGE: Great! Now I'll go ahead an implement the `eval_expr` function.
    I'll use pattern matching to handle the different cases of the `Expr` type.

    {{{edit let eval_expr: Expr -> Int =
        fun e ->
        case e
        | Num(n) => n
        | Add(e1, e2) => eval_expr(e1) + eval_expr(e2)
        | Sub(e1, e2) => eval_expr(e1) - eval_expr(e2)
        | Mul(e1, e2) => eval_expr(e1) * eval_expr(e2)
        | Div(e1, e2) =>
        let denom = eval_expr(e2) in
        if denom == 0 then
        # Division by zero error #
        0
        else
            eval_expr(e1) / denom
            end
            in}}}

    And I'd also like to add a simple test to validate this implementation.
    {{{goto_body eval_expr}}}

    {{{edit let e1 = Sub(Num(5), Num(3)) in
        let e2 = Add(e1, Num(2)) in
        test eval_expr(e2) == 4 end;}}}

    Let's view the sketch now to confirm and hopefully finalize everything.
    {{{view_sketch}}}",
  "
    SYSTEM MESSAGE: You have requested to view the sketch. Please review and continue with completing the user-specified task.
    PROGRAM SKETCH: ```type Expr =
    + Num(Int)
    + Add(Expr, Expr)
    + Sub(Expr, Expr)
    + Mul(Expr, Expr)
    + Div(Expr, Expr)
    in

    # Note: \"eval\" is a taken keyword in Hazel #
    # Input: A simple expression to-be calculated #
    # Returns: The result of the evaluated input expression #
    let eval_expr: Expr -> Int =
    fun e ->
        case e
        | Num(n) => n
        | Add(e1, e2) => eval_expr(e1) + eval_expr(e2)
        | Sub(e1, e2) => eval_expr(e1) - eval_expr(e2)
        | Mul(e1, e2) => eval_expr(e1) * eval_expr(e2)
        | Div(e1, e2) =>
        let denom = eval_expr(e2) in
        if denom == 0 then
            # Division by zero error #
            0
        else
            eval_expr(e1) / denom
        end
    in
    let e1 = Sub(Num(5), Num(3)) in
    let e2 = Add(e1, Num(2)) in
    test eval_expr(e2) == 4 end;
    ```
    STATIC ERRORS:
    No static errors found
    SELECTED CODE: None. Use a goto_* command to select a code segment.
    ",
  "LLM MESSAGE: The sketch looks good! I think we're done with the calculator program.

    {{{submit}}}",
  "</calculatorExample>",
];
