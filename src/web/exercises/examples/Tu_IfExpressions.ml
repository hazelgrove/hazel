open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000030-0030-0030-0030-000000000030");
    title = "If Expressions";
    module_name = "Tu_IfExpressions";
    version = 1;
    prompt =
      {md|If expressions let you choose between two values based on a condition.

The syntax is:
```hazelnostatics
if condition then result1 else result2
```

The condition must be a `Bool`. If it is `true`, the expression evaluates to `result1`; if `false`, to `result2`.

Example:
```hazel
let abs_val = fun x ->
  if x < 0 then 0 - x else x
in
abs_val(-3)
```

You can **nest** if expressions for multiple conditions:
```hazel
let sign = fun x ->
  if x < 0 then -1
  else if x == 0 then 0
  else 1
in
sign(42)
```

# Task

Complete the function `clamp` below.

The function takes an integer `x` and constrains it to the range 0 to 100:

- If `x < 0`, return `0`.
- If `x > 100`, return `100`.
- Otherwise, return `x` unchanged.

**Example**:
```hazelnostatics
clamp(-5) == 0;
clamp(50) == 50;
clamp(150) == 100
```|md};
    display_hint =
      "Use `if x < 0 then 0 else ...` with a nested if for the upper bound";
    task_reference =
      TaskRefDocs.compose
        [ TaskRefDocs.if_expression; TaskRefDocs.comparison_operators ];
    wrapper = false;
    show_report = true;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper ~root:Exp
           "let clamp = fun x -> x in\nclamp(50)");
    hidden_tests =
      {
        tests =
          Option.get
            (Haz3lcore.Parser.to_zipper ~root:Exp
               "test clamp(-5) == 0 end;\n\
                test clamp(50) == 50 end;\n\
                test clamp(150) == 100 end\n");
        hints =
          [
            "clamp(-5) should be 0";
            "clamp(50) should be 50";
            "clamp(150) should be 100";
          ];
      };
  }
