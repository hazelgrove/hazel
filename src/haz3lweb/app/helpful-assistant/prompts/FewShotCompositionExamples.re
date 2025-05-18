module Sexp = Sexplib.Sexp;

let self = [
  /* Example 1: Fixing a type error in a function */
  "Example 1: Fixing a type error in a function",
  "PROGRAM SKETCH:
let add = fun x -> x + \"hello\" in  # Type error: adding Int and String #
add(5)  # Error occurs when trying to add 5 and \"hello\" #
",
  "STATIC ERRORS:
Error in term:
  \"hello\"
Nature of error: Expecting type Int but got inconsistent type String",
  "USER: Fix the type error in the add function.",
  "ASSISTANT: I'll fix the type error in the add function by first locating it and then making the necessary changes.

```goto_definition add```

Now I'll fix the type error by making sure both operands have compatible types:

```edit let add = fun x -> x + 5 in```

Let me see how those changes affect the program.",
  "USER: SYSTEM: after your most recent edits, here is the current state of the code.",
  "ASSISTANT: The function now correctly adds an integer to the input. Task completed:

```submit```",
  /* Example 2: Creating a list reversal function */
  "Example 2: Creating a list reversal function",
  "PROGRAM SKETCH:
# The program has various list functions but is missing a reverse function #
let map: forall a -> forall b -> (a -> b) -> [a] -> [b] = ... in
let filter: forall a -> (a -> Bool) -> [a] -> [a] = ... in
# Our task is to implement the missing reverse function #
let reverse = ? in
[1, 2, 3]  # We want to be able to reverse this to [3, 2, 1] #
",
  "STATIC ERRORS:
No static errors found",
  "USER: Create a reverse function for lists.",
  "ASSISTANT: I'll create a list reversal function that works on any type of list.

```goto_definition reverse```

I'll implement the reverse function with proper type annotations and recursion:

```edit let reverse: forall a -> [a] -> [a] =
  typfun a -> fun xs: [a] ->
    let rev_helper: ([a], [a]) -> [a] =
      fun (acc, rest) ->
        case rest
        | [] => acc
        | hd::tl => rev_helper(hd::acc, tl)
        end
    in
    rev_helper([], xs)
in```
",
  "USER: SYSTEM: after your most recent edits, here is the current state of the code.",
  "ASSISTANT: The function is complete and typed correctly:

```submit```",
  /* Example 3: Fixing multiple functions */
  "Example 3: Fixing multiple functions",
  "PROGRAM SKETCH:
# The program has mutually recursive even/odd functions with bugs #
let (even: Int -> Bool, odd: Int -> Bool) =
(fun n -> if n == 0 then true else odd(n + 1),  # Bug: should be n - 1 #
 fun n -> if n == 1 then false else even(n + 1))  # Bug: base case should be n == 0 #
in
even(4)  # Should return true but will cause infinite recursion #
",
  "STATIC ERRORS:
No static errors found",
  "USER: Fix both the even and odd functions.",
  "ASSISTANT: I'll fix both recursive functions by first examining the even function:

```goto_definition even```

Now I'll correct the base case and recursive call:

```edit let even: Int -> Bool =
  fun n ->
    if n == 0
    then true
    else odd(n - 1)
in```

Now I'll move to the odd function:

```goto_definition odd```

And fix its implementation:

```edit let odd: Int -> Bool =
  fun n ->
    if n == 0
    then false
    else even(n - 1)
in```
",
  "USER: SYSTEM: After your most recent edits, here is the current state of the code.",
  "ASSISTANT: Both functions are now correctly implemented with proper mutual recursion:

```submit```",
  /* Example 4: Updating an ADT and related function */
  "Example 4: Updating an ADT and related function",
  "PROGRAM SKETCH:
# The program has a simple expression language and evaluator #
type Exp =
  + Var(String)
  + Lam(String, Exp)
  + Ap(Exp, Exp)
in

type Result =
  + Error(String)
  + Ok(Exp)
in

let eval: Exp -> Result =
  fun e ->
    case e
    | Var(n) => Error(\"Free Variable\")
    | Lam(x, body) => Ok(Lam(x, body))
    | Ap(e1, e2) =>
        case eval(e1)
        | Ok(Lam(x, body)) =>
            case eval(e2)
            | Error(err) => Error(err)
            | Ok(arg) => eval(subst(arg, x, body))
            end
        | _ => Error(\"Not a Function\")
        end
    end
in
",
  // Technically 'subst' is not defined in the program sketch, but it seems that it is assumed to be defined elsewhere
  "STATIC ERRORS:
No static errors found",
  "USER: Add a Mult constructor to the Exp type and update the eval function to handle it.",
  "ASSISTANT: I'll first check the Exp type definition:

```goto_definition Exp```

Now I'll update it to include the Mult constructor:

```edit type Exp =
  + Var(String)
  + Lam(String, Exp)
  + Ap(Exp, Exp)
  + Mult(Exp, Exp)
in```

Next, I'll update the eval function to handle the new constructor:

```goto_definition eval```

```edit let eval: Exp -> Result =
  fun e ->
    case e
    | Var(n) => Error(\"Free Variable\")
    | Lam(x, body) => Ok(Lam(x, body))
    | Ap(e1, e2) =>
        case eval(e1)
        | Ok(Lam(x, body)) =>
            case eval(e2)
            | Error(err) => Error(err)
            | Ok(arg) => eval(subst(arg, x, body))
            end
        | _ => Error(\"Not a Function\")
        end
    | Mult(e1, e2) =>
        case eval(e1), eval(e2)
        | Ok(Lam(_, _)), _ => Error(\"Expected number, got function\")
        | _, Ok(Lam(_, _)) => Error(\"Expected number, got function\")
        | Error(err), _ => Error(err)
        | _, Error(err) => Error(err)
        | Ok(Var(n1)), Ok(Var(n2)) =>
            if is_number(n1) && is_number(n2)
            then Ok(Var(string_of_int(int_of_string(n1) * int_of_string(n2))))
            else Error(\"Can only multiply numbers\")
        end
in```
",
  "USER: SYSTEM: after your most recent edits, here is the current state of the code.",
  "ASSISTANT: Both the type and evaluation function have been updated properly:

```submit```",
];
