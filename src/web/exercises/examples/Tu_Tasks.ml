open Haz3lcore

let string_mean_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000016-0016-0016-0016-000000000016");
    title = "Task 1: Mean of String Integers";
    module_name = "Ta_StringMean";
    prompt =
      {md|This task tests your understanding of the concepts from the tutorials. Your goal is to implement a function `string_mean : [String] -> Float` that takes a list of strings (each representing an integer) and returns their mean as a float.

Here are the built-in functions and operators you may find useful:

- `float_of_string : String -> Float` converts a string to a float
- `map : ([T], T -> U) -> [U]` applies a function to each element of a list
- `fold_left : ([T], (U, T) -> U, U) -> U` folds a list from the left
- `length : [T] -> Int` returns the length of a list
- `float_of_int : Int -> Float` converts an integer to a float
- `/. : Float -> Float -> Float` is float division (e.g. `6. /. 3.` evaluates to `2.`)

**Steps:** Convert the strings to floats, sum them, and divide by the length of the list.|md};
    wrapper = true;
    show_report = false;
    version = 16;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           {hz|let string_mean : [String] -> Float =
  fun strings ->
    let floats : [Float] = in
    let sum : Float = in
    in
string_mean|hz});
    hidden_tests =
      {
        tests =
          Option.get
            (Haz3lcore.Parser.to_zipper
               {hz|test answer(["1", "2", "3"]) == 2. end;
test answer(["10"]) == 10. end;
test answer(["0", "0", "6"]) == 2. end|hz});
        hints =
          [
            {|string_mean(["1", "2", "3"]) should be 2.|};
            {|string_mean(["10"]) should be 10.|};
            {|string_mean(["0", "0", "6"]) should be 2.|};
          ];
      };
    display_hint =
      "Convert strings to floats with map, sum with fold_left, then divide by \
       length";
    task_reference =
      {md|## Quick Reference

### Syntax Recap
```hazel
let x : Int = 5 in
x + 1
```
```hazel
fun x -> x + 1
```
```hazel
fun (acc, x) -> acc +. x
```

### List Operations
- `map : ([T], T -> U) -> [U]` — apply a function to each element
- `fold_left : ([T], (U, T) -> U, U) -> U` — fold a list from the left
- `length : [T] -> Int` — return the length of a list

### Float Arithmetic
- `2.0 +. 3.0` — addition
- `5.0 -. 1.0` — subtraction
- `3.0 *. 2.0` — multiplication
- `6.0 /. 3.0` — division

Float literals need a decimal point: `0.`, `1.0`, `3.14`|md};
  }
