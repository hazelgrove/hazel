open Haz3lcore

let list_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000012-0012-0012-0012-000000000012");
    title = "List Literals";
    module_name = "Tu_ListLiterals";
    prompt =
      {md|In Hazel, you can create a list by writing its elements between square brackets, separated by commas. For example, `[1, 2, 3]` is a list of three integers.

The type of a list is written as `[T]` where `T` is the type of the elements. So `[1, 2, 3]` has type `[Int]`, and `["hello", "world"]` has type `[String]`. All elements in a list must have the same type.

Complete the let binding below so that `first_four` contains the first four natural numbers: `[0, 1, 2, 3]`.|md};
    wrapper = true;
    show_report = false;
    version = 12;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           {hz|let first_four : [Int] = in
first_four|hz});
    hidden_tests =
      {
        tests =
          Option.get
            (Haz3lcore.Parser.to_zipper {hz|test answer == [0, 1, 2, 3] end|hz});
        hints = [ "Enter [0, 1, 2, 3]" ];
      };
    display_hint = "Fill in the list with the first four natural numbers";
    task_reference =
      {md|## Quick Reference

### List Literal
```
[1, 2, 3]
```

### List Type
`[Int]`, `[String]`, `[Bool]`|md};
  }

let concat_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000013-0013-0013-0013-000000000013");
    title = "List Concatenation";
    module_name = "Tu_ListConcat";
    prompt =
      {md|The `@` operator concatenates two lists together. For example, `[1, 2] @ [3, 4]` evaluates to `[1, 2, 3, 4]`.

Both lists must have the same element type. The `@` operator has type `[T] -> [T] -> [T]`.

Implement a function `repeat_twice` of type `[Int] -> [Int]` that takes a list and returns it concatenated with itself. For example, `repeat_twice([1, 2])` should return `[1, 2, 1, 2]`.|md};
    wrapper = true;
    show_report = false;
    version = 13;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           {hz|let repeat_twice : [Int] -> [Int] = in
repeat_twice|hz});
    hidden_tests =
      {
        tests =
          Option.get
            (Haz3lcore.Parser.to_zipper
               {hz|test answer([1, 2]) == [1, 2, 1, 2] end;
test answer([]) == [] end;
test answer([3]) == [3, 3] end|hz});
        hints =
          [
            "repeat_twice([1, 2]) should be [1, 2, 1, 2]";
            "repeat_twice([]) should be []";
            "repeat_twice([3]) should be [3, 3]";
          ];
      };
    display_hint = "Use the @ operator to concatenate the list with itself";
    task_reference =
      {md|## Quick Reference

### List Concatenation
```
[1, 2] @ [3, 4]
```
evaluates to `[1, 2, 3, 4]`

### Function Syntax
```
let f : [Int] -> [Int] = fun xs -> xs in
f([1, 2]) # Evaluates to [1, 2] #
```|md};
  }

let map_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000014-0014-0014-0014-000000000014");
    title = "Mapping Over Lists";
    module_name = "Tu_ListMap";
    prompt =
      {md|The built-in function `map` applies a function to every element of a list and returns a new list of the results. It has the signature `map : ([T], T -> U) -> [U]`.

For example, `map([1, 2, 3], fun x -> x + 1)` evaluates to `[2, 3, 4]`.

Implement a function `double_all` of type `[Int] -> [Int]` that doubles every element of a list using `map`.|md};
    wrapper = true;
    show_report = false;
    version = 14;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           {hz|let double_all : [Int] -> [Int] = in
double_all|hz});
    hidden_tests =
      {
        tests =
          Option.get
            (Haz3lcore.Parser.to_zipper
               {hz|test answer([1, 2, 3]) == [2, 4, 6] end;
test answer([]) == [] end;
test answer([5]) == [10] end|hz});
        hints =
          [
            "double_all([1, 2, 3]) should be [2, 4, 6]";
            "double_all([]) should be []";
            "double_all([5]) should be [10]";
          ];
      };
    display_hint = "Use map with a function that doubles each element";
    task_reference =
      {md|## Quick Reference

### map
```
map([1, 2, 3], fun x -> x + 1)
```
evaluates to `[2, 3, 4]`

`map : ([T], T -> U) -> [U]`|md};
  }

let fold_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000015-0015-0015-0015-000000000015");
    title = "Folding Lists";
    module_name = "Tu_ListFold";
    prompt =
      {md|The built-in function `fold_left` processes a list element by element, accumulating a result. It has the signature `fold_left : ([T], (U, T) -> U, U) -> U`.

The third argument is the initial accumulator value, and the second argument is a function that takes the current accumulator and the next element and returns a new accumulator.

For example, `fold_left([1, 2, 3], fun acc, x -> acc + x, 0)` evaluates to `6`.

Hazel also has a `++` operator for concatenating strings. For example, `"hello" ++ " world"` evaluates to `"hello world"`.

Implement a function `join` of type `[String] -> String` that concatenates all strings in a list using `fold_left` and `++`, with `""` as the initial accumulator.|md};
    wrapper = true;
    show_report = false;
    version = 15;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           {hz|let join : [String] -> String = in
join|hz});
    hidden_tests =
      {
        tests =
          Option.get
            (Haz3lcore.Parser.to_zipper
               {hz|test answer(["a", "b", "c"]) $== "abc" end;
test answer([]) $== "" end;
test answer(["hello"]) $== "hello" end|hz});
        hints =
          [
            {|join(["a", "b", "c"]) should be "abc"|};
            {|join([]) should be ""|};
            {|join(["hello"]) should be "hello"|};
          ];
      };
    display_hint = "Use fold_left with ++ to concatenate strings";
    task_reference =
      {md|## Quick Reference

### fold_left
```
fold_left([1, 2, 3], fun (acc, x) -> acc + x, 0)
```
evaluates to `6`

`fold_left : ([T], (U, T) -> U, U) -> U`

### String Concatenation
```
"hello" ++ " world"
```|md};
  }
