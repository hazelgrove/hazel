let column_projection =
  "### Column Projection\n\
   Access a column from a table (list of labeled tuples):\n\
   ```hazel\n\
   let t = [(name=\"A\", score=90), (name=\"B\", score=80)] in\n\
   t.score\n\
   ```\n"

let partial_application =
  "### Partial Application\n\
   Use `_` to defer arguments:\n\
   ```hazel\n\
   let double = map(_, fun x -> x * 2) in\n\
   double([1, 2, 3])\n\
   ```"

let multi_argument_functions =
  "### Multi-argument functions\n\
   ```hazel\n\
   let sum = fun (x, y) -> x + y in\n\
   sum(1, 2)\n\
   ```"

let map =
  "### map\n\
   ```hazelnostatics\n\
   map : ([T], T -> U) -> [U]\n\
   ```\n\
   Apply a function to each element:\n\
   ```hazel\n\
   map([1, 2, 3], fun x -> x + 1)\n\
   ```\n"

let fold_left =
  "### fold_left\n\
   ```hazel\n\
   fold_left([1, 2, 3], fun (acc, x) -> acc + x, 0)\n\
   ```\n\
   `fold_left : ([T], (U, T) -> U, U) -> U`"

let length =
  "### length\n\
   `length : [T] -> Int` return the length of a list\n\
   ```hazel\n\
   length([1.0, 2.0, 3.0])\n\
   ```"

let integer_arithmetic =
  "### Integer Arithmetic\n\
   - `2 + 3` — addition\n\
   - `5 - 1` — subtraction\n\
   - `4 * 3` — multiplication\n\
   - `10 / 3` — integer division"

let float_arithmetic =
  "### Float Arithmetic\n\
   - `2.0 +. 3.0` — addition\n\
   - `5.0 -. 1.0` — subtraction\n\
   - `3.0 *. 2.0` — multiplication\n\
   - `6.0 /. 3.0` — division\n\n\
   Float literals need a decimal point: `0.`, `1.0`, `3.14`"

let type_conversions_float_of_int =
  "### Type Conversions\n\
   - `float_of_int : Int -> Float` converts an integer to a float\n\
   ```hazel\n\
   float_of_int(1)\n\
   ```"

let type_conversions_full =
  "### Type Conversions\n\
   - `int_of_string : String -> Int`\n\
   - `float_of_string : String -> Float`\n\
   - `float_of_int : Int -> Float`"

let type_conversions_string_float =
  "### Type Conversions\n\
   - `float_of_string : String -> Float` — converts a string to a float\n\
   - `float_of_int : Int -> Float` — converts an integer to a float"

let type_conversions_float_int =
  "### Type Conversions\n\
   - `float_of_int(n)` — convert an `Int` to a `Float`\n\
   - `int_of_float(x)` — convert a `Float` to an `Int`"

let tuple_projection =
  "### Labeled Tuple Projection\n\
   Access a field of a labeled tuple with `.`:\n\
   ```hazel\n\
   let pet = (name=\"Fido\", age=4, species=\"dog\") in\n\
   pet.name\n\
   ```\n"

let tuple_extension =
  "### Tuple Extension\n\
   Use `...` to update or add fields to a labeled tuple:\n\
   ```hazel\n\
   let pet = (name=\"Spot\", age=7) in\n\
   pet ... (age=8, breed=\"Pug\")\n\
   ```\n"

let let_expression =
  "### Let Expression\n\
   ```hazel\n\
   let x = 5 in\n\
   let y = x + 1 in\n\
   y\n\
   ```\n\n\
   Variables bound by `let` are available in the body after `in`."

let function_definition =
  "### Function Definition\n```hazel\nfun n -> n + 1 # Increments n by 1 #\n```"

let binding_and_calling =
  "### Binding and Calling a Function\n\
   ```hazel\n\
   let inc : Int -> Int = fun n -> n + 1 in\n\
   inc(1)\n\
   ```"

let string_concatenation =
  "### String Concatenation\n```hazel\n\"hello\" ++ \" world\"\n```"

let pipelining =
  "### Reverse function application and pipelining\n\n\
   ```hazel\n\
   5\n\
   |> (fun x -> x * 2)\n\
   |> (fun x -> x + 1) \n\
   ```"

let filter =
  "### filter\n\
   ```hazelnostatics\n\
   filter : ([T], T -> Bool) -> [T]\n\
   ```\n\
   Keep only elements that satisfy a predicate:\n\
   ```hazel\n\
   filter([1, 2, 3, 4, 5], fun x -> x > 2)\n\
   ```"

let list_literal = "### List Literal\n```hazelnoeval\n[1, 2, 3]\n```"
let list_type = "### List Type\n`[Int]`, `[String]`, `[Bool]`"

let zip =
  "### zip\n\
   ```hazelnostatics\n\
   zip : ([T], [U]) -> [(T, U)]\n\
   ```\n\
   Combine two lists element-wise into a list of pairs:\n\
   ```hazel\n\
   zip([1, 2, 3], [\"a\", \"b\", \"c\"])\n\
   ```\n"

let find =
  "### find\n\
   ```hazelnostatics\n\
   find : ([T], T -> Bool) -> T\n\
   ```\n\
   Return the first element satisfying a predicate:\n\
   ```hazel\n\
   find([(name=\"A\", score=90), (name=\"B\", score=80)], fun r -> r.name == \
   \"B\")\n\
   ```"

let to_lvs =
  "### to_lvs\n\
   ```hazelnostatics\n\
   to_lvs : T -> [(label=String, value=?)]\n\
   ```\n\
   Convert a record to a list of label-value pairs:\n\
   ```hazel\n\
   to_lvs((name=\"A\", score=90))\n\
   ```"

let from_lvs =
  "### from_lvs\n\
   ```hazelnostatics\n\
   from_lvs : [(label=String, value=?)] -> T\n\
   ```\n\
   Convert a list of label-value pairs back into a record:\n\
   ```hazel\n\
   from_lvs([(label=\"x\", value=1), (label=\"y\", value=2)])\n\
   ```"

let group_on_key =
  "### group_on_key\n\
   ```hazelnostatics\n\
   group_on_key : ([T], T -> K) -> [(key=K, group=[T])]\n\
   ```\n\
   Group rows by the key returned by a function:\n\
   ```hazel\n\
   group_on_key([(name=\"A\", dept=\"CS\"), (name=\"B\", dept=\"CS\"), \
   (name=\"C\", dept=\"Math\")], fun r -> r.dept)\n\
   ```"

let pivot_table =
  "### pivot_table\n\
   ```hazelnostatics\n\
   pivot_table : ([T], T -> R, T -> C, [T] -> V) -> [?]\n\
   ```\n\
   Pivot a table by grouping on row and column keys, aggregating each group \
   with `agg`:\n\
   ```hazelnostatics\n\
   pivot_table(table, row_key, col_key, agg)\n\
   ```"

let filteri =
  "### filteri\n\
   ```hazelnostatics\n\
   filteri : ([T], (Int, T) -> Bool) -> [T]\n\
   ```\n\
   Keep elements where the predicate on `(index, element)` is true:\n\
   ```hazel\n\
   filteri([\"a\", \"b\", \"c\", \"d\"], fun (i, _) -> int_mod(i, 2) == 0)\n\
   ```"

let int_mod =
  "### int_mod\n\
   ```hazelnostatics\n\
   int_mod : (Int, Int) -> Int\n\
   ```\n\
   Integer modulo (remainder):\n\
   ```hazel\n\
   int_mod(7, 3)\n\
   ```"

let dynamic_type =
  "### The Dynamic Type `?`\n\
   The type `?` (called the dynamic type or unknown type) allows a value to \
   hold any type:\n\
   ```hazelnoeval\n\
   let x : ? = 1 in\n\
   let y : ? = \"hello\" in\n\
   x\n\
   ```\n\
   At runtime, the actual type is tracked. Hazel's **live typing** uses these \
   runtime types to refine `?` into more specific types shown in green."

let if_expression =
  "### If Expression\n\
   ```hazel\n\
   let x = -5 in\n\
   if x < 0 then 0 - x else x\n\
   ```\n\n\
   Nested:\n\
   ```hazel\n\
   let x = 5 in\n\
   if x < 0 then -1\n\
   else if x == 0 then 0\n\
   else 1\n\
   ```"

let case_expression =
  {md|### Case Expression
   ```hazel
   case 1
   | 0 => "zero"
   | 1 => "one"
   | _ => "other"
   end
   ```
   `_` is a wildcard that matches anything.
   
   Pattern matching can also destructure values:
   ```hazel
   case (1, "hello")
   | (0, _) => "zero and something"
   | (n, s) => "number " ++ string_of_int(n) ++ " and string " ++ s
   end
   ```
   |md}

let comparison_operators =
  "### Comparison Operators\n\
   - `==` — equal\n\
   - `<` — less than\n\
   - `>` — greater than\n\
   - `<=` — less than or equal\n\
   - `>=` — greater than or equal"

let compose sections = "## Quick Reference\n\n" ^ String.concat "\n\n" sections
