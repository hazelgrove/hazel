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
   - `2 + 3` \226\128\148 addition\n\
   - `5 - 1` \226\128\148 subtraction\n\
   - `4 * 3` \226\128\148 multiplication\n\
   - `10 / 3` \226\128\148 integer division"

let float_arithmetic =
  "### Float Arithmetic\n\
   - `2.0 +. 3.0` \226\128\148 addition\n\
   - `5.0 -. 1.0` \226\128\148 subtraction\n\
   - `3.0 *. 2.0` \226\128\148 multiplication\n\
   - `6.0 /. 3.0` \226\128\148 division\n\n\
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
   - `float_of_string : String -> Float` \226\128\148 converts a string to a \
   float\n\
   - `float_of_int : Int -> Float` \226\128\148 converts an integer to a float"

let type_conversions_float_int =
  "### Type Conversions\n\
   - `float_of_int(n)` \226\128\148 convert an `Int` to a `Float`\n\
   - `int_of_float(x)` \226\128\148 convert a `Float` to an `Int`"

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

let compose sections = "## Quick Reference\n\n" ^ String.concat "\n\n" sections
