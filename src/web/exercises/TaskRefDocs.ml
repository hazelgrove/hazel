let column_projection =
  {md|### Column Projection
Access a column from a table (list of labeled tuples):
```hazel
let t = [
(name="A", score=90),
(name="B", score=80)
] in
t.score
```
|md}

let table_construction =
  {md|### Table Construction
A table is a list of labeled tuples:
```hazel
let t = [
(name="A", score=90),
(name="B", score=80)
] in
t
```
|md}

let partial_application =
  {md|### Partial Application
Use `_` to defer arguments:
```hazel
let double =
map(_, fun x -> x * 2)
in
double([1, 2, 3])
```|md}

let multi_argument_functions =
  {md|### Multi-argument functions
```hazel
let sum =
fun (x, y) -> x + y
in
sum(1, 2)
```|md}

let map =
  {md|### map
```hazelnostatics
map : ([T], T -> U) -> [U]
```
Apply a function to each element:
```hazel
map([1, 2, 3], fun x -> x + 1)
```
|md}

let fold_left =
  {md|### fold_left
```hazel
fold_left(
  [1, 2, 3],
  fun (acc, x) -> acc + x,
  0
)
```
```hazelnostatics
fold_left : ([T], (U, T) -> U, U) -> U
```|md}

let length =
  {md|### length
`length : [T] -> Int` return the length of a list
```hazel
length([1.0, 2.0, 3.0])
```|md}

let integer_arithmetic =
  {md|### Integer Arithmetic
- `2 + 3` — addition
- `5 - 1` — subtraction
- `4 * 3` — multiplication
- `10 / 3` — integer division|md}

let float_arithmetic =
  {md|### Float Arithmetic
- `2.0 +. 3.0` — addition
- `5.0 -. 1.0` — subtraction
- `3.0 *. 2.0` — multiplication
- `6.0 /. 3.0` — division

Float literals need a decimal point: `0.`, `1.0`, `3.14`|md}

let type_conversions_float_of_int =
  {md|### Type Conversions
- `float_of_int : Int -> Float` converts an integer to a float
```hazel
float_of_int(1)
```|md}

let type_conversions_full =
  {md|### Type Conversions
- `int_of_string : String -> Int`
- `float_of_string : String -> Float`
- `float_of_int : Int -> Float`|md}

let type_conversions_string_float =
  {md|### Type Conversions
- `float_of_string : String -> Float` — converts a string to a float
- `float_of_int : Int -> Float` — converts an integer to a float|md}

let type_conversions_float_int =
  {md|### Type Conversions
- `float_of_int(n)` — convert an `Int` to a `Float`
- `int_of_float(x)` — convert a `Float` to an `Int`|md}

let tuple_projection =
  {md|### Labeled Tuple Projection
Access a field of a labeled tuple with `.`:
```hazel
let pet =
(name="Fido", 
age=4, 
species="dog") in
pet.name
```
|md}

let tuple_extension =
  {md|### Tuple Extension
Use `...` to update or add fields to a labeled tuple:
```hazel
let pet = (name="Spot", age=7) in
pet ... (age=8, breed="Pug")
```
|md}

let let_expression =
  {md|### Let Expression
```hazel
let x = 5 in
let y = x + 1 in
y
```

Variables bound by `let` are available in the body after `in`.|md}

let function_definition =
  {md|### Function Definition
```hazel
fun n -> n + 1 # Increments n by 1 #
```|md}

let binding_and_calling =
  {md|### Binding and Calling a Function
```hazel
let inc : Int -> Int =
fun n -> n + 1
in
inc(1)
```|md}

let string_concatenation =
  {md|### String Concatenation
```hazel
"hello" ++ " world"
```|md}

let pipelining =
  {md|### Reverse function application and pipelining

```hazel
5
|> (fun x -> x * 2)
|> (fun x -> x + 1)
```|md}

let filter =
  {md|### filter
```hazelnostatics
filter : ([T], T -> Bool) -> [T]
```
Keep only elements that satisfy a predicate:
```hazel
filter(
  [1, 2, 3, 4, 5],
  fun x -> x > 2
)
```|md}

let list_literal = {md|### List Literal
```hazelnoeval
[1, 2, 3]
```|md}

let list_type = {md|### List Type
`[Int]`, `[String]`, `[Bool]`|md}

let zip =
  {md|### zip
```hazelnostatics
zip : ([T], [U]) -> [(T, U)]
```
Combine two lists element-wise into a list of pairs:
```hazel
zip([1, 2, 3], ["a", "b", "c"])
```
|md}

let find =
  {md|### find
```hazelnostatics
find : ([T], T -> Bool) -> T
```
Return the first element satisfying a predicate:
```hazel
find(
  [
    (name="A", score=90),
    (name="B", score=80)
  ],
  fun r -> r.name == "B"
)
```|md}

let to_lvs =
  {md|### to_lvs
```hazelnostatics
to_lvs : ? -> [
(label=String, value=?)
]
```
Convert a record to a list of label-value pairs:
```hazel
to_lvs(
  (first="John", last="Doe")
)
```|md}

let from_lvs =
  {md|### from_lvs
```hazelnostatics
from_lvs : [
(label=String, value=?)
] -> ?
```
Convert a list of label-value pairs back into a record:
```hazel
from_lvs([
(label="x", value=1),
(label="y", value=2)
])
```|md}

let group_on_key =
  {md|### group_on_key
```hazelnostatics
group_on_key : ([T], T -> K) -> [(key=K, group=[T])]
```
Group rows by the key returned by a function:
```hazel
group_on_key(
[
(name="A", dept="CS"),
(name="B", dept="CS"),
(name="C", dept="Math")
],
fun r -> r.dept
)
```|md}

let pivot_table =
  {md|### pivot_table
```hazelnostatics
pivot_table : ([T], T -> R, T -> C, [T] -> V) -> [?]
```
Pivot a table by grouping on row and column keys, aggregating each group with `agg`:
```hazelnostatics
pivot_table(
  table,
  row_key,
  col_key,
  agg
)
```|md}

let filteri =
  {md|### filteri
```hazelnostatics
filteri : ([T], (Int, T) -> Bool) -> [T]
```
Keep elements where the predicate on `(index, element)` is true:
```hazel
filteri(
  ["a", "b", "c", "d"],
  fun (i, _) -> int_mod(i, 2) == 0
)
```|md}

let int_mod =
  {md|### int_mod
```hazelnostatics
int_mod : (Int, Int) -> Int
```
Integer modulo (remainder):
```hazel
int_mod(7, 3)
```|md}

let dynamic_type =
  {md|### The Dynamic Type `?`
The type `?` (called the dynamic type or unknown type) allows a value to hold any type:
```hazelnoeval
let x : ? = 1 in
let y : ? = "hello" in
x
```
At runtime, the actual type is tracked. Hazel's **live typing** uses these runtime types to refine `?` into more specific types shown in green.|md}

let if_expression =
  {md|### If Expression
```hazel
let x = -5 in
if x < 0 then 0 - x else x
```

Nested:
```hazel
let x = 5 in
if x < 0 then -1
else if x == 0 then 0
else 1
```|md}

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
   | (n, s) =>
  "number "
  ++ string_of_int(n)
  ++ " and string "
  ++ s
   end
   ```
   |md}

let comparison_operators =
  {md|### Comparison Operators
- `==` — equal
- `<` — less than
- `>` — greater than
- `<=` — less than or equal
- `>=` — greater than or equal|md}

let compose sections = "## Quick Reference\n\n" ^ String.concat "\n\n" sections
