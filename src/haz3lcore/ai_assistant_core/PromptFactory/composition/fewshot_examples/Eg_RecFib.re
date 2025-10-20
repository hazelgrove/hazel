// A simple example going over how to navigate from a sibling to another sibling, and update
// a definition of a function

let self =
  // This example is outdated, but may be a good reference for
  {|
<tallyFunctionExample>

<User>Please implement the recursive fibonacci function.</User>

<User(Info)>
<AST information>
Current node: factorial
No parent node, you are at the top level of the program's AST.
Sibling nodes: [rec_fib (index: 0)]
Child nodes: []
</AST information>
<Sketch information>
Definition of "factorial"'s parent (no parent, displaying entire top level of the program):
```
let rec_fib =
  ⋱
in
let factorial = fun n ->
  ?
in
?
```
No static errors found in the program.
</Sketch information>
</User(Info)>

<Assistant>
I can help with that!
I'll start by defining the `rec_fib` function. It will be a recursive function that calculates the nth Fibonacci number.
I'll begin by updating the function's definition, but first I'll need to move to the `rec_fib` function.
<ToolCall>
go_to_sibling(
rec_fib
)
</ToolCall>
</Assistant>

<User(Info)>
<AST information>
Current node: rec_fib
No parent node, you are at the top level of the program's AST.
Sibling nodes: [factorial (index: 0)]
Child nodes: []
</AST information>
<Sketch information>
Definition of "rec_fib"'s parent (no parent, displaying entire top level of the program):
```
let rec_fib =
  ?
in
let factorial =
  ⋱
in
?
```
No static errors found in the program.
</Sketch information>
</User(Info)>

<Assistant>
Great! Now I'll update the definition of `rec_fib`.
<ToolCall>
update_definition(
code="
fun n ->
  if n <= 1
    then 1
    else rec_fib(n - 1) + rec_fib(n - 2)"
)
</ToolCall>
</Assistant>

<User(Info)>
<AST information>
Current node: rec_fib
No parent node, you are at the top level of the program's AST.
Sibling nodes: [factorial (index: 0)]
Child nodes: []
</AST information>
<Sketch information>
Definition of "rec_fib"'s parent (no parent, displaying entire top level of the program):
```
let rec_fib =
  fun n ->
  if n <= 1
    then 1
    else rec_fib(n - 1) + rec_fib(n - 2)
in
let factorial =
  ⋱
in
?
```
No static errors found in the program.
</Sketch information>
</User(Info)>

<Assistant>
Great! The recursive fibonacci function is now properly implemented.
</Assistant>

</tallyFunctionExample>
|};
// how to structure and write future few shot examples.
