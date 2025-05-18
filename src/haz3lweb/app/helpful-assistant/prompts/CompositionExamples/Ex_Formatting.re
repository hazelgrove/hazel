module Sexp = Sexplib.Sexp;

let self = [
  "Example 1: Simply adding more variables, but properly formatting",
  "*Standard Prompt*",
  {| PROGRAM SKETCH: let x : Int = 3 in ?
   STATIC ERRORS:
   No static errors found

   USER MESSAGE/REQUEST: please write more variables |},
  {|I will add more variables to the program.
```goto_body x```
```edit let y : Int = 5 in let z : Int = x + y in z```
```view_sketch```|},
  {|PROGRAM SKETCH: let x : Int = 3 in let y : Int = 5 in let z : Int = x + y in z
STATIC ERRORS:
No static errors found

USER MESSAGE/REQUEST: SYSTEM: After your most recent edits, here is the current state of the code.|},
  {|The program now has more variables defined. I am finished with the task.
```submit```|},
  {|PROGRAM SKETCH: let x : Int = 3 in let y : Int = 5 in let z : Int = x + y in z
STATIC ERRORS:
No static errors found

USER MESSAGE/REQUEST: could you format them properly, having endlines after each \"in\" keyword?|},
  {|I will reformat the code to have endlines after each `in`.
```goto_definition x```
```edit let x : Int = 3 in
let y : Int = 5 in
let z : Int = x + y in
z```
```view_sketch```|},
  {|PROGRAM SKETCH: let x : Int = 3 in
let y : Int = 5 in
let z : Int = x + y in
z
STATIC ERRORS:
No static errors found

USER MESSAGE/REQUEST: SYSTEM: After your most recent edits, here is the current state of the code.|},
  {|The code is now formatted correctly with no duplicates. I am complete with the task.
```submit```|},
];
