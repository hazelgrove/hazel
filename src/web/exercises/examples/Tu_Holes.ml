open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000001-0001-0001-0001-000000000001");
    title = "Holes";
    module_name = "Tu_Holes";
    prompt =
      {md|Welcome to Hazel!

Hazel is a live functional programming environment where every edit state is a valid program.

Hazel achieves this using `holes`. When part of a program is missing or contains a parse error, Hazel inserts a hole as a placeholder. Holes are displayed as hexagon in the editor and can be evaluated around.

Your goal is to fill the empty hole in the following program to complete the arithmetic expression so that the result is 42.|md};
    wrapper = true;
    show_report = false;
    version = 1;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           {hz|
let target = 42 in
let partial = 20 +   in
target == partial|hz});
    hidden_tests =
      {
        tests = Option.get (Haz3lcore.Parser.to_zipper {hz|
test answer end|hz});
        hints = [ "Replace the hole with `22` so that `partial == 42`." ];
      };
    display_hint = "Remove the extra `+` to fix the parse error";
  }
