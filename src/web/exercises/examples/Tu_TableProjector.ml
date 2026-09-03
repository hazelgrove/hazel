open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "3bfa855a-424e-439d-a523-89313b72c9dd");
    title = "Tables";
    module_name = "Tu_TableProjector";
    version = 1;
    prompt =
      {md|In Hazel, **tables** are represented as lists of labeled tuples. Each element of the list is a row, and each label is a column name.

```hazel
let fruits = [
(fruit="Apple", color="Red", qty=5),
(fruit="Banana", color="Yellow", qty=3)
] in
fruits
```

## The Table Projector

To help with tabular programming, Hazel provides a table **projector** that shows a literal table as a table rather than as textual syntax:

```hazel
let leaderboard = ^^table([
(level="forest", player="Aria", score=1200),
(level="desert", player="Ben", score=900),
(level="forest", player="Cleo", score=1500)
]) in
leaderboard.player
```

**Note:** this table interface is currently read-only.

## Tables in Evaluation Output

Hazel can also show tables in evaluation output. Turn on **Tables** in the settings menu at the top left, then evaluate:

```hazel
let leaderboard = ^^table([
(level="forest", player="Aria", score=1200),
(level="desert", player="Ben", score=900)
]) in
leaderboard
|> map(_, fun r -> r ... (player=r.player ++ " (Hero)"))
```

## Task

Complete the table below. It should have 2 rows:
- the first row has level `wasteland` and difficulty 3
- the second row has level `ocean` and difficulty 2

When you are done, right-click the list and select **Add Table**.|md};
    display_hint =
      "Write a list of two labeled tuples, then right-click the list and \
       select \"Add Table\"";
    task_reference =
      TaskRefDocs.compose
        [ TaskRefDocs.table_construction; TaskRefDocs.list_literal ];
    wrapper = false;
    show_report = true;
    your_impl =
      Option.get
        (Parser.to_zipper ~root:Exp
           "let table : [(level=String, difficulty=Int)] =  in\ntable");
    hidden_tests =
      {
        tests =
          Option.get
            (Parser.to_zipper ~root:Exp
               "test length(table) == 2 end;\n\
                test nth(table, 0) == (level=\"wasteland\", difficulty=3) end;\n\
                test nth(table, 1) == (level=\"ocean\", difficulty=2) end\n");
        hints = [ "Table size"; "First row"; "Second row" ];
      };
  }
