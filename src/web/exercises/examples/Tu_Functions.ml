open Haz3lcore

let def_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000006-0006-0006-0006-000000000006");
    title = "Functions";
    module_name = "Tu_Functions";
    prompt =
      {md|Functions are expressions that take inputs and produce outputs. In Hazel, you write an anonymous function as `fun x -> body`, where `x` is the parameter and `body` is an expression that can use `x`.

The editor below contains a function triple with the function body missing. The function is then applied to 10.

Complete the function so that the result evaluates to 30.|md};
    wrapper = true;
    show_report = false;
    version = 6;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           {hz|let triple : Int -> Int = fun n -> in
triple(10)|hz});
    hidden_tests =
      {
        tests =
          {
            selection = { focus = Left; content = []; mode = Normal };
            relatives =
              {
                siblings =
                  ( [
                      Tile
                        {
                          id = Id.mk ();
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "answer" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "30" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary { id = Id.mk (); content = Whitespace "\n" };
                    ],
                    [ Grout { id = Id.mk (); shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
            refractors = Haz3lcore.ZipperBase.Refractor.init;
          };
        hints = [ "Multiply n by 3." ];
      };
    display_hint = "Complete the function body and apply it";
  }

let call_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000007-0007-0007-0007-000000000007");
    title = "Multi-argument functions";
    module_name = "Tu_FunctionMultiArg";
    prompt =
      {md|Hazel does not support currying but supports multi-argument functions via tuples — `fun (x, y) -> x + y` is a function that takes two arguments and sums them. You apply a multi-argument function as `f(arg1, arg2)`.

Please define a perimeter function below that takes 2 arguments `width` and `height` and returns the perimiter of a rectangle `2×(width+height)`.
|md};
    wrapper = true;
    show_report = false;
    version = 7;
    your_impl =
      Option.get
        (Haz3lcore.Parser.to_zipper
           {hz|let perimeter : (Int, Int) -> Int = in
perimeter|hz});
    hidden_tests =
      {
        tests =
          Option.get
            (Haz3lcore.Parser.to_zipper
               {hz|test answer(1, 1) == 4 end;
test answer(5, 10) == 30 end;
test answer(10, 2) == 24 end|hz});
        hints =
          [
            "Perimeter of 1 and 1 is 4";
            "Perimeter of 5 and 10 is 30";
            "Perimeter of 10 and 2 is 24";
          ];
      };
    display_hint = "Insert a function with params (width, height) ";
  }

let pipeline_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000008-0008-0008-0008-000000000008");
    title = "Pipelines and Partial Application";
    module_name = "Tu_Pipelines";
    prompt =
      {md|The pipeline operator `|>` feeds a value into a function on its right:
- `5 |> fun x -> x + 1` evaluates to `6`
- This is equivalent to `(fun x -> x + 1)(5)` but reads left-to-right

You can chain pipelines: `5 |> fun x -> x + 1 |> fun x -> x * 2` first adds 1 (giving `6`), then doubles (giving `12`).

The editor below contains `10 |>` with a hole after the pipe. Complete it with a function that doubles its input, e.g. `fun x -> x * 2`. The result should be `20`.|md};
    wrapper = true;
    show_report = false;
    version = 8;
    your_impl = Option.get (Haz3lcore.Parser.to_zipper "10 |> ");
    hidden_tests =
      {
        tests =
          {
            selection = { focus = Left; content = []; mode = Normal };
            relatives =
              {
                siblings =
                  ( [
                      Tile
                        {
                          id = Id.mk ();
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "answer" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "20" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary { id = Id.mk (); content = Whitespace "\n" };
                    ],
                    [ Grout { id = Id.mk (); shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
            refractors = Haz3lcore.ZipperBase.Refractor.init;
          };
        hints = [ "Type `fun x -> x * 2` after the `|>`." ];
      };
    display_hint = "Complete the pipeline with a doubling function";
  }
