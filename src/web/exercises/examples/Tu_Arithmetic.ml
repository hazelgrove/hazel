open Haz3lcore

let int_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000003-0003-0003-0003-000000000003");
    title = "Integer Arithmetic";
    module_name = "Tu_IntegerArithmetic";
    prompt =
      {md|Hazel supports arithmetic on integers using the operators:
- `+` addition
- `-` subtraction
- `*` multiplication
- `/` integer division

The editor below contains `2 +` with a hole after the `+`. Complete the expression by typing `2` to make `2 + 2`. It should evaluate to `4`.|md};
    wrapper = true;
    show_report = false;
    setting_overrides = Tutorial.default_setting_overrides;
    version = 3;
    your_impl = Option.get (Haz3lcore.Parser.to_zipper "2 + ");
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
                                    label = [ "4" ];
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
        hints = [ "Type `2` after the `+` to complete the expression." ];
      };
    display_hint = "Complete `2 + ⬣` by typing `2`";
    task_reference =
      {md|## Quick Reference

### Integer Operators
- `2 + 3` — addition
- `5 - 1` — subtraction
- `4 * 3` — multiplication
- `10 / 3` — integer division|md};
  }

let float_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000004-0004-0004-0004-000000000004");
    title = "Floating Point Arithmetic";
    module_name = "Tu_FloatingPointArithmetic";
    prompt =
      {md|Hazel distinguishes between integer and floating-point arithmetic. Floating-point operators are written with a `.` suffix:
- `+.` addition
- `-.` subtraction
- `*.` multiplication
- `/.` division

Float literals must include a decimal point, e.g. `3.14` or `1.0`. This distinction ensures type safety — you cannot accidentally mix integer and float operations.

The editor below contains `3.0 * 2.0`, which uses the *integer* multiplication operator `*` on float values, causing a type error. Fix it by changing `*` to `*.` (the float multiplication operator). The result should evaluate to `6.0`.|md};
    wrapper = true;
    show_report = false;
    setting_overrides = Tutorial.default_setting_overrides;
    version = 4;
    your_impl = Option.get (Haz3lcore.Parser.to_zipper "3.0 * 2.0");
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
                                    label = [ "==." ];
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
                                    label = [ "6." ];
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
        hints =
          [ "Use `3.0 *. 2.0` with the float multiplication operator `*.`." ];
      };
    display_hint = "Remember: float operators end with `.`";
    task_reference =
      {md|## Quick Reference

### Integer Operators
- `2 + 3` — addition
- `5 - 1` — subtraction
- `4 * 3` — multiplication
- `10 / 3` — integer division

### Float Operators
- `2.0 +. 3.0` — addition
- `5.0 -. 1.0` — subtraction
- `3.0 *. 2.0` — multiplication
- `6.0 /. 3.0` — division

Float literals need a decimal point: `3.0`, `1.`, `0.5`|md};
  }
