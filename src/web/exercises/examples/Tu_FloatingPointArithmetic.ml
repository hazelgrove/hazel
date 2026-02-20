let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "a0000004-0004-0004-0004-000000000004");
    title = "Floating Point Arithmetic";
    version = 4;
    module_name = "Tu_FloatingPointArithmetic";
    prompt =
      "Hazel distinguishes between integer and floating-point arithmetic. \
       Floating-point operators are written with a `.` suffix:\n\
       - `+.` addition\n\
       - `-.` subtraction\n\
       - `*.` multiplication\n\
       - `/.` division\n\n\
       Float literals must include a decimal point, e.g. `3.14` or `1.0`. This \
       distinction ensures type safety: you cannot accidentally mix integer \
       and float operations.\n\n\
       The editor below contains `3.0 * 2.0`, which uses the *integer* \
       multiplication operator `*` on float values, causing a type error. Fix \
       it by replacing `*` with the floating point multiplication operator.";
    display_hint = "Remember: float operators end with `.`";
    task_reference =
      "## Quick Reference\n\n\
       ### Integer Operators\n\
       - `2 + 3` \226\128\148 addition\n\
       - `5 - 1` \226\128\148 subtraction\n\
       - `4 * 3` \226\128\148 multiplication\n\
       - `10 / 3` \226\128\148 integer division\n\n\
       ### Float Operators\n\
       - `2.0 +. 3.0` \226\128\148 addition\n\
       - `5.0 -. 1.0` \226\128\148 subtraction\n\
       - `3.0 *. 2.0` \226\128\148 multiplication\n\
       - `6.0 /. 3.0` \226\128\148 division\n\n\
       Float literals need a decimal point: `3.0`, `1.`, `0.5`";
    your_impl =
      {
        selection = { focus = Left; content = []; mode = Normal };
        relatives =
          {
            siblings =
              ( [
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "daa49f74-f73c-4eab-a883-8465011a1e8c");
                      label = [ "3.0" ];
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
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "9bc3fea2-fe13-40cc-be81-228a10185407");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "60a35d5b-9067-4319-bb67-5ba339002374");
                      label = [ "*" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 27; sort = Exp },
                              { shape = Concave 27; sort = Exp } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "6f4d8056-0ad6-4b60-a8e7-2e029b61a82d");
                      content = Whitespace " ";
                    };
                ],
                [
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "fbef547a-091e-43e7-a8bf-af7bc772006e");
                      label = [ "2.0" ];
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
                ] );
            ancestors = [];
          };
        caret = Outer;
        refractors =
          {
            manuals = [];
            autos =
              {
                ids = Haz3lcore.Id.Map.empty;
                ephemerals = Haz3lcore.Id.Map.empty;
              };
            sample_cursor =
              {
                call_stack = [];
                index = -1;
                pinned_stack = None;
                indicated_call = None;
                time = None;
                seq = 0;
                step_range = None;
                pending_focus = None;
              };
          };
      };
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
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "3a9b4d86-05a2-4a07-9468-5c3fc21c778a");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "2d097476-16bf-411f-b128-dc5b3c0a4901");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "f9976158-56ba-4605-8615-2389521412d4");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1c8c40f8-09f3-4667-926f-266e932fb6eb");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "88982168-097b-4089-8392-b360207ec1dc");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "387f90fd-9ea4-4b94-8c66-19d44ded130d");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "0b525eda-a240-4bdd-9ddf-89b52d25ae8a");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "111c2155-2632-4e61-92c0-170a0a4de1d9");
                                    content = Whitespace " ";
                                  };
                              ];
                            ];
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "f1558eea-fa9f-46c3-a52d-78dd17eaad17");
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
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "3ac5e148-1adc-405a-b967-3047b361aa3f");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "84123871-9ddf-4683-a591-bad76d423227");
                          shape = Convex;
                        };
                    ] );
                ancestors = [];
              };
            caret = Outer;
            refractors =
              {
                manuals = [];
                autos =
                  {
                    ids = Haz3lcore.Id.Map.empty;
                    ephemerals = Haz3lcore.Id.Map.empty;
                  };
                sample_cursor =
                  {
                    call_stack = [];
                    index = -1;
                    pinned_stack = None;
                    indicated_call = None;
                    time = None;
                    seq = 0;
                    step_range = None;
                    pending_focus = None;
                  };
              };
          };
        hints =
          [ "Use `3.0 *. 2.0` with the float multiplication operator `*.`." ];
      };
    wrapper = true;
    show_report = false;
    setting_overrides = Tutorial.default_setting_overrides;
  }
