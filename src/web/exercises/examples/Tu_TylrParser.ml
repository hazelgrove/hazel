let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "a0000002-0002-0002-0002-000000000002");
    title = "The Tylr Parser and Backpack";
    version = 2;
    module_name = "Tu_TylrParser";
    prompt =
      "Hazel uses a tile-based parser called Tylr. When you type a multi-token \
       form like `let`, Tylr automatically tracks the remaining delimiters \
       that are needed to complete the syntactic form. These obligations are \
       held in the *backpack*, shown in yellow above the cursor.\n\n\
       The editor below already contains `let x =` with a hole automatically \
       placed after the `=`. Notice that the `in` delimiter is in the backpack \
       \226\128\148 it still needs to be *dropped* into the program.\n\n\
       Complete the expression step by step:\n\
       1. Type `1` to fill in the value for `x`\n\
       2. Press `Tab` or type `in` to drop the `in` delimiter from the backpack\n\
       3. Type `x + 1` as the body of the let expression\n\n\
       The result should evaluate to **2**.";
    display_hint = "Type a let expression and use Tab to complete obligations";
    task_reference =
      "## Quick Reference\n\n\
       ### Let Expression\n\
       ```hazel\n\
       let a = 1 in\n\
       a\n\
       ```\n\n\
       ### Backpack\n\
       - **Tab** drops the next obligation from the backpack\n\
       - Typing the delimiter (e.g. `in`) also works";
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
                             "01d533a7-cabf-4ef0-a886-756f25e324ce");
                      label = [ "let"; "="; "in" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat; Exp ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 45; sort = Exp } );
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
                                       "23058703-200a-479a-abe3-0fc29cb2b404");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "cc8e9d92-8558-4baf-b8d0-a5e4b4fe0cbe");
                                label = [ "x" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Pat },
                                        { shape = Convex; sort = Pat } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "514eef19-149e-483e-af0a-2dd20a03ced7");
                                content = Whitespace " ";
                              };
                          ];
                        ];
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "fec8095e-5e9f-4f8c-b152-dc5a6e116fd7");
                      content = Whitespace " ";
                    };
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "65ca6f06-4967-443d-ad5a-d536f632a023");
                      shape = Convex;
                    };
                ],
                [] );
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
                                 "7aca2284-5b0f-4cd0-bac8-eb7c8f020d4d");
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
                                           "44e65ce9-ee36-49c0-a4c9-381e05aad28a");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "aa4d1165-b1d3-4d3f-863b-17a865796920");
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
                                           "2d3fcc29-fe9e-46b6-999a-824ec493ee4b");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "56edecb1-8582-472e-a4af-e2649f557abb");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "747d6fc3-1210-4fbb-8e83-9f44ee15e7b8");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "559cae06-1df0-4ea0-9aca-0a08b0b05b66");
                                    label = [ "2" ];
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
                                           "601190e5-e256-4e8d-bdd2-ab7360b149f2");
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
                                 "ff7d81bb-0904-4caa-920e-68b87aa08e13");
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
                                 "c8bac952-1b24-45a7-91c9-94d16c65fd0f");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "112e15cb-6d6f-4947-9a40-5402075b52db");
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
          [
            "Type `let x = 1 in x + 1` using Tab to drop backpack obligations.";
          ];
      };
    wrapper = true;
    show_report = false;
    rich_probes = Some false;
  }
