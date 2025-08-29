let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "d0e1f2a3-4567-8901-2abc-def345678901");
    title = "Expressive Programming";
    version = 1;
    module_name = "Tu_ExpressiveProgramming";
    prompt =
      "You might not have realized it, but you wrote your first computer \
       programs in grade school in the form of arithmetic expressions! \n\n\
       For example, enter the program `2 + 2` in the expression editor below. \
       Hazel operates like a calculator, computing the value of your \
       expression by equationally simplifying it (i.e. evaluating it), here to \
       the integer value `4`. The symbol `\226\137\161` is pronounced \"is \
       equivalent to\".";
    display_hint = "Type 2 + 2 in the cell below \240\159\145\135";
    your_impl =
      {
        selection = { focus = Left; content = []; mode = Normal };
        relatives =
          {
            siblings =
              ( [],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "31429945-6854-4ca7-8074-ac79ecc33a22");
                      shape = Convex;
                    };
                ] );
            ancestors = [];
          };
        caret = Outer;
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
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "2ee950d9-59bd-4f9e-811d-61568975b2dd");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "a88e8cc0-7768-44c6-ae4a-82ced068855c");
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
                                 "74609467-9721-4a45-8b2c-16dc221d2a5b");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "f5dee0ac-abc5-42b1-ab4b-a0920a8c4f38");
                          label = [ "==" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 31; sort = Exp },
                                  { shape = Concave 31; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "7801bdc0-1571-444a-b8c5-5a1966e528d8");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "7030c1ee-ea52-454e-a653-cad11148a85b");
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
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "2c24f8c8-751e-4e39-8ac7-b0ef2fac38e4");
                          content = Whitespace " ";
                        };
                    ] );
                ancestors =
                  [
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "de5e2711-2a93-4bec-9560-016c741e3886");
                        label = [ "test"; "end" ];
                        mold =
                          {
                            out = Exp;
                            in_ = [ Exp ];
                            nibs =
                              ( { shape = Convex; sort = Exp },
                                { shape = Convex; sort = Exp } );
                          };
                        shards = ([ 0 ], [ 1 ]);
                        children = ([], []);
                      },
                      ( [],
                        [
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "9662b8bd-43e6-46bd-a484-a5ddbe18df3f");
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
                                     "eac2f042-fcbf-4a1d-ba1d-f17bd4c10f18");
                              content = Whitespace "\n";
                            };
                          Grout
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "0e7fa0cb-8818-4d78-a283-976413736256");
                              shape = Convex;
                            };
                        ] ) );
                  ];
              };
            caret = Outer;
          };
        hints = [ "Reread the question :)" ];
      };
    wrapper = true;
    show_report = true;
  }
