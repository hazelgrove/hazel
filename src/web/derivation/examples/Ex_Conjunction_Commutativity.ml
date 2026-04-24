let exercise : DerivationExercise.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "1040866d-20f7-42e3-96c4-a9d9a4b239d3");
    title = "conjunction commutativity";
    module_name = "conjunction commutativity";
    prompt = "TODO: prompt";
    max_points = 10;
    prelude =
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
                             "4c69a081-2240-4181-83cf-b5c1ea1f51cd");
                      shape = Convex;
                    };
                ] );
            ancestors = [];
          };
        caret = Outer;
        refractors =
          {
            manuals = [];
            multis =
              {
                ids = Haz3lcore.Id.Map.empty;
                suppressed = Haz3lcore.Id.Map.empty;
                ephemerals = Haz3lcore.Id.Map.empty;
              };
            sample_focus =
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
            autoprobe_target = None;
            pending_probe_cursor = None;
          };
      };
    setup =
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
                             "f5f267c5-c9b6-4740-b02e-abeb362d583c");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "14226aa0-2862-4c95-a0b6-cebde4adbbe0");
                      label = [ "of_ctx"; "end" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Drv Exp ];
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
                                       "24ac9c68-9205-40f5-9878-e6250c433b8a");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "6b6566a1-8a5b-4c99-8a99-7f8a40a9b679");
                                label = [ "["; "]" ];
                                mold =
                                  {
                                    out = Drv Exp;
                                    in_ = [ Drv Exp ];
                                    nibs =
                                      ( { shape = Convex; sort = Drv Exp },
                                        { shape = Convex; sort = Drv Exp } );
                                  };
                                shards = [ 0; 1 ];
                                children =
                                  [
                                    [
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "cd402575-1168-43e3-a619-d9c08b513d67");
                                          label = [ "A" ];
                                          mold =
                                            {
                                              out = Drv Exp;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Convex;
                                                    sort = Drv Exp;
                                                  },
                                                  {
                                                    shape = Convex;
                                                    sort = Drv Exp;
                                                  } );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "ec73ec1c-d5fe-43b7-bea0-04307e2c2783");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "9ca379a3-a16c-4937-a07b-9f12b70d103b");
                                          label = [ "/\\" ];
                                          mold =
                                            {
                                              out = Drv Exp;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 32;
                                                    sort = Drv Exp;
                                                  },
                                                  {
                                                    shape = Concave 32;
                                                    sort = Drv Exp;
                                                  } );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "6ac030e4-cdce-4d13-86f3-a9f9f38b442d");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "f364c234-e30e-48be-8708-e6bf9e4607fe");
                                          label = [ "B" ];
                                          mold =
                                            {
                                              out = Drv Exp;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Convex;
                                                    sort = Drv Exp;
                                                  },
                                                  {
                                                    shape = Convex;
                                                    sort = Drv Exp;
                                                  } );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                    ];
                                  ];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "0113a553-894c-44d4-8082-9ad7eb992070");
                                content = Whitespace " ";
                              };
                          ];
                        ];
                    };
                ],
                [
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "a1c6c2fd-d791-43d1-ba8c-86aabe6e44d2");
                      content = Whitespace " ";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "7842558f-e62b-4a58-b2c8-dc806662a6de");
                    label = [ "let"; "="; "in" ];
                    mold =
                      {
                        out = Exp;
                        in_ = [ Pat; Exp ];
                        nibs =
                          ( { shape = Convex; sort = Exp },
                            { shape = Concave 45; sort = Exp } );
                      };
                    shards = ([ 0; 1 ], [ 2 ]);
                    children =
                      ( [
                          [
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "bea86d04-5f72-487e-9684-ba0c7e04c132");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "38744591-0b67-4c98-b448-a66070ef5183");
                                label = [ "$ab" ];
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
                                       "a904b1b7-60df-47a5-999b-9d5e7d0dffe0");
                                content = Whitespace " ";
                              };
                          ];
                        ],
                        [] );
                  },
                  ( [],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "b1992981-315b-4893-88b1-9df7aeff0074");
                          content = Whitespace " ";
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "21708ae6-e171-40d7-8c4e-4218d06dd3cb");
                          shape = Convex;
                        };
                    ] ) );
              ];
          };
        caret = Outer;
        refractors =
          {
            manuals = [];
            multis =
              {
                ids = Haz3lcore.Id.Map.empty;
                suppressed = Haz3lcore.Id.Map.empty;
                ephemerals = Haz3lcore.Id.Map.empty;
              };
            sample_focus =
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
            autoprobe_target = None;
            pending_probe_cursor = None;
          };
      };
    corpus = PropositionalLogic;
    trees =
      [
        Node
          ( Just
              {
                jdmt =
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
                                         "da20749c-e2dc-4863-b1cc-f4fa23c94eba");
                                  label = [ "$ab" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "bb1399f6-1adc-4635-b834-2bc948d8e430");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "c397515f-8b90-4b2b-95bf-656a0ce0f472");
                                  label = [ "|-" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 49; sort = Drv Exp },
                                          { shape = Concave 49; sort = Drv Exp }
                                        );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a4d76acc-3b98-4e68-88b8-c442068d4072");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "5c611d77-fc5b-45e9-b1cc-7f2245228bfc");
                                  label = [ "A" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "ea15f133-a94c-4d7c-8b43-ce8dc0f1cd82");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "fa7d0d50-a1c6-43de-a415-ab6681580b46");
                                  label = [ "/\\" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 32; sort = Drv Exp },
                                          { shape = Concave 32; sort = Drv Exp }
                                        );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "89199e26-9aa6-4527-9e96-4a36ea60e80f");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "583dd1f3-43a5-43bd-965a-e5452c55b009");
                                  label = [ "B" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                            ],
                            [] );
                        ancestors = [];
                      };
                    caret = Outer;
                    refractors =
                      {
                        manuals = [];
                        multis =
                          {
                            ids = Haz3lcore.Id.Map.empty;
                            suppressed = Haz3lcore.Id.Map.empty;
                            ephemerals = Haz3lcore.Id.Map.empty;
                          };
                        sample_focus =
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
                        autoprobe_target = None;
                        pending_probe_cursor = None;
                      };
                  };
                rule = Some Assumption;
              },
            [] );
        Node
          ( Just
              {
                jdmt =
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
                                         "ff4d6597-e822-4ce9-9993-954ed5e9f61f");
                                  label = [ "[]" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "ded43e14-cafa-49fe-8544-1a2d5f79d243");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "205348ae-89f6-40b6-86d7-600767755926");
                                  label = [ "|-" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 49; sort = Drv Exp },
                                          { shape = Concave 49; sort = Drv Exp }
                                        );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "8c01e8f7-c9d1-43c6-87ac-154c14e6cd10");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "04ed94bf-37c3-4a81-9328-2ab96fe9152d");
                                  label = [ "("; ")" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [ Drv Exp ];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0; 1 ];
                                  children =
                                    [
                                      [
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "ee2724d5-85ec-4a43-a0ba-a47b7a7de5b7");
                                            label = [ "A" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    } );
                                              };
                                            shards = [ 0 ];
                                            children = [];
                                          };
                                        Secondary
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "a24dad24-654f-4d1f-94d1-aa7089a4123d");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "697b8973-339e-4fb9-aa9f-9d7fb3cfebe8");
                                            label = [ "/\\" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Concave 32;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Concave 32;
                                                      sort = Drv Exp;
                                                    } );
                                              };
                                            shards = [ 0 ];
                                            children = [];
                                          };
                                        Secondary
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "c66f16a5-fc5c-4144-8160-8d6554a65271");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "53134886-43b9-4616-ba86-311c99ad90f9");
                                            label = [ "B" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    } );
                                              };
                                            shards = [ 0 ];
                                            children = [];
                                          };
                                      ];
                                    ];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "f12dcbfa-2d10-4c2d-9ecf-a315ef055ee8");
                                  content = Whitespace " ";
                                };
                            ],
                            [
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "46a63e61-09fa-4212-9d9c-0a62ad4accde");
                                  label = [ "==>" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [];
                                      nibs =
                                        ( { shape = Concave 34; sort = Drv Exp },
                                          { shape = Concave 34; sort = Drv Exp }
                                        );
                                    };
                                  shards = [ 0 ];
                                  children = [];
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "43d1773c-b073-49d2-bab8-afbb38b42ca1");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "86bee2e5-4f22-4e24-8465-410a576f8d17");
                                  label = [ "("; ")" ];
                                  mold =
                                    {
                                      out = Drv Exp;
                                      in_ = [ Drv Exp ];
                                      nibs =
                                        ( { shape = Convex; sort = Drv Exp },
                                          { shape = Convex; sort = Drv Exp } );
                                    };
                                  shards = [ 0; 1 ];
                                  children =
                                    [
                                      [
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "1817a449-1e08-46bc-892a-782c21dc6686");
                                            label = [ "B" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    } );
                                              };
                                            shards = [ 0 ];
                                            children = [];
                                          };
                                        Secondary
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "c3d70401-0744-4709-9c56-d64721a3cfef");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "fd656d01-818c-40b2-9a12-2a1a7672dd22");
                                            label = [ "/\\" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Concave 32;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Concave 32;
                                                      sort = Drv Exp;
                                                    } );
                                              };
                                            shards = [ 0 ];
                                            children = [];
                                          };
                                        Secondary
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "5262d6a8-6eb8-4206-aae7-7e9c9db4a72a");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "7a143aee-913a-4fe0-a687-3d8f817a17d9");
                                            label = [ "A" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [];
                                                nibs =
                                                  ( {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    },
                                                    {
                                                      shape = Convex;
                                                      sort = Drv Exp;
                                                    } );
                                              };
                                            shards = [ 0 ];
                                            children = [];
                                          };
                                      ];
                                    ];
                                };
                            ] );
                        ancestors = [];
                      };
                    caret = Inner 1;
                    refractors =
                      {
                        manuals = [];
                        multis =
                          {
                            ids = Haz3lcore.Id.Map.empty;
                            suppressed = Haz3lcore.Id.Map.empty;
                            ephemerals = Haz3lcore.Id.Map.empty;
                          };
                        sample_focus =
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
                        autoprobe_target = None;
                        pending_probe_cursor = None;
                      };
                  };
                rule = Some Implies_I;
              },
            [
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "17234001-11b1-4754-abd7-ea07d272dc35");
                                        label = [ "$ab" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "8f1a8c6c-410b-41e3-bcdc-452e9bb1ab5e");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "bba845f5-fd1f-49e5-a005-b5b9c9b5d75a");
                                        label = [ "|-" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 49;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 49;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "392bf20d-dcdd-416c-8fc7-2f858bd2c4d7");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "6d2085d1-cf64-45ca-9234-541150ca8114");
                                        label = [ "B" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "da79ee90-8759-42d2-acee-cca9991fa7eb");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0dafa3e8-2951-4d55-ade2-bf16b0092417");
                                        label = [ "/\\" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 32;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 32;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "20a6f4a6-4554-4b32-b393-b44dfe44f988");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "54e50bf1-6255-4435-966a-442ed97d20a6");
                                        label = [ "A" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                  ],
                                  [] );
                              ancestors = [];
                            };
                          caret = Outer;
                          refractors =
                            {
                              manuals = [];
                              multis =
                                {
                                  ids = Haz3lcore.Id.Map.empty;
                                  suppressed = Haz3lcore.Id.Map.empty;
                                  ephemerals = Haz3lcore.Id.Map.empty;
                                };
                              sample_focus =
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
                              autoprobe_target = None;
                              pending_probe_cursor = None;
                            };
                        };
                      rule = Some And_I;
                    },
                  [
                    Node
                      ( Just
                          {
                            jdmt =
                              {
                                selection =
                                  { focus = Left; content = []; mode = Normal };
                                relatives =
                                  {
                                    siblings =
                                      ( [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "6abfff12-863f-4a20-9710-2de91ccb194f");
                                              label = [ "$ab" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "2dd26d7f-2aeb-4e3d-888d-93b047a6e7f5");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "35290aa0-8c91-4c20-a524-509bb0ad7cc9");
                                              label = [ "|-" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 49;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 49;
                                                        sort = Drv Exp;
                                                      } );
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
                                                     "38a03ab0-a8be-4055-b405-4925b0a73b66");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "d4eb0dad-ebce-4bc2-950c-eafda4bc183d");
                                              label = [ "B" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      } );
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
                                    multis =
                                      {
                                        ids = Haz3lcore.Id.Map.empty;
                                        suppressed = Haz3lcore.Id.Map.empty;
                                        ephemerals = Haz3lcore.Id.Map.empty;
                                      };
                                    sample_focus =
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
                                    autoprobe_target = None;
                                    pending_probe_cursor = None;
                                  };
                              };
                            rule = Some And_E_R;
                          },
                        [ Node (Abbr (Some 0), []) ] );
                    Node
                      ( Just
                          {
                            jdmt =
                              {
                                selection =
                                  { focus = Left; content = []; mode = Normal };
                                relatives =
                                  {
                                    siblings =
                                      ( [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "f4e51531-79f7-4cc8-8ee0-0e0f24e96e1e");
                                              label = [ "$ab" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "15d071d4-3437-4c79-b701-ed1f6b466559");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "cf8cd8b1-b59a-4767-b95b-244d15e270c0");
                                              label = [ "|-" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 49;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 49;
                                                        sort = Drv Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "b7754a3c-cfa0-42c3-bff7-d3e90031eb6b");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "f063f3a7-4472-435d-8ec6-a04081bea7d8");
                                              label = [ "A" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ],
                                        [] );
                                    ancestors = [];
                                  };
                                caret = Outer;
                                refractors =
                                  {
                                    manuals = [];
                                    multis =
                                      {
                                        ids = Haz3lcore.Id.Map.empty;
                                        suppressed = Haz3lcore.Id.Map.empty;
                                        ephemerals = Haz3lcore.Id.Map.empty;
                                      };
                                    sample_focus =
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
                                    autoprobe_target = None;
                                    pending_probe_cursor = None;
                                  };
                              };
                            rule = Some And_E_L;
                          },
                        [ Node (Abbr (Some 0), []) ] );
                  ] );
            ] );
      ];
  }
