let prompt = Ex_Shadowing_And_Closures_prompt.prompt

let exercise : Exercise.spec =
  {
    header =
      {
        title = "Shadowing and Closures";
        version = 1;
        module_name = "Ex_Shadowing_And_Closures";
        prompt;
      };
    pos = Proof Prelude;
    model =
      Proof
        {
          prelude =
            {
              selection = { focus = Left; content = []; mode = Normal };
              backpack = [];
              relatives =
                {
                  siblings =
                    ( [
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "2953ee29-c864-4b6b-a488-eb01d4008ecd");
                            label = [ "let"; "="; "in" ];
                            mold =
                              {
                                out = Exp;
                                in_ = [ Pat; Exp ];
                                nibs =
                                  ( { shape = Convex; sort = Exp },
                                    { shape = Concave 17; sort = Exp } );
                              };
                            shards = [ 0; 1; 2 ];
                            children =
                              [
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "644e4b5c-d02d-4bd9-a21e-2ba18a41a11c");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "cc425dbf-a3ef-4db8-9c3c-a434ae79aca6");
                                      label = [ "e2" ];
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
                                             "4b9eee99-02f7-4c51-af93-12c68333059e");
                                      content = Whitespace " ";
                                    };
                                ];
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "886c425d-e0f6-4c69-aaf7-452ee1543c74");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "f7fdaffe-8f26-483f-9033-8a7b1f09d480");
                                      label = [ "of_alfa_exp"; "end" ];
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
                                                       "ecf8f7de-6d88-490b-8587-0769da737ed7");
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "ce70fd3a-0338-4744-8d38-93c1717437db");
                                                content = Whitespace "\n";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "6c6579b5-0211-4569-9e98-8478e6861915");
                                                label = [ "let"; "="; "in" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [ Drv Pat; Drv Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Concave 17;
                                                          sort = Drv Exp;
                                                        } );
                                                  };
                                                shards = [ 0; 1; 2 ];
                                                children =
                                                  [
                                                    [
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a01b3239-2e25-4c51-aa99-cfc58ca9ffce");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d0e06074-9859-436e-a217-9aa1fdebcc13");
                                                          label = [ "y" ];
                                                          mold =
                                                            {
                                                              out = Drv Pat;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Pat;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Pat;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f13e72de-aebc-4be4-9883-9ba86e5cd0f3");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                    ];
                                                    [
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "3c865777-b0ad-45d8-8814-f5c05207a516");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "c5ffe2a5-42c8-4103-a71c-14cc86c0926a");
                                                          label = [ "y" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "07f4e91f-7055-4ec7-9f12-94009a910c8c");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "9e80e668-aa55-407c-b70b-a3d266630db3");
                                                          label = [ "-" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 6;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 6;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "8a5c18e9-18f5-4738-b827-1308cd74c806");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "5862c980-9ad7-49e8-bca1-b224398cdebd");
                                                          label = [ "3" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "929da685-7e68-4a41-a756-6a0eb5a773d5");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                    ];
                                                  ];
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "f3a38d92-21f5-4645-b217-4565580bf071");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "9dc6b877-2c48-4c3b-8543-9920b3a55194");
                                                label = [ "f" ];
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
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "553b383e-a951-48fd-86ef-71b93a017b46");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [ Drv Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 2;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Drv Exp;
                                                        } );
                                                  };
                                                shards = [ 0; 1 ];
                                                children =
                                                  [
                                                    [
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "1a2bec5f-04b2-4342-becd-8796f4f2e4e1");
                                                          label = [ "y" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
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
                                                       "70372c8a-fea6-41cf-afb5-c64a80b536d2");
                                                content = Whitespace "\n";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "a1c2f882-870d-41ec-bebb-af74b37be8d8");
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
                                   "968d7aa2-3603-4cb9-a83c-b3bc886527a5");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "d30f793b-e102-4782-b6e1-cbf66a6cc115");
                            label = [ "let"; "="; "in" ];
                            mold =
                              {
                                out = Exp;
                                in_ = [ Pat; Exp ];
                                nibs =
                                  ( { shape = Convex; sort = Exp },
                                    { shape = Concave 17; sort = Exp } );
                              };
                            shards = [ 0; 1; 2 ];
                            children =
                              [
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "42a76b38-e63d-4425-a309-0030bd3d963f");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "7877c39c-83b6-40a0-84d2-1a728d8c4ae0");
                                      label = [ "e1" ];
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
                                             "057ecf1d-9182-4ff6-b0b6-f97e0fdff011");
                                      content = Whitespace " ";
                                    };
                                ];
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "c03cb5cd-6182-4dc6-a55e-ed66c9c3188a");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "5d6ce326-4468-4718-bb77-b9a499979103");
                                      label = [ "of_alfa_exp"; "end" ];
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
                                                       "fb125a2f-8eee-48b8-ad2b-99984bf3495a");
                                                content = Whitespace "\n";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "8fbbebee-5712-4b91-9091-494e5e0ca20b");
                                                label = [ "let"; "="; "in" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [ Drv Pat; Drv Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Concave 17;
                                                          sort = Drv Exp;
                                                        } );
                                                  };
                                                shards = [ 0; 1; 2 ];
                                                children =
                                                  [
                                                    [
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "7ac29fc5-1789-4bdb-8738-d54b838ab601");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "02b5b7b3-202c-45b1-84d8-983d9f54500a");
                                                          label = [ "f" ];
                                                          mold =
                                                            {
                                                              out = Drv Pat;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Pat;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Pat;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "ddddb87f-3048-4ef6-b4e0-7c2d91a95f83");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                    ];
                                                    [
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a04a6f09-37a9-4992-9f93-e962c132ea3e");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "211e51e6-ea29-4ce6-bbb7-e3dd5f09fb5c");
                                                          label =
                                                            [ "fun"; "->" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [ Drv Pat ];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 14;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0; 1 ];
                                                          children =
                                                            [
                                                              [
                                                                Secondary
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "c27ed853-009d-4870-8256-cc87a40c639b");
                                                                    content =
                                                                      Whitespace
                                                                        " ";
                                                                  };
                                                                Tile
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "0901f66c-a21f-42f8-8206-9fceabbf1e57");
                                                                    label =
                                                                      [ "z" ];
                                                                    mold =
                                                                      {
                                                                        out =
                                                                          Drv
                                                                            Pat;
                                                                        in_ = [];
                                                                        nibs =
                                                                          ( {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Drv
                                                                                Pat;
                                                                            },
                                                                            {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Drv
                                                                                Pat;
                                                                            } );
                                                                      };
                                                                    shards =
                                                                      [ 0 ];
                                                                    children =
                                                                      [];
                                                                  };
                                                                Secondary
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "a2c0173b-745f-43b4-bc81-8ace9b25bfc5");
                                                                    content =
                                                                      Whitespace
                                                                        " ";
                                                                  };
                                                              ];
                                                            ];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f4b38752-6986-4bc5-baba-d9a18248da01");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "51e8e8a1-3781-4943-8f86-c8524e781147");
                                                          label = [ "y" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "c415ee42-9718-4552-a510-19dcfa509071");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "6c34c2d3-a926-4ccb-adb5-abd6682ada0e");
                                                          label = [ "*" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 5;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 5;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b106cec2-34fe-4616-a1bd-0d4181daee3f");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "0f0ed193-d79a-4ee9-af39-8b5211310401");
                                                          label = [ "z" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "7e80039c-afd5-4cbe-a884-9797d2604ee2");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                    ];
                                                  ];
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "79722e01-784d-4a14-8f7e-a3bc15d50225");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "aff5b575-9f0b-43c7-a283-036b9905a0c2");
                                                label = [ "{"; "}" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [ Pat ];
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
                                                shards = [ 0; 1 ];
                                                children =
                                                  [
                                                    [
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "997516c6-5b16-4552-aec8-5869784497c4");
                                                          label = [ "e2" ];
                                                          mold =
                                                            {
                                                              out = Pat;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Pat;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Pat;
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
                                                       "83200505-3fe6-4dbc-b269-6359fa2d526f");
                                                content = Whitespace "\n";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "7fc1baa2-c5e6-47fb-817f-1aa0dbc51d9b");
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
                                   "43ea586d-6524-4ca7-95b1-d2d2967e714a");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "5546a755-566d-489b-b127-ea27d938e02f");
                            label = [ "let"; "="; "in" ];
                            mold =
                              {
                                out = Exp;
                                in_ = [ Pat; Exp ];
                                nibs =
                                  ( { shape = Convex; sort = Exp },
                                    { shape = Concave 17; sort = Exp } );
                              };
                            shards = [ 0; 1; 2 ];
                            children =
                              [
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "d1e2d460-0cbd-4b50-8321-2d961b0a6e16");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "372071b5-2405-4eef-8c76-f97d5a75b4a7");
                                      label = [ "e_example" ];
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
                                             "1678f010-ff59-4f7f-aa53-f8134cb3418b");
                                      content = Whitespace " ";
                                    };
                                ];
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "54faa5b1-23fb-44ab-ae49-7fd2f964a961");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "d03bcbdf-95de-44d8-a586-1fbed25802c7");
                                      label = [ "of_alfa_exp"; "end" ];
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
                                                       "1b4735f1-515d-494e-bc68-c520330a0580");
                                                content = Whitespace "\n";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "a1403040-45ae-4db7-b120-8184733b61b7");
                                                label = [ "let"; "="; "in" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [ Drv Pat; Drv Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Concave 17;
                                                          sort = Drv Exp;
                                                        } );
                                                  };
                                                shards = [ 0; 1; 2 ];
                                                children =
                                                  [
                                                    [
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "5fc52622-1006-4338-ba65-78e90c782e4a");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "1baa3796-2a0f-4170-bd58-b7d8a0ccd9bc");
                                                          label = [ "y" ];
                                                          mold =
                                                            {
                                                              out = Drv Pat;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Pat;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Pat;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "78491f01-aefc-4f59-b711-5e5f8e3364e0");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                    ];
                                                    [
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "cd9a37f6-8dad-4519-b0ec-92fa17c87378");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "7c025e54-9cb8-46d0-aaca-af986fc02a1d");
                                                          label = [ "4" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "816944dc-b8dd-4bde-8cb0-f5ab3c58ef48");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                    ];
                                                  ];
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "7ac9f5ee-37d4-406c-809d-d8877773211e");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "21d70300-159a-4722-b3ee-e2127e84d3e1");
                                                label = [ "{"; "}" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [ Pat ];
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
                                                shards = [ 0; 1 ];
                                                children =
                                                  [
                                                    [
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "fefed903-1aed-48f5-980b-94fb183f0c20");
                                                          label = [ "e1" ];
                                                          mold =
                                                            {
                                                              out = Pat;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Pat;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Pat;
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
                                                       "ea6aafd8-b0f1-4c68-acf2-91465237068f");
                                                content = Whitespace "\n";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "4efcdd23-498a-402d-a2f0-2e42ca3432e3");
                                      content = Whitespace " ";
                                    };
                                ];
                              ];
                          };
                        Grout
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "6547130f-5016-4109-ad3b-14f0deca4596");
                            shape = Convex;
                          };
                      ],
                      [] );
                  ancestors = [];
                };
              caret = Outer;
            };
          setup =
            {
              selection = { focus = Left; content = []; mode = Normal };
              backpack = [];
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
                                   "7a205d3c-14f7-4b39-a13e-f0b4bba49577");
                            shape = Convex;
                          };
                      ] );
                  ancestors = [];
                };
              caret = Outer;
            };
          trees =
            [
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            {
                              focus = Right;
                              content =
                                [
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "35d3fe31-b9ed-452d-8ca9-e867c3cbcbf8");
                                      label = [ "4" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              { shape = Convex; sort = Drv Exp }
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
                                             "10b97146-2d2a-4331-9ae7-74596538108b");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "6e17674a-788a-4f65-b851-6741fc5b17ac");
                                      label = [ "\\=/" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( {
                                                shape = Concave 26;
                                                sort = Drv Exp;
                                              },
                                              {
                                                shape = Concave 26;
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
                                             "8fb58054-27b4-4072-9411-6ed4e3aaa2a7");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "ed4c9aad-744c-4df3-ba87-28596f7db430");
                                      label = [ "4" ];
                                      mold =
                                        {
                                          out = Drv Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Drv Exp },
                                              { shape = Convex; sort = Drv Exp }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                ];
                              mode = Normal;
                            };
                          backpack = [];
                          relatives = { siblings = ([], []); ancestors = [] };
                          caret = Outer;
                        };
                      rule = Some E_Val;
                    },
                  [
                    Node
                      ( Just
                          {
                            jdmt =
                              {
                                selection =
                                  {
                                    focus = Right;
                                    content =
                                      [
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "f82b1e94-be20-4ea3-8065-ba17011fa2fa");
                                            label = [ "val"; "end" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [ Drv Exp ];
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
                                            shards = [ 0; 1 ];
                                            children =
                                              [
                                                [
                                                  Secondary
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "eb58cd2b-b2fe-4ca1-81bf-2c2c3944241a");
                                                      content = Whitespace " ";
                                                    };
                                                  Tile
                                                    {
                                                      id =
                                                        Option.get
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "68ff316b-001b-4d90-8beb-ba685d8fedc2");
                                                      label = [ "4" ];
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
                                                          (Haz3lcore.Id
                                                           .of_string
                                                             "2d39ebd9-0a61-4ff1-9ed0-1620efad9e22");
                                                      content = Whitespace " ";
                                                    };
                                                ];
                                              ];
                                          };
                                      ];
                                    mode = Normal;
                                  };
                                backpack = [];
                                relatives =
                                  { siblings = ([], []); ancestors = [] };
                                caret = Outer;
                              };
                            rule = Some V_Num;
                          },
                        [] );
                  ] );
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          backpack = [];
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "319d2edc-56fe-4266-b319-cc4bbe1f26ea");
                                        label = [ "1" ];
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
                                               "28c54e38-7362-4512-9ce1-27e0a6ed9d0c");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "1cd1e124-3fa4-4563-b36c-b6d753658637");
                                        label = [ "\\=/" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 26;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 26;
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
                                               "7ddd1792-bc35-47cb-993d-afb05a842695");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "48b64f1e-d20b-46b7-8af0-8020406e0019");
                                        label = [ "1" ];
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
                        };
                      rule = Some E_Val;
                    },
                  [
                    Node
                      ( Just
                          {
                            jdmt =
                              {
                                selection =
                                  { focus = Left; content = []; mode = Normal };
                                backpack = [];
                                relatives =
                                  {
                                    siblings =
                                      ( [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "8d438e64-820a-4178-905c-c684a24d7907");
                                              label = [ "val"; "end" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [ Drv Exp ];
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
                                              shards = [ 0; 1 ];
                                              children =
                                                [
                                                  [
                                                    Secondary
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "96ea9199-228e-43d4-8b43-50a93a2595e5");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "701297d1-9251-4b73-8be2-082fdb99bae9");
                                                        label = [ "1" ];
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "30be6e98-ebf7-488a-bc36-2fbadeec64b7");
                                                        content = Whitespace " ";
                                                      };
                                                  ];
                                                ];
                                            };
                                        ],
                                        [] );
                                    ancestors = [];
                                  };
                                caret = Outer;
                              };
                            rule = Some V_Num;
                          },
                        [] );
                  ] );
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          backpack = [];
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "aa1c78b1-fc9e-4b61-9b1e-d7ec0850f50e");
                                        label = [ "fun"; "->" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Pat ];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 14;
                                                  sort = Drv Exp;
                                                } );
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
                                                         "51ac6853-55d4-4370-b78a-5a36a3f75672");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "7f056298-ea6c-43b6-bb2a-14a32d65ceeb");
                                                  label = [ "z" ];
                                                  mold =
                                                    {
                                                      out = Drv Pat;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Pat;
                                                          },
                                                          {
                                                            shape = Convex;
                                                            sort = Drv Pat;
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
                                                         "66aa1075-c4c9-4ba3-b33b-185d2e8df27b");
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
                                               "37f74ab3-9b5e-4321-ad18-979c2c1dca37");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "9352a318-0631-47c0-8a06-ca6f17fbf81a");
                                        label = [ "4" ];
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
                                               "3e694678-43da-4bde-94df-9722cf0237e1");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7b0ce6cf-04c5-4a0c-a19b-595a563f6a2f");
                                        label = [ "*" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 5;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 5;
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
                                               "995530a5-6bb6-448c-a493-d843864a294a");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0be67f22-813d-43a4-a8bc-5be932510079");
                                        label = [ "z" ];
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
                                               "efbe1ada-90fe-4b26-88a1-e0baee0d4c71");
                                        content = Whitespace " ";
                                      };
                                  ],
                                  [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "c9a96e23-ad57-458f-b8f6-1b74dbe7f7ff");
                                        label = [ "\\=/" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 26;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 26;
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
                                               "48cc31f3-e76e-40b8-a7c9-3d1c1ea17570");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "cf51d03d-69a1-4083-b8fc-c2e9014bd82c");
                                        label = [ "fun"; "->" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Pat ];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 14;
                                                  sort = Drv Exp;
                                                } );
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
                                                         "05a9abad-a444-41f3-bf6c-f2c6946e5df6");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "f8660954-089e-4c2a-89c8-6a95c68e6221");
                                                  label = [ "z" ];
                                                  mold =
                                                    {
                                                      out = Drv Pat;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Pat;
                                                          },
                                                          {
                                                            shape = Convex;
                                                            sort = Drv Pat;
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
                                                         "b6b0aef0-7ba1-4056-9a8d-b8926d5df631");
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
                                               "848a25ca-006f-4a34-a9bb-996f141f1e1d");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "80921017-8b38-402d-a594-e1fd18ac3132");
                                        label = [ "4" ];
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
                                               "a38fa90a-60a7-418a-857f-308ce8976b7e");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e6229b2b-57de-4f5e-b24c-313252965150");
                                        label = [ "*" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 5;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 5;
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
                                               "3b583743-0a09-4f4b-85d4-b2855dd4a5fd");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5cdc8667-9ae6-4329-be98-e49062a4378a");
                                        label = [ "z" ];
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
                          caret = Inner (0, 0);
                        };
                      rule = Some E_Val;
                    },
                  [
                    Node
                      ( Just
                          {
                            jdmt =
                              {
                                selection =
                                  { focus = Left; content = []; mode = Normal };
                                backpack = [];
                                relatives =
                                  {
                                    siblings =
                                      ( [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "2014d651-1178-4ae8-910b-f43794639ffd");
                                              label = [ "val"; "end" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [ Drv Exp ];
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
                                              shards = [ 0; 1 ];
                                              children =
                                                [
                                                  [
                                                    Secondary
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "f9bcd518-7e93-437d-8445-f06a5f52aa88");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "9ec2e9b2-cb54-4e45-9d2e-f62736180c30");
                                                        label = [ "fun"; "->" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [ Drv Pat ];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 14;
                                                                  sort = Drv Exp;
                                                                } );
                                                          };
                                                        shards = [ 0; 1 ];
                                                        children =
                                                          [
                                                            [
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "cbad7466-5d1f-419a-8b41-8560c3a1e7db");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                              Tile
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "5eecc62e-4857-405c-a868-2c9505898489");
                                                                  label =
                                                                    [ "z" ];
                                                                  mold =
                                                                    {
                                                                      out =
                                                                        Drv Pat;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Drv
                                                                                Pat;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Drv
                                                                                Pat;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "57e2d85d-cd19-46d0-999e-013886c7d607");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                            ];
                                                          ];
                                                      };
                                                    Secondary
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "af989bc7-c9cc-42b8-8d02-3d37179b97bf");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "fd72d1e3-769d-4556-8ee8-92d51f59a194");
                                                        label = [ "4" ];
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "3ae71148-c23e-4748-b3b1-b7cd2fa2f5f1");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "e0eccb45-50f3-481c-9ad4-d37c273e3283");
                                                        label = [ "*" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 5;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 5;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "48ed0df8-3502-4e75-8c26-026c9ff1c0d7");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "93da415f-ea66-4b10-aed0-e4db18fbdff2");
                                                        label = [ "z" ];
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "eb0b5fe9-c9c0-4602-b460-ea09188fb5e7");
                                                        content = Whitespace " ";
                                                      };
                                                  ];
                                                ];
                                            };
                                        ],
                                        [] );
                                    ancestors = [];
                                  };
                                caret = Outer;
                              };
                            rule = Some V_Fun;
                          },
                        [] );
                  ] );
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          backpack = [];
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "c8327f32-584d-4e66-9db2-bb0005d9f947");
                                        label = [ "let"; "="; "in" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Pat; Drv Exp ];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 17;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0; 1; 2 ];
                                        children =
                                          [
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "07f63882-3fb2-4d97-ac84-b7b3fdbcce0a");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "1b7247dc-e6f1-4fe3-9632-6ff6db66f421");
                                                  label = [ "y" ];
                                                  mold =
                                                    {
                                                      out = Drv Pat;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Pat;
                                                          },
                                                          {
                                                            shape = Convex;
                                                            sort = Drv Pat;
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
                                                         "5fdb57c2-f7d3-45cc-8e3a-eec51cf00f4d");
                                                  content = Whitespace " ";
                                                };
                                            ];
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "2898ae95-0951-4f47-a14d-c351060f957a");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "660d88c6-35dc-40d4-8890-4d38163f29ed");
                                                  label = [ "4" ];
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
                                                         "f7f951d2-1e89-4c1f-a6d7-2e1200a57ec3");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "538fd0d9-4750-4565-88f1-99c41a4bc2b2");
                                                  label = [ "-" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 6;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 6;
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
                                                         "3ad55975-7f2e-4ac6-9577-2dd29622c8ea");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "07cad42a-d71b-406d-a973-65fc57872d5d");
                                                  label = [ "3" ];
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
                                                         "f501edc2-5cc9-4a1a-94aa-e7ea2b6f1b91");
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
                                               "fda355ea-7081-43b8-a43e-9ea9e02d8fe8");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0fb09482-1a0c-4371-887b-84496e1e065f");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Exp ];
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
                                        shards = [ 0; 1 ];
                                        children =
                                          [
                                            [
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "567884ad-728d-4d00-a69f-7281669432f0");
                                                  label = [ "fun"; "->" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [ Drv Pat ];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 14;
                                                            sort = Drv Exp;
                                                          } );
                                                    };
                                                  shards = [ 0; 1 ];
                                                  children =
                                                    [
                                                      [
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "d8c65771-3347-4fff-8a5c-b256680c6bc3");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "919105fc-05a6-49c6-bc65-f8449aae3d4f");
                                                            label = [ "z" ];
                                                            mold =
                                                              {
                                                                out = Drv Pat;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Pat;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Pat;
                                                                    } );
                                                              };
                                                            shards = [ 0 ];
                                                            children = [];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "e92bb4a6-c075-4653-977b-5f89f7a5e80a");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                      ];
                                                    ];
                                                };
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "9c7a4337-3c91-4c55-bb14-f73c0d62b90e");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "4152ffbe-4d5a-4f5d-b1e2-a1e991fb7000");
                                                  label = [ "4" ];
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
                                                         "79a61d35-4713-4346-b30d-7addb0675bfc");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "97285ce5-3124-4ddc-82bd-1a8084c85ecb");
                                                  label = [ "*" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 5;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 5;
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
                                                         "06045f4c-444c-4545-9c13-e92abcb678c8");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "8bc693d8-fcd1-4648-8cd1-00ed926b4628");
                                                  label = [ "z" ];
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
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e3bac142-fb12-4990-ab1d-a96a75107e77");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 2;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                } );
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
                                                         "6ce744f3-ba8a-4591-a3f0-b4e3d2b27b32");
                                                  label = [ "y" ];
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
                                  ],
                                  [
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "333184a4-4ba0-48b1-83df-59a5f15f031f");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f6656809-06bb-4210-ac06-f50c87c7843d");
                                        label = [ "\\=/" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 26;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 26;
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
                                               "259abd84-997e-40ef-8f47-646617a4d7b3");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5a1d56c9-bd86-4eb3-86af-ad85d1044770");
                                        label = [ "4" ];
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
                        };
                      rule = Some E_Let;
                    },
                  [
                    Node
                      ( Just
                          {
                            jdmt =
                              {
                                selection =
                                  { focus = Left; content = []; mode = Normal };
                                backpack = [];
                                relatives =
                                  {
                                    siblings =
                                      ( [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "444d34ff-5afa-42e7-a349-4e791fabd70b");
                                              label = [ "4" ];
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
                                                     "c822b9a3-2f6b-497d-8cc6-1372b4fa802e");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "c2621143-eb7c-4f26-be65-787b00d18e95");
                                              label = [ "-" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 6;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 6;
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
                                                     "fcdf97e0-41a4-45c2-8a2e-e9d209f8f2f7");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "a27fd77c-2d1f-4be6-a8dc-84c9596aac2e");
                                              label = [ "3" ];
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
                                                     "e65fcd5e-3fb1-4fb0-809a-9d45402e7d03");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "2c546b01-0237-49ef-96d3-37f4738708be");
                                              label = [ "\\=/" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 26;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 26;
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
                                                     "f45d6d35-a8e0-450c-8c35-025880da8205");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "765c437e-8388-4818-9228-ac00904c00ca");
                                              label = [ "1" ];
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
                              };
                            rule = Some E_Minus;
                          },
                        [
                          Node (Abbr (Some 0), []);
                          Node
                            ( Just
                                {
                                  jdmt =
                                    {
                                      selection =
                                        {
                                          focus = Left;
                                          content = [];
                                          mode = Normal;
                                        };
                                      backpack = [];
                                      relatives =
                                        {
                                          siblings =
                                            ( [
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "9397bdc2-b989-493f-8e72-12bf7a00a387");
                                                    label = [ "3" ];
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
                                                           "17c8d741-cbec-4c30-bd04-1bb3858cb67d");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "31bc38d0-4611-4018-af19-879fee1ffc5e");
                                                    label = [ "\\=/" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Concave 26;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 26;
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
                                                           "dd21e9d7-58b1-45bd-a156-21b76782eac2");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "83b9a081-609e-4a16-8f5e-5bc600cfafd8");
                                                    label = [ "3" ];
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
                                    };
                                  rule = Some E_Val;
                                },
                              [
                                Node
                                  ( Just
                                      {
                                        jdmt =
                                          {
                                            selection =
                                              {
                                                focus = Left;
                                                content = [];
                                                mode = Normal;
                                              };
                                            backpack = [];
                                            relatives =
                                              {
                                                siblings =
                                                  ( [
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d7192fdc-e79a-4434-a81f-c21ed8b71973");
                                                          label =
                                                            [ "val"; "end" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [ Drv Exp ];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0; 1 ];
                                                          children =
                                                            [
                                                              [
                                                                Secondary
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "5fb310f7-7fa2-4195-a2d6-7c9e58499996");
                                                                    content =
                                                                      Whitespace
                                                                        " ";
                                                                  };
                                                                Tile
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "a3a9e585-f8b6-45e8-832a-4cb8afcde954");
                                                                    label =
                                                                      [ "3" ];
                                                                    mold =
                                                                      {
                                                                        out =
                                                                          Drv
                                                                            Exp;
                                                                        in_ = [];
                                                                        nibs =
                                                                          ( {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Drv
                                                                                Exp;
                                                                            },
                                                                            {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Drv
                                                                                Exp;
                                                                            } );
                                                                      };
                                                                    shards =
                                                                      [ 0 ];
                                                                    children =
                                                                      [];
                                                                  };
                                                                Secondary
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "ab29aef2-7fbf-456f-ae65-53cf34caea59");
                                                                    content =
                                                                      Whitespace
                                                                        " ";
                                                                  };
                                                              ];
                                                            ];
                                                        };
                                                    ],
                                                    [] );
                                                ancestors = [];
                                              };
                                            caret = Outer;
                                          };
                                        rule = Some V_Num;
                                      },
                                    [] );
                              ] );
                        ] );
                    Node
                      ( Just
                          {
                            jdmt =
                              {
                                selection =
                                  { focus = Left; content = []; mode = Normal };
                                backpack = [];
                                relatives =
                                  {
                                    siblings =
                                      ( [
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "97bb9431-74f0-4e36-9644-74c1a319091a");
                                              label = [ "fun"; "->" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [ Drv Pat ];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 14;
                                                        sort = Drv Exp;
                                                      } );
                                                };
                                              shards = [ 0; 1 ];
                                              children =
                                                [
                                                  [
                                                    Secondary
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "e45e7d78-90bf-4331-b538-b862c6d10feb");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "2ab90a06-9219-4024-8313-73ced611ee5d");
                                                        label = [ "z" ];
                                                        mold =
                                                          {
                                                            out = Drv Pat;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Drv Pat;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Drv Pat;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Secondary
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "ea8c7cad-4292-470a-ab51-51d1e16d9037");
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
                                                     "c2edb19b-f869-42f3-9076-8394b1289d4e");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "ca310618-2bc9-4054-bfd2-7de7c4fa75a9");
                                              label = [ "4" ];
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
                                                     "8c6ae011-e188-4d7b-ac15-4cd1e9945d71");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "af1173eb-9dde-4215-8ba3-fc4db3ff7084");
                                              label = [ "*" ];
                                              mold =
                                                {
                                                  out = Drv Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 5;
                                                        sort = Drv Exp;
                                                      },
                                                      {
                                                        shape = Concave 5;
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
                                                     "a8a55641-4e29-4556-8deb-93e50bb27509");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "3e36831b-3e4b-42da-8d36-813dcb92a7ed");
                                              label = [ "z" ];
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
                                    ancestors =
                                      [
                                        ( {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "3faab106-642e-4c01-9ff5-4214172e0173");
                                            label = [ "("; ")" ];
                                            mold =
                                              {
                                                out = Drv Exp;
                                                in_ = [ Drv Exp ];
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
                                                         "c017f8c4-a656-4b7f-b808-16fd2e2bfaf0");
                                                  label = [ "("; ")" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [ Drv Exp ];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 2;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Convex;
                                                            sort = Drv Exp;
                                                          } );
                                                    };
                                                  shards = [ 0; 1 ];
                                                  children =
                                                    [
                                                      [
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "df6114fe-8e03-4ccb-bb5b-3317c9a628ca");
                                                            label = [ "1" ];
                                                            mold =
                                                              {
                                                                out = Drv Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Exp;
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
                                                         "d7afeb9d-34a7-4e7f-9d4b-30dcb5781980");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "1696864b-4d43-44a8-8e9d-0c270e8ec3f4");
                                                  label = [ "\\=/" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 26;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 26;
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
                                                         "6a88b56f-576b-4d74-a227-88ef010da207");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "aa6be13e-6e27-4c11-8950-fce3ca4c0913");
                                                  label = [ "4" ];
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
                                            ] ) );
                                      ];
                                  };
                                caret = Outer;
                              };
                            rule = Some E_Ap;
                          },
                        [
                          Node (Abbr (Some 2), []);
                          Node (Abbr (Some 1), []);
                          Node
                            ( Just
                                {
                                  jdmt =
                                    {
                                      selection =
                                        {
                                          focus = Left;
                                          content = [];
                                          mode = Normal;
                                        };
                                      backpack = [];
                                      relatives =
                                        {
                                          siblings =
                                            ( [
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "9939d602-b40b-4d36-8431-061fe6c7ba4d");
                                                    label = [ "4" ];
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
                                                           "37394f54-b1de-42ac-b154-6c4800ff5f16");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "b1c94b83-b03a-4b1f-b5e7-a1ba02ef906b");
                                                    label = [ "*" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Concave 5;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 5;
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
                                                           "fed8e126-399e-47a5-9886-45a90127cb43");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "9c924436-0927-4144-b8ef-5052597299e3");
                                                    label = [ "1" ];
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
                                                           "820cb474-2e6f-4257-945c-a867c50fa8f1");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "2f9a91c0-87c0-4ef4-b4ff-08e6811c53eb");
                                                    label = [ "\\=/" ];
                                                    mold =
                                                      {
                                                        out = Drv Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Concave 26;
                                                              sort = Drv Exp;
                                                            },
                                                            {
                                                              shape = Concave 26;
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
                                                           "1b6d8ab8-cfee-44db-8b1c-e85bd4dfc5cd");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "e0984273-0323-4274-9266-be194d2a6baa");
                                                    label = [ "4" ];
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
                                    };
                                  rule = Some E_Times;
                                },
                              [
                                Node (Abbr (Some 0), []);
                                Node (Abbr (Some 1), []);
                              ] );
                        ] );
                  ] );
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          backpack = [];
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "bb06e078-7e75-4594-b7dd-247c7f191a84");
                                        label = [ "let"; "="; "in" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Pat; Drv Exp ];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 17;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0; 1; 2 ];
                                        children =
                                          [
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "84e60dcf-54f6-40a9-ae72-d8634718215b");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "188bf8af-879a-46ea-b821-68e34bd3a228");
                                                  label = [ "f" ];
                                                  mold =
                                                    {
                                                      out = Drv Pat;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Pat;
                                                          },
                                                          {
                                                            shape = Convex;
                                                            sort = Drv Pat;
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
                                                         "95ebf0fe-d909-426c-84c7-36f894733e23");
                                                  content = Whitespace " ";
                                                };
                                            ];
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "dbb25e57-f47e-4512-8568-6c4098dd4550");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "226999ae-5301-4b8e-b48d-3803c4fbc3f8");
                                                  label = [ "fun"; "->" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [ Drv Pat ];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 14;
                                                            sort = Drv Exp;
                                                          } );
                                                    };
                                                  shards = [ 0; 1 ];
                                                  children =
                                                    [
                                                      [
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "415ba217-5057-4204-904f-eedc439c42e0");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "5a22868d-ac5a-4979-a1f2-95e3fef06e9e");
                                                            label = [ "z" ];
                                                            mold =
                                                              {
                                                                out = Drv Pat;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Pat;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Pat;
                                                                    } );
                                                              };
                                                            shards = [ 0 ];
                                                            children = [];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "db104399-be58-4002-b8d8-9c898b8b0bbb");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                      ];
                                                    ];
                                                };
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "736593d0-7db3-42ae-9fa9-29158a28ca5e");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "ff2404e6-e881-4024-a88e-af2ba35177a7");
                                                  label = [ "4" ];
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
                                                         "bfe1aa36-95d5-4f46-a8a6-ab9ab1360841");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "f796f721-faaf-48f0-a128-b75937defc10");
                                                  label = [ "*" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 5;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 5;
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
                                                         "b3011bbc-2926-4f90-a301-9299ed568bbe");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "3c98c439-e024-4abe-9dcf-717b71695616");
                                                  label = [ "z" ];
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
                                                         "dbef0546-feba-42d4-a446-560c5b71c330");
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
                                               "4b1ea798-b1f2-4a13-a10f-4af8049c7704");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "c2ba5884-4266-4e5b-b893-6468bdb3333b");
                                        label = [ "let"; "="; "in" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Pat; Drv Exp ];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 17;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0; 1; 2 ];
                                        children =
                                          [
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "518b26ca-36de-47a8-abca-9bf4799848dc");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "f6671095-0857-4ef0-b973-7022cf7c3eed");
                                                  label = [ "y" ];
                                                  mold =
                                                    {
                                                      out = Drv Pat;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Pat;
                                                          },
                                                          {
                                                            shape = Convex;
                                                            sort = Drv Pat;
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
                                                         "54102511-9ea1-4476-816a-c661026159b0");
                                                  content = Whitespace " ";
                                                };
                                            ];
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "f47f87e6-1ef9-4bc8-8562-4b27cf975b0a");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "64ddc137-5bed-4b9b-aece-2c4fd01c9700");
                                                  label = [ "4" ];
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
                                                         "d7f7cf5e-5404-4a0a-b7c9-fa72f44b03a2");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "68f83ff7-7bdc-4a90-8b99-7fe9888b56cc");
                                                  label = [ "-" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 6;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 6;
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
                                                         "795cfcb2-7083-4f9f-97d0-6c9c53068765");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "672b646f-0af2-4413-9938-5881d604d6b2");
                                                  label = [ "3" ];
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
                                                         "5e9a8a1c-86d3-4fdc-b8e8-202488926558");
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
                                               "5e3fdd55-431c-4559-a6cf-b73f7a6207de");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "bf160016-99cf-4e76-8c5c-5c0e1c5010dc");
                                        label = [ "f" ];
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
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "775be00a-28d4-4888-8241-617fea7e22a7");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 2;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                } );
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
                                                         "9a76537d-8926-4bf0-a595-67b2f990c58c");
                                                  label = [ "y" ];
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
                                               "5aa9ab26-e332-4f54-a19b-6289c2de6810");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "56d8262f-1671-42fa-920d-7032c113e5a3");
                                        label = [ "\\=/" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 26;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 26;
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
                                               "799f6de1-719b-4d95-abbf-a89b7c68bea0");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "74d82622-af37-45b0-bd8a-f9ddeec237fd");
                                        label = [ "4" ];
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
                        };
                      rule = Some E_Let;
                    },
                  [ Node (Abbr (Some 2), []); Node (Abbr (Some 3), []) ] );
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          backpack = [];
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "52593cf5-3620-4699-92cc-1067225674cf");
                                        label = [ "{"; "}" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Pat ];
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
                                        shards = [ 0; 1 ];
                                        children =
                                          [
                                            [
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "12a889ca-cd42-4208-8c9b-ea75eaa9c7e9");
                                                  label = [ "e_example" ];
                                                  mold =
                                                    {
                                                      out = Pat;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Pat;
                                                          },
                                                          {
                                                            shape = Convex;
                                                            sort = Pat;
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
                                               "aa83ea2f-1977-4254-bb89-69b4228a1580");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0aa6b135-28ea-43c2-9e15-daea988148ed");
                                        label = [ "\\=/" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 26;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 26;
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
                                               "1b2e2fb8-0f02-4747-8826-5533f2c00dc7");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7bed43f3-1557-4313-bd7c-1540bf54cafb");
                                        label = [ "4" ];
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
                        };
                      rule = Some E_Let;
                    },
                  [ Node (Abbr (Some 0), []); Node (Abbr (Some 4), []) ] );
            ];
        };
  }
