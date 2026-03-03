let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "a40321c5-a92e-472a-8530-71abbdb52b42");
    title = "Gradebook Tidy Term";
    version = 1;
    module_name = "Ta_TidyTerm";
    prompt =
      "Implement the `tidy_term` function. It should take a gradebook as a \
       list of `GradebookEntry` and return a new table that splits the `term` \
       column into separate `year` and `semester` columns.\n\n\
       For example, given a table containing:\n\
       ```hazelnostatics\n\
       ^^table([(student_id=1, term=\"2025FA\"), (student_id=2, \
       term=\"2020SP\")])\n\
       ```\n\n\
       The result should be:\n\
       ```hazelnostatics\n\
       ^^table([(student_id=1, year=2025, semester=Fall), (student_id=2, \
       year=2020, semester=Spring)])\n\
       ```\n\n\
       The `year` should be extracted from the first 4 characters of the \
       `term` string (converted to `Int`), and the `semester` from the last 2 \
       characters: `\"FA\"` → `Fall`, `\"SP\"` → `Spring`.\n\n\
       `Semester` is a sum type defined in the prelude: `type Semester = Fall \
       + Spring`. Use the constructors `Fall` and `Spring` directly (not as \
       strings).\n\n\
       The output table should contain all original columns **except** `term`, \
       plus the new `year` and `semester` columns. The order of columns does \
       not matter.";
    display_hint =
      "Think about how to transform each row independently. What string \
       operations could help you break the `term` field into its components?";
    task_reference =
      (let semester_type =
         "### Semester Type\n\
          `Semester` is a sum type defined in the prelude:\n\
          - `Fall` — represents fall semester (term code `\"FA\"`)\n\
          - `Spring` — represents spring semester (term code `\"SP\"`)"
       in
       let string_operations =
         "### String Operations\n\
          - `string_sub(s, start, len)` — extract substring (0-indexed)\n\
          - `string_length(s)` — length of a string\n\
          - `int_of_string(s)` — convert string to Int"
       in
       TaskRefDocs.compose
         [
           semester_type;
           string_operations;
           TaskRefDocs.case_expression;
           TaskRefDocs.if_expression;
           TaskRefDocs.map;
           TaskRefDocs.tuple_extension;
           TaskRefDocs.column_projection;
         ]);
    your_impl =
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
                             "6e01f020-9e6a-467a-be6c-23b5e180f371");
                      content = Whitespace "\n";
                    };
                  Projector
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "ad04d556-15c9-4fb3-acc0-5fe5d266bd27");
                      kind = Table;
                      syntax =
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "cc93a15f-b177-4111-939d-12dc74c00a42");
                            label = [ "("; ")" ];
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
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "eb7f6f38-0678-45ec-b44d-d090dbe2ed40");
                                      label = [ "["; "]" ];
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
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "e68beb97-2ccd-44a1-b92d-41929a3c4df5");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [ Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
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
                                                                 "9ad007e7-b674-4063-97bc-cac5bf1af6c1");
                                                          label =
                                                            [ "student_id" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "9a58712b-61c4-4037-9c36-bf6df58290e8");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "e06aff94-d7d1-4f55-bb7f-64187fea573f");
                                                          label = [ "1" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "71a6e3c0-15fe-470f-b0ea-a555ea9b9627");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "81c162c9-9041-4aa5-98d5-40ed3cd98cf1");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "626e2dc2-04eb-4ab6-b03a-e8ef341c23be");
                                                          label = [ "term" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d827ac71-4c60-4412-bdd8-7db7d098de9b");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "62c87730-0770-477a-9d41-73f4940c31ed");
                                                          label =
                                                            [ "\"2025SP\"" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "12da4223-2531-4a6c-919b-52ac18d324fd");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "317b23fd-d5cb-4bbc-8d26-ac5c22b3e255");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "9a87e4cc-3036-4e0f-bbe8-a6af9d3954cb");
                                                          label = [ "quiz1" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "29668102-c71f-401d-9983-a703d9f81e50");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "84338c40-5a33-4784-8dbe-18759f801290");
                                                          label = [ "7." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "00922ca1-ad4f-4595-947e-bf96b459b92a");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "3f52398c-3b84-4b0d-a8d3-86ccf5c1bb18");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "3885882f-1dad-44a1-83e5-7f9630a724bc");
                                                          label = [ "quiz2" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "41a7cd35-ede9-441f-b79b-45105965069f");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "da0f03bb-ea1f-4b78-8723-a000da99c9e2");
                                                          label = [ "5." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b51ff8ec-78f0-4392-81bf-2156bdee068d");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "140c5365-c31b-4316-a598-f9d0d84d9393");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "23f8a5ba-ee9a-4283-9711-a1068bffce57");
                                                          label = [ "midterm" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "18ad0f1c-6143-4c23-b877-5c7bffce0bf6");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "69efb15e-85df-422c-9114-2f4ff47e0b4a");
                                                          label = [ "85." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b650b873-984c-4bd1-a9dc-72da56f32c68");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "8adcd459-871b-4fb0-b82b-16ccc1feeb7b");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d2dc91d9-feb2-4c2c-9400-1b2be5ca27ec");
                                                          label = [ "final" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "96d822f6-38a7-46da-955e-0479b89d76aa");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "c8f2dd59-5f49-4a05-b2cb-51227d4e5121");
                                                          label = [ "88.0" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
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
                                                       "05850242-72bf-4144-9b74-a45470e5a322");
                                                label = [ "," ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 44;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 44;
                                                          sort = Exp;
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
                                                       "9d826724-033a-4d7b-bead-2f345f517ef3");
                                                content = Whitespace "\n";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "ef130595-d4ee-414f-92f5-648463856436");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [ Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
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
                                                                 "f18eb1fc-a8b9-465d-8206-95ea791f2792");
                                                          label =
                                                            [ "student_id" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "238a074a-6648-4ae3-ba1f-2941ca3c5e84");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "2a8e75ef-cafb-4239-a928-ff899d2368da");
                                                          label = [ "2" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "fc7067a5-4308-48b0-9a54-18614e5f4597");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "04cf2931-82ac-4fe6-a64a-7764a13ed4bd");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "70dd8a90-e684-4fe6-9d86-336fdf6705b1");
                                                          label = [ "term" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "ea31e545-80a1-488b-a171-2e88a58af8bd");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "fa64ca2e-4aef-4d18-a029-8e61b2a5e305");
                                                          label =
                                                            [ "\"2025FA\"" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "90b06bba-de3f-40d0-825a-3ccbd0b507bb");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "38d4367d-8b20-4257-bc3a-717bcc144d38");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "28b37ddf-3663-468d-9291-5e46ba9f915e");
                                                          label = [ "quiz1" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a301a1f0-4ec5-4271-bb05-85c0dae24496");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d721cbd9-84b8-40d0-bdbf-5e48f57f3af2");
                                                          label = [ "5." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "4ca58c00-c542-4f7d-8bd1-0afc46e72681");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "98ca917a-3601-4a0d-848c-9372a0d5bb9d");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b4178732-1de0-4108-896b-b03d5edb52b8");
                                                          label = [ "quiz2" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "31c2238c-0f66-4cda-801e-42e79c79813d");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "6443d12b-e574-43a0-b073-e4bd1472e6e1");
                                                          label = [ "8." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "c2711e58-5fc5-44fe-9859-3a3cd38287dd");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "129b34f6-88da-48b4-ae04-54a444e08935");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "6b9ece8e-fddc-46fc-bfec-6c481a8ebf84");
                                                          label = [ "midterm" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "8f5dbace-287e-4e4b-9aa3-f00d5aea7805");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "78571804-0bbe-4e19-81df-43a98585c152");
                                                          label = [ "90." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f14f4daa-aea9-4c89-a959-4aa2ed32776d");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "cefefea8-347d-4626-9a12-2076026a0f5a");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "70f7e8f2-d55e-41c0-a2ef-a30be9e44a04");
                                                          label = [ "final" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f3102531-53d1-4e91-84e2-518948640680");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f4ffb8a4-58eb-42f1-ab9d-51af620d9a64");
                                                          label = [ "82." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
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
                                                       "7e3c2db0-e828-4e38-a2fe-7c2762177f81");
                                                label = [ "," ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 44;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 44;
                                                          sort = Exp;
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
                                                       "d4cdeb13-cb76-4085-aa3d-1f446c7616da");
                                                content = Whitespace "\n";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "fb584dd8-55e1-4d53-a295-93036bafdb39");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [ Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
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
                                                                 "c188ca1a-9a1b-4129-a78e-3607503f18e2");
                                                          label =
                                                            [ "student_id" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "daba93d6-235b-4098-a790-257c1cf13ae7");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "12c4cf29-70cb-472e-a280-339725f6615a");
                                                          label = [ "3" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "76746906-6356-4060-a08a-df42d44446cf");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "33757a9a-075e-4d7b-8b27-4ff40023ea5a");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "142151bb-05f9-4c77-bf6b-be497eb6ac21");
                                                          label = [ "term" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "182cd109-cbfd-4d2e-8ec6-8451daa2cd2a");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "4376cd49-243d-41db-af62-d88fbe9b185a");
                                                          label =
                                                            [ "\"2024FA\"" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "abe5a509-0e04-4f04-af9f-317086b45441");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "8e58d30a-9190-4f49-8e2e-b92aabdc9dbc");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "49876032-0e1e-443b-be6a-28ca06e8e5c2");
                                                          label = [ "quiz1" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "8c90b84a-8c72-4ca3-a0e6-392285311c59");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "8e4d2a00-a386-4352-9224-9a62a5663ff2");
                                                          label = [ "8." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "c20e640b-fd31-44c4-b871-af3b020306b6");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "14f60930-4db2-4384-9932-189e9b3a98a6");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "7929aaf5-cf59-419e-9c49-5f5b4f4c6008");
                                                          label = [ "quiz2" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "891c8a6e-cbef-4f94-8393-591e45dbedd9");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b6482771-05ed-4f4a-a11b-12adfe7672cb");
                                                          label = [ "7." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "872b5bc9-8f64-4a8e-a1c7-c6fabb67b2cf");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "fc649799-2e17-41e6-b717-277fc1469b55");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d3574ed2-37de-40b6-adf6-4e8e27bd541d");
                                                          label = [ "midterm" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "310c139f-0e52-48f1-a9d8-30c6ab36206b");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "486b85ef-5726-4b65-b537-a4c962d858b8");
                                                          label = [ "78." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "9c9c496d-ccce-467a-ae83-97a1a2e86fe4");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "d05efa9b-0d23-4b53-a87f-686bc384a259");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "35ca4842-b2b2-450c-a485-15b4348c9b42");
                                                          label = [ "final" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "304e9a2b-c2ea-49d9-8e5c-5cca4d80ee7c");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d845f5f4-d08a-409f-94e8-f2c942e57d68");
                                                          label = [ "80." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
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
                                                       "e3d11b99-4e68-419a-966e-97561f3aefc2");
                                                label = [ "," ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 44;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 44;
                                                          sort = Exp;
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
                                                       "7455bfc0-395f-40b1-8999-27657c3aff1a");
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "d427b3ec-32d0-46f0-9f4d-2dc6a2a9b83b");
                                                content = Whitespace "\n";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "ffde0510-8326-45f8-9777-8b20e08bbba5");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [ Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
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
                                                                 "2bb3a8b2-fd7f-4e8a-ae55-9f1811387756");
                                                          label =
                                                            [ "student_id" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "67d88fd8-f6ec-4ade-a903-dbcf3fc04f28");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "c62a9a33-f1e5-4abe-a41e-30c2de89aa9b");
                                                          label = [ "4" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "3667d8ec-4430-4d07-a30f-4391f2b9fb7b");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "858482f5-cc5a-421c-b3a8-1b94db2aceae");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "5dee3d60-502d-4d21-bc7d-c12f57ae9045");
                                                          label = [ "term" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "0f352d1d-5692-43fe-ba95-7d6b39de3ee9");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "e4116f68-cf13-410d-a5cd-521598a681d1");
                                                          label =
                                                            [ "\"2025SP\"" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "df00c033-1c7b-4776-8a4c-65b206ce2ba8");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "f2d5bef8-eb73-48bf-bd56-52d3682c5843");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "0bd50e15-0c9e-4f95-bc0b-dd0bbb1bd9b0");
                                                          label = [ "quiz1" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "1cbcd0a0-e48c-48e1-8e47-3bab83fc81b5");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "8d6a2a16-ce21-41f5-b458-cc5c02f9133c");
                                                          label = [ "9." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f017f025-1864-43ce-bdde-f20883e5451c");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "f42d8ee9-086a-4192-9870-5fdf6799c8e2");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b31975d2-848a-49d4-8f60-de2080f3bd52");
                                                          label = [ "quiz2" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f806d7af-4679-4c85-a817-aa4e38d26998");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "2ccad1ee-7c76-404c-82dc-92c21b5a3f4e");
                                                          label = [ "10." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "6c8f6222-33bd-40ef-a006-0bf5adfd1a2a");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "74a8a505-dc6c-4348-838f-2b131ec0f015");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a1de8877-39e5-4221-a7e0-f2453b2fae1a");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "847645b0-ee36-450a-bc24-89ff3e5eaea8");
                                                          label = [ "midterm" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a6f52594-707d-4374-b4fa-71ebfa85fa53");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f74911d6-24b3-479a-ada0-895c970e6260");
                                                          label = [ "95." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a1f1819a-5cfc-489e-a623-e6f9867a4d10");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "38974ebc-3de3-4b6e-bc30-f38a98222e36");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b993d4b4-8a99-4514-b802-7b7721f9b802");
                                                          label = [ "final" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "7cad237b-b2e8-44e4-bde2-c445daca7162");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "54939391-f6e8-41d0-bd3f-797d0c394aad");
                                                          label = [ "98." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
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
                                                       "2ecb0857-6774-4c0f-a8c2-ba43b0a0929e");
                                                label = [ "," ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 44;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 44;
                                                          sort = Exp;
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
                                                       "5e92bc39-d08e-4232-948b-833b4f7d4ba2");
                                                content = Whitespace "\n";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "38540608-1ff7-4d89-81e0-ee988d4f9600");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [ Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
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
                                                                 "d43e7703-06bf-4c04-b3dd-0d80fb943ba7");
                                                          label =
                                                            [ "student_id" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f2fea0f0-41e7-43e8-9faf-9907d8423664");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "90ea0d9e-4ca3-4766-b255-d0446474435a");
                                                          label = [ "5" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "2bba49a2-49cd-4450-ba65-19bb78034a67");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "493e4c68-115a-411a-ac78-816bf65deeec");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f50c8e39-6fdc-4e68-93a6-78d0c6842312");
                                                          label = [ "term" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "c604aa46-0c56-4368-b7fd-126652d8f658");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b7e21d3c-b9e6-4a09-9a46-81a3c46b0978");
                                                          label =
                                                            [ "\"2024FA\"" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "3e5cdfe6-74c0-462a-b91e-9be91b130037");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "cf503c84-a1e3-4165-b232-7e63838ce167");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "040dc0ef-b3e1-4fc4-a29a-708288bb9a87");
                                                          label = [ "quiz1" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "7985813d-fc68-4a92-8e76-a6cf61d864e0");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "e8f7d9d8-512a-452e-8490-c8eb8de31e95");
                                                          label = [ "4." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b03e917c-1e1c-4fc3-bc23-185362749f52");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "a2f3320a-c8b1-4c48-a418-6af897bc6a71");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a46e4cbf-1e44-42fe-bbd7-85774a116207");
                                                          label = [ "quiz2" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "538cbfe5-2007-4019-a4c9-d040ce4783b2");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "fa8cafa0-6f32-489b-80b5-18fd71709d32");
                                                          label = [ "3." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "e45911ec-7dac-460f-87f7-4bc0a74f9ce2");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "edd1a73f-3a82-47fb-b830-5429c467d252");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "04fa0618-b098-4801-ab4f-3310922d9610");
                                                          label = [ "midterm" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "8e3c7af7-b579-446f-ac27-e870c1661876");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f9ae5e0a-1e69-4c42-94f5-d206387518fb");
                                                          label = [ "60." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a59f749c-c879-4ff1-8080-c6a45c8a0008");
                                                          label = [ "," ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 44;
                                                                    sort = Exp;
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
                                                                 "e3dd8bf5-3f2b-4ebb-acae-2ed49813bd28");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "c77d6505-bbbc-4c4d-8b8a-2200922b8134");
                                                          label = [ "final" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "7e0d90f3-ad04-439a-bb59-9a7922a7000e");
                                                          label = [ "=" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 39;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "e5186e6d-fe62-4eaf-81b7-5ab60af6dabe");
                                                          label = [ "65." ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                    ];
                                                  ];
                                              };
                                          ];
                                        ];
                                    };
                                ];
                              ];
                          };
                      model = "()";
                    };
                ],
                [
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "a6e1cc42-b23a-4ab9-99c9-d30e28176576");
                      content = Whitespace "\n";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "90fc7884-2eeb-4ce3-99ea-688c45eee980");
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
                                       "218177f5-bb78-4c00-b64b-a40d6407988e");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "415facdb-49c9-49c6-8934-23d7a7018161");
                                label = [ "gradebook" ];
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
                                       "377773c9-854a-4da1-9870-0d5210bee923");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "0f82ab4e-342a-4bee-85a4-f9576cdc0607");
                                label = [ ":" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 24; sort = Pat },
                                        { shape = Concave 24; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "cf5c469c-f4d1-4068-bec1-8dae8b4e5335");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "1fa3eb07-6a6c-4efb-9220-be7cf95cad9d");
                                label = [ "["; "]" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [ Typ ];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
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
                                                 "83e724c2-9692-4130-832f-2320f6c4892d");
                                          label = [ "GradebookEntry" ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Typ },
                                                  { shape = Convex; sort = Typ }
                                                );
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
                                       "1828a82d-0b46-4756-a593-4cae5995debd");
                                content = Whitespace " ";
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "f88da7ee-2910-4497-b87e-f489bac7d45b");
                                content = Whitespace " ";
                              };
                          ];
                        ],
                        [] );
                  },
                  ( [
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "b32c3962-d54c-426b-a938-d1466f0d04f5");
                          label = [ "type"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ TPat; Typ ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 45; sort = Exp } );
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
                                           "32cb8dfc-0b73-4c60-b632-ee61398dc071");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c1560545-3cc6-4e19-8873-0391ee14e9e6");
                                    label = [ "Semester" ];
                                    mold =
                                      {
                                        out = TPat;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = TPat },
                                            { shape = Convex; sort = TPat } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c3356b56-0037-4f0e-ad4e-1298e4559f0e");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "614ffcef-02df-4bad-8e05-5e39237d9d03");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "fee1b1f0-a7cd-4c07-95df-4efc96d20a3f");
                                    label = [ "+" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Typ },
                                            { shape = Concave 33; sort = Typ }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c813b20b-6070-4d41-9298-6475658ab9aa");
                                    label = [ "Spring" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Typ },
                                            { shape = Convex; sort = Typ } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c47ed4a9-941f-4bf3-9ab2-e170f6e3b341");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "0aa1a65e-f605-41ce-9a09-7a51fc54fe92");
                                    label = [ "+" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 12; sort = Typ },
                                            { shape = Concave 12; sort = Typ }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "88d1d243-0bc8-44c6-bdc4-71fa26e8b90a");
                                    label = [ "Fall" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Typ },
                                            { shape = Convex; sort = Typ } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "3b9530f3-480e-48c1-9476-145a251e1fe7");
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
                                 "d0c16a76-76b3-4ec1-9ac8-efb30223c44c");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "2d01419d-0647-4ae0-8360-1a179883c312");
                          label = [ "type"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ TPat; Typ ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 45; sort = Exp } );
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
                                           "dbea140b-c8af-4197-b623-bbb4cbec4b60");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c3c52e77-fb86-4d90-afab-844ef5e4b69f");
                                    label = [ "GradebookEntry" ];
                                    mold =
                                      {
                                        out = TPat;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = TPat },
                                            { shape = Convex; sort = TPat } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "688f4a18-c406-4403-b536-1e5bd1ff6426");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "287c139f-ae37-4f79-815a-40a112fa9adc");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1b9251b5-879c-40f3-989a-a9e3b68c1d77");
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [ Typ ];
                                        nibs =
                                          ( { shape = Convex; sort = Typ },
                                            { shape = Convex; sort = Typ } );
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
                                                     "19dca7f6-e075-4657-be7b-a3a0e46b989f");
                                              label = [ "student_id" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "ec1a9ea1-cee7-44d1-8bae-25ca1a91d35a");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
                                                        sort = Typ;
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
                                                     "f62c13b0-4d02-4ea5-8b50-a00c417ae94c");
                                              label = [ "Int" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "357bec84-6389-482f-84c5-3ac2e3857429");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Typ;
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
                                                     "00d84bdb-9bc2-4f41-a796-165d10c30f71");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "971af465-ad22-40bc-b94c-b12723c8b407");
                                              label = [ "term" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "8385e00b-87f7-47bf-9b74-88b779d04dbe");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
                                                        sort = Typ;
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
                                                     "98b34912-7747-454f-80fa-fca8a04d22bf");
                                              label = [ "String" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "6746fcc4-e048-4afe-8884-f8e587941e4f");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Typ;
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
                                                     "2c9f03b1-7c4b-4b47-8e05-2b81823f0286");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "33f1d626-17a7-4368-9882-e6e0cc9710b2");
                                              label = [ "quiz1" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "485bc0bc-a065-4ac1-bc38-07b2a6959f0e");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
                                                        sort = Typ;
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
                                                     "43cf2b43-0707-4a32-baf1-48631f4b2d29");
                                              label = [ "Float" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "50e406b2-1297-485f-8f7d-c61acfda0f26");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Typ;
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
                                                     "a6a1a434-6207-477c-9f64-f310c9a6abae");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "ca4b27cb-2a65-4d40-aa6c-c6dde43167e5");
                                              label = [ "quiz2" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "bb3197e5-bd52-4db8-b94a-84e9c9842bec");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
                                                        sort = Typ;
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
                                                     "70417645-6fba-435a-b779-8bdd0cd63f28");
                                              label = [ "Float" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "974b0a2e-0bb4-435e-b93e-4d42260047a5");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Typ;
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
                                                     "e9889fa2-96ed-4817-b38d-42bf7a3624fd");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "86d1bc24-8cba-417a-8023-99450d43b432");
                                              label = [ "midterm" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "08f7b548-24be-42f9-a5e4-0bf2b6854f9b");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
                                                        sort = Typ;
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
                                                     "193e5d87-e6a2-41c8-bc6c-95b7d22aacd5");
                                              label = [ "Float" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "71959fc5-335a-4ecf-8fcc-e4d2c0e68746");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Typ;
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
                                                     "d79afa2f-cfe8-485c-bd56-6276293e90ff");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "362b48c6-2288-43d9-bac1-cbd1d8640f02");
                                              label = [ "final" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                                     "cd20d8ea-fe97-403e-9201-bde3b3d8ec02");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
                                                        sort = Typ;
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
                                                     "d34c3e00-fa33-4667-91a0-bc79ac42ccb6");
                                              label = [ "Float" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                           "46d882a4-061d-48cc-9f8b-fe2112592d9e");
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
                                 "078f9c01-77b1-49f2-b0a7-a8c49d496c93");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "d48d4d5a-090f-483d-aa74-d3aea9cb6868");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "c01be4e3-2bbd-4277-a76d-b02a6fd93f65");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "ddd940fe-3e3b-4d9b-bd54-123a1386e4d9");
                          label = [ "let"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Pat; Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 45; sort = Exp } );
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
                                           "e9741674-7851-47de-91fe-945fbc81dd6c");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1006c8f6-e295-4d7e-8077-d6acdb628533");
                                    label = [ "tidy_term" ];
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
                                           "323b46cb-812c-43af-8088-cd2ed4793a19");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e6ceb7b7-c6a5-421a-ad5b-63d54fef1a8a");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "8477a893-15c8-484f-b82d-32f4bbd3413f");
                                    label = [ "fun"; "->" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Pat ];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Concave 37; sort = Exp }
                                          );
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
                                                     "93cdf3bb-1d29-466b-980b-2ca3cad5a68c");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "48a6ebc9-d7a8-44cc-b0ba-97f67ceb159f");
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Pat;
                                                  in_ = [ Pat ];
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
                                                               "fbeebbc9-f09d-4810-be0b-de6ddf2254e6");
                                                        label = [ "gradebook" ];
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
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "8905db20-1947-422f-8946-bca7cb4071aa");
                                                        label = [ ":" ];
                                                        mold =
                                                          {
                                                            out = Pat;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 24;
                                                                  sort = Pat;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 24;
                                                                  sort = Typ;
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
                                                               "68b3a451-e875-47ac-910b-fe9fa04ad91f");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "cc217b02-693e-40e2-be92-817729b5f8f2");
                                                        label = [ "["; "]" ];
                                                        mold =
                                                          {
                                                            out = Typ;
                                                            in_ = [ Typ ];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Typ;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Typ;
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
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "143b8263-683b-47b7-859f-371eb9851c97");
                                                                  label =
                                                                    [
                                                                      "GradebookEntry";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out = Typ;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Typ;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Typ;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                            ];
                                                          ];
                                                      };
                                                  ];
                                                ];
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "8cb4579f-41d3-4a8d-9ce7-611e88f0dfc1");
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
                                           "45fb505a-e2d6-4082-97c1-64f5a5028110");
                                    content = Whitespace "\n";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "f35b7f6f-c7a8-4853-9175-057409921bb7");
                                    label = [ "gradebook" ];
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
                                           "dd12d8a1-b78b-4241-8f70-e2e73b4eaf97");
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
                                 "8008de82-f2a5-45b8-81a6-be0be53fcf16");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "dd865093-9fc3-4564-8d51-aff36fb52941");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "9b6d18cd-947f-4c89-be4d-3b7157b34535");
                          label = [ "tidy_term" ];
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
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "c9a96d3c-8bdb-402a-83fa-b3099d3c49db");
                          label = [ "("; ")" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Concave 23; sort = Exp },
                                  { shape = Convex; sort = Exp } );
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
                                           "54754036-2c0b-49e1-a1e5-d68f4eed772c");
                                    label = [ "gradebook" ];
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
                              ];
                            ];
                        };
                    ] ) );
              ];
          };
        caret = Outer;
        refractors =
          {
            manuals =
              [
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "19c45d54-c545-45f4-a937-8afd5150c720"),
                  { kind = Probe; model = "((active_renderer()))" } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "fdcba018-42d3-4300-9d92-a2522672c4f2"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "a903d500-a1ba-417b-9c81-d8a00a67a176"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "70ce6dea-0a98-4b46-bb95-0553eaf343b6"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "852a94ae-d0b5-42fd-8a83-9a22b8e87642"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "d88a4d74-ea7d-4a4c-b411-b3c225065fd5"),
                  { kind = Probe; model = "((active_renderer()))" } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "4230da10-060b-49ca-bf52-370367bcd392"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "2def0551-398e-4d23-adc9-bba8e0df8142"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "6da3a0e5-6b6d-4179-80f9-36e9faf10c7c"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
              ];
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
                                 "0d936128-9ba7-416d-9fd1-d9fcce6f9517");
                          label = [ "let"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Pat; Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 45; sort = Exp } );
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
                                           "385f96f1-8122-46c2-8c84-aeadb2ae98e9");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "be2d4e2d-dd05-42c5-8121-2240c27805a1");
                                    label = [ "test_data" ];
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
                                           "5fe0b01b-3088-4bb6-a483-832eeade76d8");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "256a6b8b-683d-4b7a-bdd8-3225f150d35c");
                                    label = [ ":" ];
                                    mold =
                                      {
                                        out = Pat;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 24; sort = Pat },
                                            { shape = Concave 24; sort = Typ }
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
                                           "bcde3080-f4e8-46bc-93ff-c7e0dac4f14d");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "0cb89b04-6913-47a7-a696-35cb00867de2");
                                    label = [ "["; "]" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [ Typ ];
                                        nibs =
                                          ( { shape = Convex; sort = Typ },
                                            { shape = Convex; sort = Typ } );
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
                                                     "404a7933-9211-4ac9-bf1d-cf3ad3a54933");
                                              label = [ "GradebookEntry" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Typ;
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
                                           "aac7d8fb-61d0-41f8-a395-2756e9022eb6");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e9f4fe74-11f5-4868-953c-59acad905e28");
                                    content = Whitespace "\n";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "d36da87c-44a6-4a03-920e-f87021c9e4e3");
                                    label = [ "["; "]" ];
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
                                                     "d97ae3d8-b139-4797-8eb6-b3c335c56b57");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "27e56200-ef50-4ae3-ae1a-73ce7c8de3c4");
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                               "d0da9644-969a-479f-86ab-3b0f34443a44");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "7ab1499b-d223-40eb-81ec-50046a48d981");
                                                        label = [ "student_id" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "51b9e641-a9fd-452f-a87d-b279f94a9ffd");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "067cf588-217f-4973-a6d4-77c56fdea5d4");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "5ed56782-0662-4337-b0c3-926a9f95b7ad");
                                                        label = [ "1" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "a2545baa-a82e-40f4-8e81-e443680ae9f4");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "a358c380-aa74-4e40-9dd9-b4bab70db275");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "ba646ef4-073f-414c-8c4a-9fc732b2bd3d");
                                                        label = [ "term" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "ff7abad7-f294-4e02-b24f-cdb81b6f1355");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "d1ab6762-3cf6-400d-a375-ff0c5eb881a0");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "0e9b95c6-2f20-4ac0-bfe8-85ca56b2c2de");
                                                        label = [ "\"2025FA\"" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "cc44354f-cd54-4f01-a0d1-75ad4b5a7994");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "c2b586d2-0dbd-4a7a-9336-95cbd21a637f");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "450960c5-d37d-48b6-8dfc-feff5c35df46");
                                                        label = [ "quiz1" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "272bfbd9-02eb-4349-9567-7653033b4d48");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "98c98c71-2545-4654-8958-ea9cd667d488");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "ad85cae3-162d-4b83-9d0f-628fb220e149");
                                                        label = [ "8." ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "da4de4f9-81b9-4bb0-ab42-910ddd7d4ced");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "236d29f9-75ae-4c7e-b91e-6511d3a3a9b2");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "318ec6c4-4f83-4c84-86d1-5edd40008268");
                                                        label = [ "quiz2" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "67b21ef0-28f4-41aa-a34f-23dbfdefa7ba");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "c9286cc8-3a93-4e56-bbba-80de35b29dc2");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "0f71ceed-dae7-469b-b9ea-52c97a408960");
                                                        label = [ "9." ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "39e9c84b-5b03-4bb4-a173-4d9099845e71");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "853d6858-5559-4bd3-b66a-c55c3b8077a6");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "4d7ae050-755b-4a9a-8c06-752e9f7003f0");
                                                        label = [ "midterm" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "cb59c2c0-3b65-433c-af0e-011975a06da6");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "6ffa7b40-f0db-4232-9df7-9d7c08268c5e");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "d3d9229a-c933-4303-a68f-2b6040ebbd32");
                                                        label = [ "84." ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "2b6a2c8c-d4c6-4d3c-8fae-87b4c332d8e9");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "0032a46b-1c73-4394-b100-cb9490c1f45b");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "c328ebf7-e5b4-4fa7-a743-8cb97008cf42");
                                                        label = [ "final" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "7a018646-31c1-42fa-b4a4-0d959392a6a9");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "e1be0d94-362e-4ce3-8aa3-85a839e02569");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "97aa0ebf-a388-4bf2-b87c-674d7d1262b7");
                                                        label = [ "92." ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
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
                                                               "a833981f-23b2-4ffa-aedb-9dac9d6f44e8");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                  ];
                                                ];
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "4f587cca-eec8-4b79-a787-5386a2e64ddf");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Exp;
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
                                                     "3fd6103e-485b-495b-adaa-b7183a531c12");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "f7924c98-4e63-412f-873a-b1ef31e1a5ab");
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                               "41d087ea-f68e-4a66-a19f-06cc3afa22ec");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "779d1264-858d-4f91-bc6b-4ec2c4827029");
                                                        label = [ "student_id" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "d09d2e9a-a11f-4021-8ae4-0494e1142994");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "aafa06f3-ddbd-4310-8e3a-2cc154301bbf");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "93970f95-d531-4ed1-9ca5-65d963b1fa4a");
                                                        label = [ "2" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "ec0397e1-b18d-41d0-9aea-6829a18f191e");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "42abe1a8-22f6-4d81-a3cb-626f6d6f097f");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "88328ac6-b71d-48bb-bab9-46b15ab4b446");
                                                        label = [ "term" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "5636654a-d53a-47ad-a056-ab6c2ee0c69d");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "e02269be-ed7a-4e9e-92ef-563a48dbd3f7");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "7b5f5944-a7c5-4cc3-82e1-c8c9e000596b");
                                                        label = [ "\"2020SP\"" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "a28ff184-d736-41e1-a677-e49a7849086e");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "c50a9e88-ca3c-47dd-acc8-5dcf390d25d4");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "41a5c8c8-1478-4ba4-bf45-78d0e7aa1816");
                                                        label = [ "quiz1" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "c1f79748-7e0d-4d1d-9031-fbb85c688455");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "3d775ad1-8167-49fe-bc9c-342791ff969b");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "40b7cebc-b093-4351-9985-4e3865a4bbd5");
                                                        label = [ "6." ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "5877db3b-5883-405c-be5f-398f207b4b99");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "769314ee-2e3f-4669-a819-e37b3d3ec3f5");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "81e0294f-f601-4138-9da8-417773fa1036");
                                                        label = [ "quiz2" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "1470f452-8013-4d63-82fc-029807d0abc0");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "becae18e-a58b-49b4-afab-94ca5b37aae9");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "699fdcb6-3fb5-4a36-aecc-f5d111ad6836");
                                                        label = [ "7." ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "008cd5aa-1068-4e5b-ac63-e14206e3a69b");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "4e084ea2-3409-4914-be6b-48dc071cf6fe");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "82be8aa5-324e-417e-bad4-0e0f87d93342");
                                                        label = [ "midterm" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "bf225468-3031-4b77-855c-3a38fc060b8b");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "e5a99305-f0dd-423c-9469-9eedb1c9a2b5");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "ac228387-f366-4f5e-b663-3f2a2d31a1c2");
                                                        label = [ "78." ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "9664b229-e616-423b-9b59-5f7da505dc1c");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Exp;
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
                                                               "94e9cd0a-ddda-426d-88f8-6b113560dba0");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "38356e05-d82e-4bb9-b2ea-87d92f8cc522");
                                                        label = [ "final" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "9a337d30-2716-479d-8a0a-e0372c3344a1");
                                                        label = [ "=" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 39;
                                                                  sort = Exp;
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
                                                               "cb334aba-056a-486c-893d-a4f363b5bfcc");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "facbdf87-ea3b-424c-bf35-d1118998ffea");
                                                        label = [ "88." ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
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
                                                               "a9ad972b-c1ea-434d-9881-8e0ba5bb1b06");
                                                        content =
                                                          Whitespace "\n";
                                                      };
                                                  ];
                                                ];
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "6535040e-64ae-46bb-b540-52253ea753a0");
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
                                           "21010ebb-971a-4ad9-ac39-7012de54afc9");
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
                                 "79d931e2-abde-4a6c-ad13-f795843ace64");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "30ffb28b-97ec-4442-a8d7-bdc227e65598");
                          label = [ "let"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Pat; Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 45; sort = Exp } );
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
                                           "6aaaaf47-84b4-4c8c-a6f1-de09b8dec3c9");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ce836968-2f7f-4ba4-9a26-ae806855dec6");
                                    label = [ "result" ];
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
                                           "14d9ba3f-741b-448b-9bc4-c068757b4e31");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "b998be3d-896c-4fbf-a086-96879ed366b2");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "0807ec75-fa21-47af-bd74-bab6f8da25dd");
                                    label = [ "tidy_term" ];
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "64d80fae-d52c-45aa-8ed5-bdeaf78eb394");
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 23; sort = Exp },
                                            { shape = Convex; sort = Exp } );
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
                                                     "a6bee695-399b-4a00-aaf3-7f926e3980f2");
                                              label = [ "test_data" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                           "5ca84b8c-28e6-4be7-b6ea-b466465d28f6");
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
                                 "ac0e0b05-77b4-49cd-b56f-e441af6a63c6");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "eaa6c6a5-1df7-4b2f-9986-d1c4eb1e95e0");
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
                                           "0633021a-2305-4fde-a5e1-0b81bb39e163");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "39345d92-75ef-4843-a697-f7d008be4479");
                                    label = [ "result" ];
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
                                           "d2ddfcbd-1cad-45e5-98bc-830d742f90e0");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "eb3283e1-fe87-4518-9cf3-5461e2f6d4dc");
                                    label = [ "." ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 22; sort = Exp },
                                            { shape = Concave 22; sort = Exp }
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
                                           "28311383-4284-474b-9527-35cd670ade81");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e3372f13-0835-4b97-8c19-2cf3ac0929cd");
                                    label = [ "semester" ];
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
                                           "659082a5-dab0-401f-ac78-37c220b43429");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "9aea40d4-6b0b-4c68-be7b-c09fcff39188");
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 31; sort = Exp },
                                            { shape = Concave 31; sort = Exp }
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
                                           "d9fb8d15-005c-4498-b462-b6865275b745");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "4e539628-3424-4de6-b972-ac24240aeaac");
                                    label = [ "["; "]" ];
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
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "2f052fc8-442e-47f3-b176-f60125f2ae81");
                                              label = [ "Fall" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                     "29352003-5b4c-470b-bdd0-7c9f1409dffb");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Exp;
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
                                                     "8a3f5102-92d3-43be-99f6-0e0349b948b0");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "552d0a48-3790-40c7-a2fd-b65eb90e53f6");
                                              label = [ "Spring" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                           "e6cb4386-4eea-4257-a92f-1360a63105f6");
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
                                 "54bd635a-8f46-4a8c-9727-89f52599ca14");
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 35; sort = Exp },
                                  { shape = Concave 35; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "267bd0c1-2a01-4243-b25a-2a2a7139c45f");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "f3da4fb7-1630-45ca-a2a1-a6506bcc4fbc");
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
                                           "d9f3f5d4-5e73-4206-bebc-a76fa2498967");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "9658976a-6acf-4a83-a9e4-7658c3bde4e2");
                                    label = [ "result" ];
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
                                           "caabce44-d354-4d5a-a0ae-5d2a8c91640d");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "83b2992e-fff2-4319-92dc-00eb957d86df");
                                    label = [ "." ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 22; sort = Exp },
                                            { shape = Concave 22; sort = Exp }
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
                                           "6a6e3234-adba-4bf8-a21c-01a2fca4d535");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "8c868d71-0e6a-485c-bd95-0d026e4e69ab");
                                    label = [ "year" ];
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
                                           "c1da88e6-4a66-4c86-b54c-b0183ab45485");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "66388938-f8cb-4319-b655-f781704d35c6");
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 31; sort = Exp },
                                            { shape = Concave 31; sort = Exp }
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
                                           "399702b6-b4a5-414e-b91a-8400b6f93a6d");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e82dfa4e-1c5b-4209-a967-071257b555ec");
                                    label = [ "["; "]" ];
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
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "71b1a677-d9c2-4db1-8873-973fc1ed7b49");
                                              label = [ "2025" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                     "285cf1f6-0942-42fc-b27a-2e7876248063");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Exp;
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
                                                     "e9613a3d-de99-4e17-a7ce-f5370d709e61");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "2b5c213f-2fa9-4192-9568-2e6ccd234a7e");
                                              label = [ "2020" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                           "b94d22aa-d467-4dae-9ec7-8495fda71d2c");
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
                                 "7946af68-cd0c-42ac-8785-f79da800bc1b");
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 35; sort = Exp },
                                  { shape = Concave 35; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "3fe7d064-df2d-4e9d-8fa3-b23b49813709");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "e0dccca4-da22-4fd7-bb84-86f212f2d1e2");
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
                                           "9f6ab591-7c63-4bba-8dc1-bbc17473d171");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "b4385b9a-d189-4359-bbf3-bdfc75fda0bc");
                                    label = [ "let"; "="; "in" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Pat; Exp ];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Concave 45; sort = Exp }
                                          );
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
                                                     "3ee169cf-b68f-40d9-8419-987b99d9bb7f");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "2879adb1-c174-44e3-9e3e-25c73c70ed30");
                                              label = [ "columns" ];
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
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "8405d9ce-fd67-43c6-ad65-9dedfb98774b");
                                              content = Whitespace " ";
                                            };
                                        ];
                                        [
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "6cfa0bed-a0d9-48d6-bb0c-3601977f40aa");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "7a8ba4a1-0ef2-4c7b-a67a-190a96af311c");
                                              label = [ "to_lvs" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                     "31fb3cb2-22ec-4816-933e-bda5b193c06a");
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 23;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                               "72163393-b04e-40ee-b011-3d9156149f77");
                                                        label = [ "head" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "2a91430a-c97d-4b77-9fc6-3f953b36cd4c");
                                                        label = [ "("; ")" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [ Exp ];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 23;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
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
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "6aba6499-05f4-46fd-b49b-edd0138632e3");
                                                                  label =
                                                                    [ "result" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                            ];
                                                          ];
                                                      };
                                                  ];
                                                ];
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "f00fdf46-89fc-49c7-91af-c0e87fd07226");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "1632595a-2709-48b1-8ee0-e6c59fe4476f");
                                              label = [ "." ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 22;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 22;
                                                        sort = Exp;
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
                                                     "442a0785-71b9-4118-8f3a-b5c2b143eff1");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "2261e884-cc49-4e18-a965-26500180fb06");
                                              label = [ "label" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                     "064831dc-02f3-44bb-9362-d100643ac9a3");
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
                                           "984a1054-d922-41fb-9fe1-45979eb8c2c6");
                                    content = Whitespace "\n";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "19fefcde-0f25-47c6-b65f-a3138bb1d560");
                                    label = [ "mem" ];
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "a4dd8ab0-2ec6-47fa-b97f-f6c25ec4c95a");
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 23; sort = Exp },
                                            { shape = Convex; sort = Exp } );
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
                                                     "e3d24395-565a-4116-bd91-0d1290d173a0");
                                              label = [ "columns" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                     "1c13ce1a-fdea-4179-8b45-04717be9b6c5");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Exp;
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
                                                     "be8c71ae-be7e-4d71-a728-f4ad75af31be");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "6c33fb40-f422-4eac-85a1-5f7d83060f9b");
                                              label = [ "\"semester\"" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                           "a5a2bbc4-8894-4a1c-96cb-4041f481e132");
                                    content = Whitespace "\n";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "6a268a4d-3e9e-4b08-845a-78afc2cd11e7");
                                    label = [ "&&" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 32; sort = Exp },
                                            { shape = Concave 32; sort = Exp }
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
                                           "0b12ff0b-f908-42b7-9ceb-b0149a0def3b");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "adf405d0-7a80-42a3-88b5-6f3ba2c92dae");
                                    label = [ "mem" ];
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "605dfcdd-f7f0-4088-84bd-118cc0c09475");
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 23; sort = Exp },
                                            { shape = Convex; sort = Exp } );
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
                                                     "81244f2c-8c4c-443c-9f5b-02a916b08d67");
                                              label = [ "columns" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                     "0b7219ad-07c7-4dff-933f-b16ae96e4b25");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Exp;
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
                                                     "f32802e3-8e65-4301-a48b-ccb994e808c3");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "dc3fd770-e290-45d0-8bbc-f4cdf29d9146");
                                              label = [ "\"year\"" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                           "45f7505a-aa75-4727-9bec-51ccb47506b7");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "aa9c30b9-47cd-4dc0-a07f-d2f8ea52bd19");
                                    label = [ "&&" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 32; sort = Exp },
                                            { shape = Concave 32; sort = Exp }
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
                                           "969be24b-2195-4499-bcb0-90ef99c0853a");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "8c49cb7e-afe2-4765-94c1-7191cad9c1dc");
                                    label = [ "!" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Concave 27; sort = Exp }
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
                                           "e7224a5e-e21d-4d69-934c-7624589abac8");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "b82675f6-fd7a-47e7-8979-fe602c74a0e9");
                                    label = [ "mem" ];
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1c78f173-c5d3-4649-99c4-1cccb4f6e98e");
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 23; sort = Exp },
                                            { shape = Convex; sort = Exp } );
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
                                                     "929e8027-4280-44d8-8fc6-85efc3df689a");
                                              label = [ "columns" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                                     "efba45c1-946b-4923-9a1b-dac67b2f2c79");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 44;
                                                        sort = Exp;
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
                                                     "5697a348-4403-47eb-8735-7be33a3c0d68");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "29392ed3-3c7f-4894-82ab-6af718cc909f");
                                              label = [ "\"term\"" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
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
                                           "d8a1b63a-bc09-4c3a-b95f-1035c4608325");
                                    content = Whitespace " ";
                                  };
                              ];
                            ];
                        };
                    ] );
                ancestors = [];
              };
            caret = Outer;
            refractors =
              {
                manuals =
                  [
                    ( Option.get
                        (Haz3lcore.Id.of_string
                           "1632595a-2709-48b1-8ee0-e6c59fe4476f"),
                      { kind = Probe; model = "((active_renderer()))" } );
                    ( Option.get
                        (Haz3lcore.Id.of_string
                           "4bf489e0-f101-4efc-b219-79bde0433c82"),
                      { kind = Probe; model = "((active_renderer()))" } );
                    ( Option.get
                        (Haz3lcore.Id.of_string
                           "cae27004-0453-4e72-9090-2f6fa3c9f713"),
                      { kind = Probe; model = "((active_renderer()))" } );
                  ];
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
        hints = [ "semester"; "year"; "term column removed, new columns added" ];
      };
    wrapper = false;
    show_report = true;
    setting_overrides =
      {
        Tutorial.default_setting_overrides with
        rich_probes = Some true;
        display_tables = Some true;
      };
  }
