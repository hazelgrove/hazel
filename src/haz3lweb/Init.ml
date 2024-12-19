let startup : PersistentData.t =
  {
    scratch =
      ( 0,
        [
          {
            root_id =
              Haz3lcore.Id.of_string "e87c8d67-9374-4a6f-ba01-5ec8f300b924"
              |> Option.get;
            components =
              [
                {
                  id =
                    Haz3lcore.Id.of_string
                      "e87c8d67-9374-4a6f-ba01-5ec8f300b924"
                    |> Option.get;
                  parent = None;
                  kind = None;
                  model = "";
                  editor =
                    {
                      zipper =
                        "((selection((focus Left)(content())(mode \
                         Normal)))(backpack())(relatives((siblings((((Grout((id \
                         e87c8d67-9374-4a6f-ba01-5ec8f300b924)(shape \
                         Convex))))))(ancestors())))(caret Outer))";
                      backup_text = "";
                    };
                };
              ];
          };
        ] );
    documentation =
      ( 0,
        [
          ( "The only slide",
            {
              root_id =
                Haz3lcore.Id.of_string "e87c8d67-9374-4a6f-ba01-5ec8f300b924"
                |> Option.get;
              components =
                [
                  {
                    id =
                      Haz3lcore.Id.of_string
                        "e87c8d67-9374-4a6f-ba01-5ec8f300b924"
                      |> Option.get;
                    parent = None;
                    kind = None;
                    model = "";
                    editor =
                      {
                        zipper =
                          "((selection((focus Left)(content())(mode \
                           Normal)))(backpack())(relatives((siblings((((Grout((id \
                           e87c8d67-9374-4a6f-ba01-5ec8f300b924)(shape \
                           Convex))))))(ancestors())))(caret Outer))";
                        backup_text = "";
                      };
                  };
                ];
            } );
        ]
        (* ,
           [
             ("scratch_ADT Dynamics", Evaluation);
             ("scratch_ADT Statics", Evaluation);
             ("scratch_Basic Reference", Evaluation);
             ("scratch_Booleans and Types", Evaluation);
             ("scratch_Casting", Evaluation);
             ("scratch_Composing Arithmetic Expressions", Evaluation);
             ("scratch_Compositionality", Evaluation);
             ("scratch_Computing Equationally", Evaluation);
             ("scratch_Conditional Expressions", Evaluation);
             ("scratch_Functions", Evaluation);
             ("scratch_Polymorphism", Evaluation);
             ("scratch_Programming Expressively", Evaluation);
             ("scratch_Projectors", Evaluation);
             ("scratch_Scope", Evaluation);
             ("scratch_Shadowing", Evaluation);
             ("scratch_Types & static errors", Evaluation);
             ("scratch_Variables", Evaluation);
           ] *) );
  }
