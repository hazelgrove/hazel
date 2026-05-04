let out : string * Haz3lcore.PersistentSegment.t =
  ( "Stepper Filters",
    {
      segment =
        "((Secondary((id \
         c1477c44-05e9-4810-b969-67375d23d23d)(content(Comment\"# We want to \
         skip over the evaluation of most expressions, ... \
         #\"))))(Secondary((id \
         7b7c03f4-7a5b-43dc-89c6-1780df1286a7)(content(Whitespace\"\\n\"))))(Tile((id \
         4ea94418-af04-4ee8-8b84-49202c3b3c1f)(label(debug in))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d8b193fd-d3b3-4c99-a9d2-0b19811e79d3)(content(Whitespace\" \
         \"))))(Tile((id \
         cb38906b-f3bb-496e-8211-e38002e321e1)(label(hide))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7bbad8d8-be5b-4f38-ab14-47583854fb55)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         337c541f-5300-48c5-9617-74c29248e403)(label($e))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ec618cff-c0de-4182-8d6a-74fa344c971e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         649ec23b-9a13-4fec-8219-0e3085ba4bf3)(content(Whitespace\"\\n\"))))(Secondary((id \
         73d4e493-1802-403f-8020-8e112e46947f)(content(Comment\"# So that we \
         can explicitly stop at some point in program execution. \
         #\"))))(Secondary((id \
         fe4cfe9f-2765-4daf-b202-bd18a7ef8956)(content(Whitespace\"\\n\"))))(Secondary((id \
         053fca60-0086-48c9-a40c-386751366ba4)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c691f5c-a172-4f33-b973-3f2fdaf10884)(content(Comment\"# Here is a \
         buggy factorial implementaiton. We know that fac(3) is problematic. \
         #\"))))(Secondary((id \
         9c8a8cca-ced6-4298-a3bf-f483728f3a35)(content(Whitespace\"\\n\"))))(Tile((id \
         d525bd72-1860-4342-b878-3d16a1664197)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d1f80e28-5a70-41d0-8cd7-f0265623684b)(content(Whitespace\" \
         \"))))(Tile((id \
         ddfe2ade-8cda-4d9e-82c4-bafa9c01cafd)(label(fac))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cde6d7f7-d2e1-4f2a-a432-d67f480fca5a)(content(Whitespace\" \
         \"))))(Tile((id \
         18d3568b-973c-43ce-b87b-3433e77dfdb1)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e78d6212-1e77-42d1-af8b-e5a31206f4d8)(content(Whitespace\" \
         \"))))(Tile((id \
         9127f76c-6c53-4715-be15-e07b6d2ce62f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         fb4b1c22-20b6-44b6-9f37-3bf1548f8fbe)(content(Whitespace\" \
         \"))))(Tile((id \
         f56fb730-0d31-497d-a3ef-d6b41474ca14)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         fa9554bd-9a1b-4adb-b78f-6cfdb7688718)(content(Whitespace\" \
         \"))))(Tile((id \
         f4580623-3369-46e9-9143-8ecc7e11664a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f8d937e1-e69c-4bb7-a0e1-ab85f463c675)(content(Whitespace\" \
         \")))))((Secondary((id \
         07e9b7a3-d18b-4f94-83d0-90e154137561)(content(Whitespace\"\\n\"))))(Tile((id \
         44eb0357-4f1f-43e3-9ac0-ba09ae7b3ff8)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         7a0612fa-5c4e-4c45-bc2a-1688bbb59777)(content(Whitespace\" \
         \"))))(Tile((id \
         2ece9893-ad91-4b66-b8d5-c8fb4cf33db1)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9fa636c5-cc89-4bad-bc6c-6c9208b19ba0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7c958ee4-ed39-45af-90b1-7eebff8da804)(content(Whitespace\"\\n\"))))(Tile((id \
         f759980f-34ae-47a9-a4d0-1ae102383ba6)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         aa9774ff-2c92-4d6a-a190-8dade16c7f3b)(content(Whitespace\" \
         \"))))(Tile((id \
         ca76db09-c42f-4496-b813-157137f50da8)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b0bd92ec-49d2-4285-9640-f42f90f60fa5)(content(Whitespace\"\\n\"))))(Tile((id \
         8457bacc-6461-42d7-8542-611ef19a2933)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5f81c133-965f-49db-af58-83715ae13e71)(content(Whitespace\" \
         \"))))(Tile((id \
         8f998190-b8f0-49d0-b7f6-78c3d149d4b6)(label(0))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bbb8c133-fb0b-4e18-aca3-df5bb4ab7759)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         88242498-653c-4365-953c-959dca8c1fa6)(content(Whitespace\" \
         \"))))(Tile((id \
         62d0ee85-851e-44dc-b836-391cbf38758f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a99eeafe-8d87-4d2a-a330-0665c7cca3d2)(content(Whitespace\"\\n\"))))(Tile((id \
         c21d7b6d-62c5-4922-8118-7930e02eee32)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         dcac906c-e16c-4b10-a4bf-512250c7dd5d)(content(Whitespace\" \
         \"))))(Tile((id \
         0f1a9e27-1090-4c36-9734-79cdc41a3825)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d276a59e-1466-4393-b0bb-bc7c2b44a83a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         302d66fc-b4d1-4e3b-bb82-b7562e3b3a5a)(content(Whitespace\" \
         \"))))(Tile((id \
         43979646-4aca-472e-a403-762c1710c78e)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bebb245f-b07a-411a-83e6-99ea3b8c7587)(content(Whitespace\" \
         \"))))(Tile((id \
         c3739d1f-8af4-446a-9eb0-23c0e605e65a)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aefe08dd-86b0-4804-9b44-bcb5dfd91c48)(content(Whitespace\" \
         \"))))(Tile((id \
         66277136-eb09-49c2-a93b-206a47ec6a24)(label(fac))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d141398f-bdbe-461c-8541-b084e296c615)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4296fc5c-2994-49b7-9fc5-5c634583eca3)(label(n))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         838dfc12-c41e-423d-ad84-456bd07070be)(content(Whitespace\" \
         \"))))(Tile((id \
         1da8cf77-c5f3-4030-bd5e-7f087fcee907)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f214c3d-7823-433e-a5cd-18a079202242)(content(Whitespace\" \
         \"))))(Tile((id \
         e3f98377-2459-4a4b-b9f8-9e692b8c1bf0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         081f9a64-190c-4915-abd7-de468964c09e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         63cc058c-fd83-47e7-bbbe-84c569df3439)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         7a09f2f5-791b-44c4-b940-17fbf2b8bdcf)(content(Whitespace\"\\n\"))))(Secondary((id \
         3750ff4d-cf3b-4d90-be0b-79612b394b63)(content(Whitespace\"\\n\"))))(Tile((id \
         8c34b01a-a611-49d8-b52c-487e46b0591b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         576fbc21-48d7-43dc-86c4-b007d74f8eec)(content(Whitespace\" \
         \"))))(Tile((id \
         fdabce1d-1a49-4b55-b599-babaf9c86b02)(label(stop_at_fac_3))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f79770ea-2f87-4ed0-80cb-5863dc390076)(content(Whitespace\" \
         \")))))((Secondary((id \
         cc7d7088-b43a-4e9a-9483-6aa5d56a521c)(content(Whitespace\" \
         \"))))(Tile((id ec8d0bea-d355-4bab-a0e6-0feedb92b2fa)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8dd2e019-d4bd-4c01-b31c-876d6141298d)(content(Whitespace\" \
         \"))))(Tile((id \
         ebfea817-8df3-48a5-9f2e-1c83d4d03764)(label(\"()\"))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4d974f25-b529-4229-8d94-3af6e3222501)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         662d0cae-9664-4d5c-825b-a4fb41a76b5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         697e26b7-bc13-4b15-b657-3f1a26c0a236)(content(Comment\"# Therefore, \
         we can stop at the step where the program is about to evaluate \
         fac(3): #\"))))(Secondary((id \
         5a7bf3e1-97fe-4621-b623-a466c9c76a08)(content(Whitespace\"\\n\"))))(Tile((id \
         4d768e07-3b1c-44d7-998b-952e63511ef1)(label(debug in))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b42c453b-81a6-4cd9-95c0-ccb5135da05f)(content(Whitespace\" \
         \"))))(Tile((id \
         6d786dc1-3c2c-4a3d-b165-41d6045765b6)(label(step))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         776d5970-9145-4269-84b0-4eb00c3c45a1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f0432083-e925-4f83-87c3-0aa9edaa4378)(label(fac))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af6b2115-7711-467e-aff9-50d1f68eadd6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         59b882be-a618-49a0-be1a-f2d079f5b74a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         a2b621eb-c509-44eb-a64d-3d8415875c8a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5ec97e0d-b747-453b-a9e0-dfa302c4cb15)(content(Whitespace\"\\n\"))))(Secondary((id \
         f55b51a8-e8e8-43c3-aaed-397bf3e07480)(content(Comment\"# We run our \
         debug-expression through the evaluation of fac(5) \
         #\"))))(Secondary((id \
         8bc52018-bcc4-4335-a77e-0633bfead0ca)(content(Whitespace\"\\n\"))))(Tile((id \
         8a3d587a-5949-454a-9b37-89ebeeb069af)(label(fac))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         856f3054-472f-41b5-8767-a189406cf261)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         717146f4-656e-4664-9614-98a2b031af8b)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ae609dee-f518-4e1c-99b2-65f2ea2f0b7a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0731e38c-4e10-49a0-af57-bd2e88bb0fce)(content(Comment\"# The program \
         will stop at 5 * (4 * fac(3)), and we can take over and start to \
         stepping through the evaluation of fac(3) manually. \
         #\"))))(Secondary((id \
         6b3a4e83-5004-46f1-a1bd-e78c710a1d2f)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         54392ad8-cfd6-48b3-8ba3-fb101cd13a69)(content(Whitespace\"\\n\"))))(Secondary((id \
         6100fb91-811e-4020-a159-e6497e88d0ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         c40afa93-77f9-4bd9-8e28-60cecd086efb)(content(Comment\"# Now, here is \
         a correctly implemented map function that applies function f over all \
         elements in array xs. #\"))))(Secondary((id \
         df85adb1-49a9-4ee9-9fbf-b54c306bac2a)(content(Whitespace\"\\n\"))))(Tile((id \
         0ab2ba9f-5f85-4182-944d-76396840b1a3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         aa2ea586-8c22-4590-af43-43a0cc1fa8af)(content(Whitespace\" \
         \"))))(Tile((id \
         23fad498-e1a0-4585-91cb-29af3fc8b783)(label(map))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f43799d0-acc6-40d7-b46d-9b5630cabe19)(content(Whitespace\" \
         \"))))(Tile((id \
         ddfeb10c-5d86-4ee3-895f-af7e68148e02)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         55cc8969-cd10-44a3-92fa-1bd0b5d91f43)(content(Whitespace\" \
         \"))))(Tile((id \
         c0b27679-c688-49a8-a52f-edf870717020)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         d4240403-2541-4083-bb71-a8be4ae47a5b)(label([ ]))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         fe5ec5a1-87fe-4c90-ba7a-cf298ca00422)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Tile((id \
         09497ef4-c75b-4533-a423-836c27af3d30)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e051c46f-b65b-49db-b708-51a07600a088)(content(Whitespace\" \
         \"))))(Tile((id \
         40c2c753-e8d3-42e9-aaec-b41fbae82e46)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e79e9601-0314-4bf4-ba84-ab61527c2f98)(content(Whitespace\" \
         \"))))(Tile((id \
         7804411c-adc9-4400-a195-3431b78f5c71)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         301a4207-189e-4b5e-a211-4958c395b424)(content(Whitespace\" \
         \"))))(Tile((id \
         49dcc917-de72-40e5-9ba5-968f39f6dfb4)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         3f89fadd-9c26-47c1-8852-1adae5693a07)(content(Whitespace\" \
         \"))))(Tile((id \
         2b6371b2-9fd6-49cb-b374-d48fdb4674e8)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d2f686b5-d2fc-4b81-98cd-2b570480a1ff)(content(Whitespace\" \
         \"))))(Tile((id 121b2fc1-d223-4169-a56c-fd7696a75a75)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         004c02e8-0547-464d-b29e-4d464f098236)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         40c560ea-9623-46dd-9c3f-58eac1829a21)(content(Whitespace\" \
         \")))))((Secondary((id \
         04f209a0-2ca3-4424-8ea1-33d64d80d05d)(content(Whitespace\"\\n\"))))(Tile((id \
         2e8c8064-2f9e-42c8-a8ea-493bba6efcd6)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         aa9319a5-bf21-403c-80cd-6a9b00da1c5b)(content(Whitespace\" \
         \"))))(Tile((id \
         177680c8-1d6f-40fe-9075-97f7880ba73c)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         96747217-7751-4683-ae75-060e292341ab)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         fc9c127f-47e4-4f95-acc7-f7edc6eff183)(content(Whitespace\" \
         \"))))(Tile((id \
         8bfa5a26-a066-44c7-b589-1ae210236eb1)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b40057e0-0cda-41d3-9626-474171b08c00)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         adec0b6f-a926-4d5b-b2b7-d2cc28d3cd53)(content(Whitespace\"\\n\"))))(Tile((id \
         494253b1-346c-4db0-83f6-1dcbcbbcff93)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b646c80b-5852-452b-9b78-f6354a5e5e0d)(content(Whitespace\" \
         \"))))(Tile((id \
         ab1ec388-d320-48fc-8b82-6da4d68f1dbd)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a48c412c-9049-4b68-aedd-c5ea48dc14a3)(content(Whitespace\"\\n\"))))(Tile((id \
         34ee3285-f5a9-42a1-aa24-48b4d7591e7f)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e0df73fc-76c6-4f74-b723-519ae7b6258d)(content(Whitespace\" \
         \"))))(Tile((id \
         f5db8446-5803-43b5-9856-7d3cf439d76e)(label([]))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         877d645f-a928-419a-aff6-df6218921c02)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         944eaf9c-9f05-4e79-97d1-98d7b989ae9e)(content(Whitespace\" \
         \"))))(Tile((id \
         008d10fe-8285-4926-8241-3d14463acc97)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         93e72964-e7c5-4afa-8ec4-5a5f2475a665)(content(Whitespace\"\\n\"))))(Tile((id \
         89ccbe20-9d76-4946-8cc2-66f46a26dbed)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         648a84bf-f88a-41c9-a0f8-ab21bfbb3492)(content(Whitespace\" \
         \"))))(Tile((id \
         6a0f1e53-442a-4357-b542-18cc18cc0bc6)(label(hd))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         da88f4f0-bb12-4d12-835a-80993ee674b7)(label(::))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 29))(sort Pat))((shape(Concave \
         29))(sort Pat))))))(shards(0))(children())))(Tile((id \
         85909875-98a5-49fc-b497-e0a2dcdecfbe)(label(tl))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dcd841e9-099c-480e-a75d-dd8e2d4af9bf)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5b5d33b1-6367-4349-91ec-dd6f59d468e6)(content(Whitespace\" \
         \"))))(Tile((id \
         561e66d5-1c8a-4f9f-806c-91d619b37221)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1119a1c2-a1b8-4d13-a9da-763a16a01e1e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c1bad901-0ea7-4972-a6ea-b2860f1af932)(label(hd))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         5c4cbc9c-648a-4910-b3b7-9fc340e16937)(label(::))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 29))(sort Exp))((shape(Concave \
         29))(sort Exp))))))(shards(0))(children())))(Tile((id \
         184edcf9-f1c6-4685-9f53-a9597c59e5db)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc73f31c-e4de-407b-9e90-22eb5f96d3f2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e0b9ed6a-93fa-46d4-a48e-3b81c2385511)(label(tl))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5e93061a-868e-475c-b78e-30c644cee55b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         98d1e64b-fdc8-47d4-8428-78debb1f3435)(content(Whitespace\" \
         \"))))(Tile((id \
         d189b6d2-3b13-4581-92f6-62ff7c823eb9)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5fc6e82d-acd3-45d4-b9b5-5830f331f053)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f95eafc7-f69f-4e0d-bdf7-25a8bbdd9ee0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ae50bf00-a782-42b7-8854-ab4523bb1608)(content(Whitespace\"\\n\"))))(Tile((id \
         47eb7cd2-6c46-4aa3-96a9-36fe6d4d6a1a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3a64a3d7-7cdd-4652-953a-8dc989782a9b)(content(Whitespace\" \
         \"))))(Tile((id \
         251fb5bc-64a0-4e60-9ad8-dad09cd69057)(label(square))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         721ef875-daaa-4a10-9514-499895eb7572)(content(Whitespace\" \
         \"))))(Tile((id \
         ce983e8a-573d-4870-84b1-26c22bc7cfe7)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0344b649-801c-4dbe-abae-667eec7d8bd6)(content(Whitespace\" \
         \"))))(Tile((id \
         827929bf-b196-4bb4-8fd0-3ed5cfa4d882)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8b5f5686-9189-4249-8eba-3dddef95fcb3)(content(Whitespace\" \
         \"))))(Tile((id \
         b08ec0c8-816f-4697-b2bd-069b4cb2b77e)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5f8da8b7-ae1d-4db2-92a7-22c73cde97e3)(content(Whitespace\" \
         \"))))(Tile((id \
         09395b09-6698-4916-8dc5-34f5d6240e1d)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         227ece1b-7bb1-4740-8e3a-944ff672d4f3)(content(Whitespace\" \
         \")))))((Secondary((id \
         3f5916ad-2fec-450d-94a8-ed4aad04ca10)(content(Whitespace\" \
         \"))))(Tile((id 74eacfd8-8988-4925-9917-ae3deb2f0c49)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         294ddb7d-29c2-4605-814f-329907ec5766)(content(Whitespace\" \
         \"))))(Tile((id \
         0f897ecd-b4ae-4ecc-99b2-fd81ef78a41c)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         abf61520-7dbc-4ddf-9558-076bd40eafcc)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         944d5552-157b-4236-a2f4-19c0962db5f6)(content(Whitespace\" \
         \"))))(Tile((id \
         da0a1e05-4253-4407-a978-98c33ff6a1ad)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d84d6c2-3496-4cd4-8dd7-e7c7aca0eb8a)(content(Whitespace\" \
         \"))))(Tile((id \
         9df85cb0-4b3b-4eed-bff7-fbda0d3c61ff)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce70c735-c90b-4f29-8a7c-3ad6a3f9762c)(content(Whitespace\" \
         \"))))(Tile((id \
         ced5b077-9c30-4744-b3f9-74eeafd47fc6)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         52935d53-831d-41a6-8b7e-ff79590b02b0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e2d1ac53-40a7-492d-a241-34e61d88e733)(content(Whitespace\"\\n\"))))(Secondary((id \
         e751acef-3602-458a-b992-106d46a9325f)(content(Whitespace\"\\n\"))))(Secondary((id \
         3fc9fdef-e3fd-4e65-a5b6-9afef17db56c)(content(Comment\"# One can \
         easily verify the function actually does such thing, by ... \
         #\"))))(Secondary((id \
         6e5c937a-a6d4-40e1-94af-c966a366ae1d)(content(Whitespace\"\\n\"))))(Tile((id \
         421eec6a-ea21-44a7-861b-0e05a42de3be)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ee6e1232-eb3d-4031-85d2-837f34d852b4)(content(Whitespace\" \
         \"))))(Tile((id \
         a0e0ada1-2b2b-4799-bfda-ae741ba7c9b8)(label(stop_at_square))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6c1860ba-32d3-45fb-b220-24539a0a4727)(content(Whitespace\" \
         \")))))((Secondary((id \
         412ce9dd-7c1e-4dc4-a4d8-4b7214d21f34)(content(Whitespace\" \
         \"))))(Tile((id 82161c22-8194-4419-9747-051e26a8180e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         cf1229c7-f090-4eac-b9dd-fd663038121f)(content(Whitespace\" \
         \"))))(Tile((id \
         f6b884e9-189c-47a9-ac55-5b77e6e2f0fe)(label(\"()\"))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         027fce10-448b-409d-95f0-0b4bc30db1f0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         56ae5a09-681c-41e0-b817-0bd5a75c984a)(content(Whitespace\"\\n\"))))(Secondary((id \
         abb91184-94c3-45b1-8776-7f794f6c0b0b)(content(Comment\"# Stopping at \
         each application of the function square #\"))))(Secondary((id \
         146a064b-cd6f-43f9-9131-caa79f9a8fbd)(content(Whitespace\"\\n\"))))(Tile((id \
         4fb760bf-3569-4140-a4a1-59fb9226fbca)(label(debug in))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         68164552-487e-446f-b8ac-ef38a8789372)(content(Whitespace\" \
         \"))))(Tile((id \
         4eeaebd9-62cc-469a-97d6-6f4206142781)(label(stop))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7f077b69-3a9d-4109-8171-801386840cd6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1535e4f5-328b-4126-9746-71d88874f21b)(label(square))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1dea4a81-ed9d-4826-a830-a930bd4b4d5a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bc8db55f-482f-40de-adf4-29bcfd28b764)(label($v))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         17415fbc-d297-44cf-bf63-8c08e769ac5d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cefcca6f-31b1-40f0-94ac-6c44bd3db8ac)(content(Whitespace\"\\n\"))))(Tile((id \
         da1694b4-6336-4cd3-aa7f-cc781e35f194)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9eaa8713-9b34-4286-83c7-3258bdecc03e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         19f58516-73eb-4631-9636-e8099abca427)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3fbc580d-b244-4c33-bf8c-e8cc380afeb6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         727017eb-f021-4a32-b2c3-7bfedabe018a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7498d104-2a6c-4262-84fa-486d01cc10f6)(content(Whitespace\" \
         \"))))(Tile((id \
         3dd00361-c5ff-4704-a66d-0ebc57b5cfc9)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8763a6fc-a278-48b4-8482-2570976b42ba)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         810eaa96-453b-4f8c-a35c-552918a901f6)(content(Whitespace\" \
         \"))))(Tile((id \
         7a5175ca-1fa3-4c6f-b6af-06250ed90ccb)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b83a1e7f-76f4-4111-84e6-3aa265633ff0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         894db2c8-4d33-4eee-9f71-0e0feef85903)(content(Whitespace\" \
         \"))))(Tile((id d7414812-5dda-4ffd-9c1a-68001bbca082)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e4209a3c-ed9a-48f2-a6c4-e699a27e7676)(content(Whitespace\" \
         \"))))(Tile((id \
         91011fbe-024b-4e53-a371-82924ca0a8de)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         64d0f38c-f073-44f0-b5eb-0accb56154f4)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         356acc9a-85b5-4d43-833f-75ea84fe61e3)(content(Whitespace\" \
         \"))))(Tile((id \
         1c3e8a12-05b6-4842-bde0-0c6167d005e7)(label(square))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b5431302-bebc-4a88-8771-de33b48830aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         97f4f1ef-27fe-40d6-a205-5e39b83e7220)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         a1753ef6-873d-4dec-9710-f14bacabb47d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e304ad2-e548-449c-8279-ffeb7ae3b718)(content(Comment\"# The program \
         will stop at [square(1), square(2), square(3)] #\"))))(Secondary((id \
         138cd1be-42de-4fed-bb32-369e3c8b8b4a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         01b81139-0c67-42ee-8452-63c59ebd13d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b56667b-7fb8-496d-8a3b-1f40b5e9d879)(content(Whitespace\"\\n\"))))(Tile((id \
         ffce14e5-d862-4b38-9618-c16f344f54f0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f1c0bfa2-6654-4c09-ba5b-61e5140a2ff6)(content(Whitespace\"\\n\"))))(Tile((id \
         e0ba3725-c951-478d-83dd-00aef4aef1af)(label(stop_at_fac_3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fdfd05f6-6c4e-43f4-9d6b-521d43a78dc2)(label(\"()\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 23))(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         79bef5cf-726f-43a6-a467-92171c33546d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         accc2952-c31d-4f62-996f-247e7d80c95a)(content(Whitespace\"\\n\"))))(Tile((id \
         2600d2df-8daa-4867-8678-18000b3a5124)(label(stop_at_square))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ce54f633-d6ec-45cc-b3e8-dd1c51019907)(label(\"()\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 23))(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c135c7f8-86d3-49bc-812d-4faee1eed56d)(content(Whitespace\"\\n\"))))))))))";
      backup_text =
        "# We want to skip over the evaluation of most expressions, ... #\n\
         debug hide($e) in\n\
         # So that we can explicitly stop at some point in program execution. \
         #\n\n\
         # Here is a buggy factorial implementaiton. We know that fac(3) is \
         problematic. #\n\
         let fac : Int -> Int =\n\
         fun n ->\n\
         case n\n\
         | 0 => 0\n\
         | n => n * fac(n - 1)\n\
         end\n\
         in\n\n\
         let stop_at_fac_3 = fun () ->\n\
         # Therefore, we can stop at the step where the program is about to \
         evaluate fac(3): #\n\
         debug step(fac(3)) in\n\
         # We run our debug-expression through the evaluation of fac(5) #\n\
         fac(5)\n\
         # The program will stop at 5 * (4 * fac(3)), and we can take over and \
         start to stepping through the evaluation of fac(3) manually. #\n\
         in\n\n\
         # Now, here is a correctly implemented map function that applies \
         function f over all elements in array xs. #\n\
         let map : ([Int], Int -> Int) -> [Int] =\n\
         fun xs, f ->\n\
         case xs\n\
         | [] => []\n\
         | hd::tl => f(hd)::map(tl, f)\n\
         end\n\
         in\n\
         let square : Int -> Int = fun x -> x * x in\n\n\
         # One can easily verify the function actually does such thing, by ... #\n\
         let stop_at_square = fun () ->\n\
         # Stopping at each application of the function square #\n\
         debug stop(square($v)) in\n\
         map([1, 2, 3], fun x -> square(x))\n\
         # The program will stop at [square(1), square(2), square(3)] #\n\
         in\n\n\
         (\n\
         stop_at_fac_3(),\n\
         stop_at_square()\n\
         )";
      refractors = "()";
    } )
