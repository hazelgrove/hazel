let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-solution",
    {
      segment =
        "((Secondary((id \
         192f4a70-25d8-49dd-90f5-47765369500f)(content(Comment\"# MENTION \
         EXTRACTOR - SOLUTION #\"))))(Secondary((id \
         dc499574-12f9-44cc-ab95-99c23eb08b86)(content(Whitespace\"\\n\"))))(Secondary((id \
         e06f4b23-145c-4fab-aea3-08d7a780ba71)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ea337e5-edcf-494d-ba21-75848c90d76e)(content(Comment\"# Check if a \
         word starts with @ #\"))))(Secondary((id \
         3834d557-48a5-4fee-8c6b-9e3d45940eae)(content(Whitespace\"\\n\"))))(Tile((id \
         e2d437cc-277e-49db-82ae-07ddfe167d79)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         96d112d1-5d38-4cee-89db-c2f10f18e0e6)(content(Whitespace\" \
         \"))))(Tile((id \
         11d08434-dbfb-418d-b4d0-afebbf79f74b)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ecf5fb31-168c-4d6e-a4ca-5a905babda04)(content(Whitespace\" \
         \")))))((Secondary((id \
         9f11a2ea-c82c-4ef2-a89a-fc28b8015b64)(content(Whitespace\" \
         \"))))(Tile((id 9fc7fc5d-b766-470b-aa88-87ff7394b348)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5184f355-4e2b-4c70-876e-8c7e11ac9b1e)(content(Whitespace\" \
         \"))))(Tile((id \
         420ede78-a48d-4671-8d56-9a79bb9d354f)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9d88e3a5-6dbc-418c-9c63-5ce41da3d1df)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d18d5a68-cb9c-4a63-875a-698941aa6004)(content(Whitespace\"\\n\"))))(Tile((id \
         78be4e9e-dc98-4ea7-bbe8-772099f88036)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e456bab-aa29-4d9b-a9a5-fe49f752f8cc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7ef61e45-829a-4c33-9636-983b913d5dbb)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ddc9e78b-3fa2-4415-bcfe-5992fd2b0b3b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         829b5e49-5612-4120-933a-40abc559882b)(content(Whitespace\" \
         \"))))(Tile((id \
         7d2279e0-5d20-4cf0-8128-a8f39f04e6d3)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         97f85f6e-5602-4318-88ba-efff464e49fc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44173b22-72ff-4197-87fd-d97b06a25736)(content(Whitespace\" \
         \"))))(Tile((id \
         d0b75d24-e923-41cc-8a2d-b30e57ddc04f)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ef0f4d88-f62f-462b-900f-7605d1b89f98)(content(Whitespace\" \
         \"))))(Tile((id \
         c3494023-f86f-4af7-8424-c094457103b5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5321f1ef-4b28-4da6-9190-43699f68ba74)(content(Whitespace\" \
         \"))))(Tile((id \
         4b01138e-aae0-48fa-ba01-7dff265b056a)(label(\"\\\"@\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a2eacb0d-1e4a-4b4e-ba87-851048fd6c1e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e9fb9627-b143-4f04-b5c0-9c4b8054db15)(content(Whitespace\"\\n\"))))(Secondary((id \
         4a3ed3a4-c34b-46c6-b69c-ccf26b641b7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         07c02dde-2954-4001-81ad-976c64a97757)(content(Comment\"# Remove the @ \
         prefix (take everything after index 0) #\"))))(Secondary((id \
         18745af2-1c58-4dfc-8532-fecfdae8f899)(content(Whitespace\"\\n\"))))(Tile((id \
         359232cd-e95b-46dc-be95-5b1b57bc7a5e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1e9e74db-9086-4637-8e27-77be7a26b275)(content(Whitespace\" \
         \"))))(Tile((id \
         7ce86352-cdbb-4629-839e-f019b73f3413)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7ee92929-c693-4a97-8ea2-a69742f6e637)(content(Whitespace\" \
         \")))))((Secondary((id \
         378f2388-48bb-4f29-9006-bee4fd0e623a)(content(Whitespace\" \
         \"))))(Tile((id 781a107c-dbb0-4e24-b3e3-a599f2e33662)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         292ac61a-fd82-4920-b856-af04e634bf63)(content(Whitespace\" \
         \"))))(Tile((id \
         ad0428fb-c210-41a9-86bc-1faee12847a7)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ab77fa29-a381-4aef-a748-2093905d2a6b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b57a9c89-9ae5-4c11-a838-799a8a39ffa9)(content(Whitespace\"\\n\"))))(Tile((id \
         8974a79d-72fe-4a55-927c-3a16dbdf9406)(label(string_sub))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5024bb8b-cace-4dea-ac9f-804726b0011c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9c5a531c-224b-4932-a825-1c9719952b85)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1133860d-29df-4601-9c66-7b27858d540d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f226fc7-b769-43cb-8694-67fa2c04dfea)(content(Whitespace\" \
         \"))))(Tile((id \
         f3294b58-0eef-4d89-ad94-c4a90fc1740e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7cedc45-093e-4fad-a92e-7ce2e749dc75)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         096eabf0-d25a-4145-ac57-71c1bb463642)(content(Whitespace\" \
         \"))))(Tile((id \
         b2da0ffe-2c94-4b62-ab37-e2a6d4943195)(label(string_length))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         72f4ce51-b8a3-4ab2-ab1b-a9c0984f5cc9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         48f54ca2-0e82-4ff6-96f4-c6a7072d8463)(label(word))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6e8c49d6-ac3b-43b7-b46c-ac17b1545d7c)(content(Whitespace\" \
         \"))))(Tile((id \
         9ec5c894-7ed1-4561-8d9c-df4e005f92e7)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27064642-54ea-4dc9-b487-3b85487c1d0e)(content(Whitespace\" \
         \"))))(Tile((id \
         a091730b-4514-4377-9564-cd789fd075d4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1f909dfd-da96-46d0-a309-b9e5b8da8edd)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8c9231b4-3b9a-4d58-95da-d5c93a12a9d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         188f18a8-bea2-4685-acdc-20bccc0d663b)(content(Whitespace\"\\n\"))))(Secondary((id \
         b192a43a-8efd-408c-a64d-0378b21be718)(content(Comment\"# Extract \
         usernames: split -> filter -> map #\"))))(Secondary((id \
         0c5e2b31-c1a4-4cf6-878f-e2c3fcf4e642)(content(Whitespace\"\\n\"))))(Tile((id \
         bca96e2d-ccfc-401a-afe4-5c5710b59197)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1c3a977f-cb62-41b0-9cbc-c8db3302f851)(content(Whitespace\" \
         \"))))(Tile((id \
         c636b764-1bec-4581-b5e3-d804deca4609)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fcf59966-650e-4c5d-bbac-b44f818d3518)(content(Whitespace\" \
         \")))))((Secondary((id \
         08feb103-01c6-4948-a301-edc92b424aa8)(content(Whitespace\" \
         \"))))(Tile((id 8cee571a-1aae-4e0a-858f-18d6bb439527)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         88e72456-7a87-4752-8a1d-432241d4ddc8)(content(Whitespace\" \
         \"))))(Tile((id \
         da7c2cc2-f66c-4ca5-b05e-dca3bd3d2297)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         581f5885-6651-4804-8034-00f9008e2f73)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e830db13-23aa-4ddd-bcb6-f9e2cf2de36a)(content(Whitespace\"\\n\"))))(Tile((id \
         af815947-83fd-42b5-95a1-c918401989f3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d96e443c-d5d1-459d-8d62-45afe5ff0b90)(content(Whitespace\" \
         \"))))(Tile((id \
         e7ff1fe2-0626-43da-934f-fe31e05cf11e)(label(words))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         05557285-a3db-4280-bab8-3b9f4caeb59e)(content(Whitespace\" \
         \")))))((Secondary((id \
         d5e4d3af-b3c1-458e-b631-07a6b34a7d22)(content(Whitespace\" \
         \"))))(Tile((id \
         88f2cb6c-6108-4612-ae4a-826e0d6892b2)(label(string_split))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ed8b9f82-390a-4d96-a205-5356b3793006)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0d7ada48-22f0-4bf7-985a-1c9ef72d20d7)(label(\"\\\" \\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ea5d7efb-0b51-4a48-9cb7-45404c44b802)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b15461e9-dbbf-4c1c-87f1-bfee4af6a8b9)(content(Whitespace\" \
         \"))))(Tile((id \
         3f8bd9cc-226f-4d47-b841-280ccd8a8b80)(label(message))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e59b7b57-f78c-48fb-827b-f9978960e073)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         805d4c9c-21a0-4245-bd89-b71e3b7e7266)(content(Whitespace\"\\n\"))))(Tile((id \
         fb0572f0-de1a-401a-a1d5-68fcb6f03414)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         82051239-2e98-483f-8af4-212368bf20d2)(content(Whitespace\" \
         \"))))(Tile((id \
         1494a037-365d-4597-bde1-d2f354ed8633)(label(mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         ac4ca709-4b27-4332-9ca3-781c5441522e)(content(Whitespace\" \
         \")))))((Secondary((id \
         e0da371b-51eb-4bda-9cb8-1edc2880f216)(content(Whitespace\" \
         \"))))(Tile((id \
         976ed626-8dd4-493b-b8ea-009b2eec232f)(label(filter))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b22e1a3f-d756-4f79-8767-90f6960cc497)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         054efe93-3f6b-4438-a6a4-cb68bb1ef38d)(label(words))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f64224c7-2e7c-4a61-a3c5-3d5210c39431)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b03c673d-a32b-42b5-a7d4-762d47ba569c)(content(Whitespace\" \
         \"))))(Tile((id \
         d80e9260-9e7c-4d1d-88bf-e879fe557664)(label(starts_with_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         0d11b666-7cd0-4fd1-971f-353850e3abb2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8fab1aee-3f36-44e5-9dd9-e7584db1bbfd)(content(Whitespace\"\\n\"))))(Tile((id \
         5279f4f9-4f40-47d3-b605-3e9f993d23b9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1412091a-8b5f-4411-a5b5-d3238bb2faec)(content(Whitespace\" \
         \"))))(Tile((id \
         8d1412d8-79ba-4076-a1ba-f70a50416be9)(label(usernames))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         856569ae-a254-4721-bc63-92758577ed47)(content(Whitespace\" \
         \")))))((Secondary((id \
         c994a98c-b869-4393-8aaf-0f385f707373)(content(Whitespace\" \
         \"))))(Tile((id \
         84112c0d-a909-45f6-930f-a04e0ea0ba47)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28de9941-36e9-4e83-82c0-a4e4ed89286c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         18a76734-53ff-411a-8747-9619c826ede7)(label(mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb3d0118-9506-4fcc-98bf-34add0953e9b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3c6e4626-6ac3-47d1-ab16-c4a04574f176)(content(Whitespace\" \
         \"))))(Tile((id \
         fb246ce1-198e-4d72-9529-0c3831d9898e)(label(strip_at))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a17aa260-2c0e-4de8-8c6f-fb0a9afb550f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b6102b73-f51b-4a59-a225-2a7259c955e6)(content(Whitespace\"\\n\"))))(Tile((id \
         8d96e6a4-4e4c-4b00-a324-0f398034df3c)(label(usernames))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a0632ad0-74df-4dae-b258-8b41c9dd0e32)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         55476c33-7de4-4808-baf5-d0e0c9a511e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b719e05-3c84-40d6-a021-9e7b1d8a4459)(content(Whitespace\"\\n\"))))(Tile((id \
         09dae8b0-e006-48ec-bd47-ec13ea6d4dfc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         25f92d96-6369-41e8-b3c8-4df144d7465f)(content(Whitespace\"\\n\"))))(Tile((id \
         14112e51-85f2-4ce1-9352-600ffe070ab4)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3ca29452-28a7-4c14-8c5c-51f59e9636af)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         607f9dca-309c-4598-b0d6-c3ddaa4ff109)(label(\"\\\"Hey @luna the \
         moonblooms are opening\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e7ba481e-8e53-48a6-82f8-cca8bf33cca7)(content(Whitespace\"\\n\"))))(Tile((id \
         d72cc6c2-6af8-49e6-9ce4-6a34aadce1e2)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         389eee7a-d0ce-42bc-a0cf-1c83c45f8e7c)(content(Whitespace\" \
         \"))))(Tile((id 6a45821e-66a0-4045-8a60-acc38e2420cb)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         41e76d98-abb8-4d56-b7b0-0048fa675366)(label(\"\\\"luna\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         22ebc6ea-dfab-42d3-861e-7cd9dc9f22b8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         5e124904-8f04-4fa0-a385-63470a8fd54c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3785d3db-4038-4732-a96d-79fe07ac8786)(content(Whitespace\"\\n\"))))(Secondary((id \
         69d1a901-68b5-403c-a02c-883375f6066f)(content(Whitespace\"\\n\"))))(Tile((id \
         d018fe3d-4b88-4721-afa9-6abf69b11b96)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         44e0753c-34cb-4993-a86c-f70e172e97df)(content(Whitespace\"\\n\"))))(Tile((id \
         25962b08-536c-40af-a1ba-c9ba33e42f22)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dce7a3d6-e12f-4438-9bed-e0eb1e35ab5a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f0c17b53-3eb6-42e5-9f93-e551f6767965)(label(\"\\\"@thorn @moss check \
         the greenhouse\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6d27cb46-5997-44cb-a4f2-4c6dd57e4370)(content(Whitespace\"\\n\"))))(Tile((id \
         5c202ea6-75ac-490d-989e-5e6d3fc3c123)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d738522-3459-4b76-89cc-d736951eb207)(content(Whitespace\" \
         \"))))(Tile((id 1e163ecf-fabd-48e0-bc03-cf83052d3480)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4b23779e-f344-447a-906a-5191ba5d1efd)(label(\"\\\"thorn\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4a27c0d-9ef1-4183-aa5b-4980ef7019fb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2d233980-47c0-4ab1-998b-fb86b0f3a47e)(content(Whitespace\" \
         \"))))(Tile((id \
         e5457c3c-ec75-4dd6-9d3f-5659bfb12fa7)(label(\"\\\"moss\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         63b7c6be-2c9f-4559-8a73-261edfdb8c1c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b4bbdf12-9482-4adf-9f43-560c4db7379a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8f02f1b-7adf-407b-8b3a-7a02c60980dc)(content(Whitespace\"\\n\"))))(Secondary((id \
         f91310de-de2d-416b-badb-802c43849ab8)(content(Whitespace\"\\n\"))))(Tile((id \
         1d3fe91c-80ea-44b5-8ea9-57670df5c15a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7d87c039-6bae-4f45-a723-3feac5d64271)(content(Whitespace\"\\n\"))))(Tile((id \
         1f7a8c23-3462-49ca-aa42-83b20aa35f4f)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0b5977a0-7b8e-49c3-9bfb-eedd0278818e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bc2772dd-887c-47de-9cae-49938077e4fb)(label(\"\\\"the night air is \
         still\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b92f19ca-e400-42af-a3af-7c8b0abe9a62)(content(Whitespace\"\\n\"))))(Tile((id \
         22a2f8c7-438c-4ebd-89fe-6867d330e71d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d5851fee-db75-4a77-b17c-79085df59218)(content(Whitespace\" \
         \"))))(Tile((id \
         2015a9e8-3a18-4cb7-8ea5-632e20dba0ea)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         68bad0e8-7c4d-4a9e-878c-949673b4bc14)(content(Whitespace\"\\n\")))))))))(Tile((id \
         efd2e102-a6e8-4ac1-a2f6-928cb0ff413d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b6458bab-bdf4-4ac2-aca0-4f502c0e9112)(content(Whitespace\"\\n\"))))(Secondary((id \
         61da0b78-9666-4659-a105-3edfc5afcfec)(content(Whitespace\"\\n\"))))(Tile((id \
         814635a9-ce20-4819-bb3b-a1154d5c994c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3de76ee1-2bd8-413b-b9ea-c9b1e0be9283)(content(Whitespace\"\\n\"))))(Tile((id \
         f86561a2-2372-4da6-a55e-c9f4aa896fce)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         243d9480-7094-4394-8264-543e5c4234bd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         128d410b-4055-440e-acfb-66f722bb923d)(label(\"\\\"@fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7eda1c6e-8c6f-4670-ae99-fb2161583baa)(content(Whitespace\"\\n\"))))(Tile((id \
         db95d178-8fc3-4c90-9c31-12d079b79790)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         27e5b87a-1a48-422f-ad1f-ab3d04486a3d)(content(Whitespace\" \
         \"))))(Tile((id 129601c6-5d65-4b73-a146-d442b10db100)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e351c4f0-8c2d-4679-8ee5-3dc622aa4be4)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         52c8765d-e1fe-4342-b74a-78033f272f1c)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         db28d89f-11af-4f2c-ab5b-7ea6bf3bfa5d)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MENTION EXTRACTOR - SOLUTION #\n\n\
         # Check if a word starts with @ #\n\
         let starts_with_at = fun word ->\n\
         string_sub(word, 0, 1) == \"@\"\n\
         in\n\n\
         # Remove the @ prefix (take everything after index 0) #\n\
         let strip_at = fun word ->\n\
         string_sub(word, 1, string_length(word) - 1)\n\
         in\n\n\
         # Extract usernames: split -> filter -> map #\n\
         let extract_mentions = fun message ->\n\
         let words = string_split(\" \", message) in\n\
         let mentions = filter(words, starts_with_at) in\n\
         let usernames = map(mentions, strip_at) in\n\
         usernames\n\
         in\n\n\
         test\n\
         extract_mentions(\"Hey @luna the moonblooms are opening\")\n\
         == [\"luna\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@thorn @moss check the greenhouse\")\n\
         == [\"thorn\", \"moss\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"the night air is still\")\n\
         == []\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@fern\")\n\
         == [\"fern\"]\n\
         end\n";
      refractors = "()";
    } )
