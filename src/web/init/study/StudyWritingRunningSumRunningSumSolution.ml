let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-solution",
    {
      segment =
        "((Secondary((id \
         89620180-a34e-40c3-81b2-3aab64721267)(content(Comment\"# RUNNING SUM \
         - SOLUTION #\"))))(Secondary((id \
         8fbc171d-1476-4d0f-9d69-556d9216226b)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ae90bfc-3b6e-450e-bb6b-7b3d54246b36)(content(Whitespace\"\\n\"))))(Secondary((id \
         38fbc12e-ede7-4e12-a5b0-5525471de44f)(content(Comment\"# Uses \
         fold_left with a tuple accumulator:         #\"))))(Secondary((id \
         8295c608-d30a-469b-aa0b-9df04881c195)(content(Whitespace\"\\n\"))))(Secondary((id \
         658df084-0b87-4e9a-a812-43b6550d4a83)(content(Comment\"# \
         (running_total, result_list_so_far)              \
         #\"))))(Secondary((id \
         1111f922-113a-41e1-a00f-995ad3c15c90)(content(Whitespace\"\\n\"))))(Secondary((id \
         947d8f81-979f-454e-9df8-95530f839e4c)(content(Comment\"# On each \
         step, add current element to total,      #\"))))(Secondary((id \
         6e0cc7da-c728-4573-bc79-4f3d8689ceb0)(content(Whitespace\"\\n\"))))(Secondary((id \
         6abf45b5-47f8-47d8-ae81-f7fb948a7ae5)(content(Comment\"# append new \
         total to result list.                 #\"))))(Secondary((id \
         afd5f5db-cffd-403c-8db6-c430ae942ef7)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1e0b6a6-69f0-4555-8d3a-b20f16114a1a)(content(Whitespace\"\\n\"))))(Tile((id \
         5fba2c3d-c830-4715-9c98-2c34fba19e70)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         cd301e4c-87b7-4d15-9235-cd245923c416)(content(Whitespace\" \
         \"))))(Tile((id \
         3a063878-f519-4f0d-8045-7ad8ef5e7861)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         016effca-d2c6-4373-9a41-4128e01b167b)(content(Whitespace\" \
         \")))))((Secondary((id \
         5a146e5a-28bb-435d-a3aa-2945f6d7be04)(content(Whitespace\" \
         \"))))(Tile((id ff804663-f34c-481b-85db-6d28cc886127)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         8bbb56ef-af6b-4e63-8751-1fcc451f384a)(content(Whitespace\" \
         \"))))(Tile((id \
         c1266820-02f2-41c6-9ac8-d18e3aa5956c)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0a1302e7-b232-46b6-a2a8-359db9be01aa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         318cdf63-c475-4771-bca8-4eb622f3cf44)(content(Whitespace\"\\n\"))))(Tile((id \
         148b94d3-1446-42ce-8af0-f4fc96d1d984)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e15ea57c-d295-4c15-865c-70b78e01dca9)(content(Whitespace\" \
         \"))))(Tile((id \
         f1276b02-3e99-4ac5-8fe7-beefca65a578)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         e945ae98-392d-41ed-b455-255933cc3638)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         77b18901-3e5f-4ad7-bcaa-1b0791a5ac15)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         cf2aa002-ca7c-45f4-b1c4-04c992fc3ed3)(content(Whitespace\" \
         \"))))(Tile((id \
         93510eec-f65c-4654-a9a7-fbfe987bf872)(label(result))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1b31ceb2-9b45-4377-a56f-09496607c7a7)(content(Whitespace\" \
         \")))))((Secondary((id \
         06c9da25-220e-4172-8b59-24b1b69e804d)(content(Whitespace\" \
         \"))))(Tile((id \
         a1566834-3e90-4bc4-979d-60acbd38b425)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fa97d93b-65a6-4d04-939b-8afe12be8e97)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         72d1c941-3cdf-4654-aafb-735b2a5076fa)(label(nums))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bae76441-2a86-4ed2-a221-4652a03c5ce7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f49fb39e-659d-40e5-bd07-385a3da71df6)(content(Whitespace\"\\n\"))))(Tile((id \
         593a5289-b215-40e1-bf9e-ca3902e5c9dd)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         33bcf945-e56d-4572-b1de-78a912919270)(content(Whitespace\" \
         \"))))(Tile((id \
         bfb8c791-3351-4a1f-b8a0-220232401476)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4d7a29db-99aa-4520-93d0-70fdfc1e2025)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         8a2f5f8a-a978-4542-8ae7-2c7e2b0b7956)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         58678646-ba2a-4cbf-9397-fc75cbc41c62)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f4c4f30a-8a0a-4c1a-b633-6d2690e9cabf)(content(Whitespace\" \
         \"))))(Tile((id \
         71ac6502-9b50-4b6f-89d6-5a4ac9ee55ae)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         f8c73c61-d330-4a23-a7e3-7ee5c96024a7)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         2148993f-5f34-4047-a051-56729f338a9c)(content(Whitespace\" \
         \"))))(Tile((id \
         ab4e325b-7250-4566-9447-02fa6f12d8c5)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         e4c752d4-6677-4dbd-aa22-cd19b34264ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f32c8c8d-426b-43a7-b517-412ec1ccbbd5)(content(Whitespace\" \
         \"))))(Tile((id \
         c472b9c2-4d28-4c48-a66a-07d09b696fff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         23239f73-6fa8-48af-83a4-3ae5ddf7c0d5)(content(Whitespace\"\\n\"))))(Tile((id \
         c108772d-d64c-4fb5-bf72-a0eea8bb3b04)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         13309253-6696-420f-9830-addd28187dc1)(content(Whitespace\" \
         \"))))(Tile((id \
         f55c3230-96e4-4664-a866-c502c5e6d181)(label(new_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7087d629-7656-442f-bf66-3af18c78f4a9)(content(Whitespace\" \
         \")))))((Secondary((id \
         da63ccdf-1312-4823-bcb4-3694a10e99df)(content(Whitespace\" \
         \"))))(Tile((id \
         cfbb5076-5a11-424c-8619-5a9650f7e202)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         661db86a-17bd-4a9e-83ad-7243baf5cb78)(content(Whitespace\" \
         \"))))(Tile((id \
         8f764a0b-16f2-4f90-8233-25fec98905de)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc2aa92a-d763-4d5a-a97b-9fb485d5d86e)(content(Whitespace\" \
         \"))))(Tile((id \
         7331cfc5-e554-4afe-9ed8-c09c775e0735)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         115f1d57-c922-4343-a062-c9ba17a79f35)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         66bfba7b-e6e8-4174-b5bb-5dbcbf5cf2ed)(content(Whitespace\"\\n\"))))(Tile((id \
         36c57363-22fe-4bd1-be02-cda16f45fc63)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ff4b68b5-20c2-4293-80d3-9e1ef513841c)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bb0a3eb0-7245-4f7a-bb4c-43fa7c9130fe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ff0bf87-9fd2-44e4-9f9f-4c938f4ed4a9)(content(Whitespace\" \
         \"))))(Tile((id \
         43af1e4d-2fc4-48f1-a593-580ef2c59eb2)(label(append))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d21a3df-7180-4e42-83e8-3310cb30d17b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         65f8bb7a-db2b-45d5-8846-2c801f61a5f1)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         02c0776a-9481-4d70-adbb-7db608e0d264)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7183edf2-d4ac-4bf0-80e7-7b39d308d0b7)(content(Whitespace\" \
         \"))))(Tile((id 01c620eb-450d-4b22-8684-31747dc4c388)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c7bbbe3-5714-42a3-b90c-8e082810c091)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
         a95d3ed1-c977-44cd-8719-085606809500)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e3b5be8-4612-4548-ae64-368648cce1d6)(content(Whitespace\"\\n\"))))(Tile((id \
         e8e5bb48-bf6f-4069-8603-dd36474c3c7b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         620a26ee-6e2b-4052-b36c-9f89b018fe94)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0176e03a-c71e-462f-89fa-111ca07a5f3e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2074f927-e926-424c-8d4a-ad5a2006ef9a)(content(Whitespace\" \
         \"))))(Tile((id \
         de809d4a-d701-4e12-b278-482a301622eb)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7a0b92df-c375-4071-bab6-596361c6ba7d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         70e38b89-3e9f-4dde-a5e1-79f469113152)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a381272-04ba-4bcc-a07d-47522080c7dc)(content(Whitespace\"\\n\"))))(Tile((id \
         e8c73421-8dcf-46e4-9ba2-b0136ee7bffa)(label(result))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         cd72aef8-3780-471d-9b4b-1a7159ffd5d7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cd60e470-184a-4889-84b3-3b094b3ec24b)(content(Whitespace\"\\n\"))))(Secondary((id \
         69a829fc-f9ff-46f5-9741-c643b70a4e66)(content(Whitespace\"\\n\"))))(Tile((id \
         20e1155e-78ef-43d5-9ee7-2a369b68b0c8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d9510a12-8bb3-474b-84a4-b400c22e02d2)(content(Whitespace\"\\n\"))))(Tile((id \
         28f3aec7-2f78-4fb8-b40e-dd70419a20f7)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3e7f9b69-17ca-4a6f-8aa5-847019c09ab1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a75ecdd8-8dc7-473e-8cec-cafc9397dd67)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3c879ccb-1d02-45d0-91f6-4290d21a48ac)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b36fe54-3423-4d7f-bc76-dab5ef4ca772)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f4efb99c-2448-48bf-8f13-5aebfdca908c)(content(Whitespace\" \
         \"))))(Tile((id \
         e392f34f-804c-423e-9eda-bc24f0079bdc)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07e0b33f-e69e-461a-9640-ea9e89bd3271)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         549421d6-48d3-49b9-b6b4-4a5645988b7d)(content(Whitespace\" \
         \"))))(Tile((id \
         22f829dc-c970-4d05-8692-60add0322197)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         712743d9-7c15-4b73-94f5-7220b615718a)(content(Whitespace\"\\n\"))))(Tile((id \
         adc044de-4c89-4b87-af9f-8c03069fd512)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad4ebcd2-315e-4d81-be21-3a1f17c3992a)(content(Whitespace\" \
         \"))))(Tile((id 5dc43158-1e67-40ab-a50e-70f8452b73a9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1bea3e56-0e11-4045-9ac1-fa96d9b32667)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8467e445-5829-465a-8476-e0f320081b4f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4bc8199-6551-46e7-a440-f584466c916d)(content(Whitespace\" \
         \"))))(Tile((id \
         0a54d633-041d-4d89-8e8c-3083a3613e5f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d6e9a50-0c32-434d-91d8-7b841f687ad6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb29384f-5ce8-492c-a772-1aa34ef5f06d)(content(Whitespace\" \
         \"))))(Tile((id \
         d36c9509-a846-49a8-a0d0-4db30b35bc64)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8e6dfc59-3d21-40e2-86eb-a2245c3329ac)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f1646d6e-8b93-43c5-b5f2-31d1e9b0ba4a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb7ee37c-43ca-4229-a8db-1d20d15c4cb9)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab980711-8928-4f4e-8ca8-488d4e14ba00)(content(Whitespace\"\\n\"))))(Tile((id \
         de5b6628-a07d-4035-9294-1bff6a0c3a48)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         02323121-bb3d-487b-bf4e-10fd585288db)(content(Whitespace\"\\n\"))))(Tile((id \
         9a714500-426f-484d-91ce-61d6e93c29ae)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         17884c01-c01b-4a60-8345-7f79aedbce70)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         044cb79f-4655-4daa-91c4-facc2c1115b4)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         533d80e8-e006-4083-874b-4f129c2b2d0f)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         aad9e32c-cfe3-416f-a91c-467603c52da0)(content(Whitespace\"\\n\"))))(Tile((id \
         0ce96134-50c8-4bcb-896b-68a65e6db27e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         265000d1-db24-449a-ae4e-975a4bdf3725)(content(Whitespace\" \
         \"))))(Tile((id 24b6003e-6215-45c5-9f3a-be88674ee3d1)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6ee67492-35c9-4b26-b48a-bc436c4bc90c)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a68282bd-9f9e-4e62-9b19-361d39cea418)(content(Whitespace\"\\n\")))))))))(Tile((id \
         ffb09ea1-c55b-4fff-9759-f9ca7d6ea358)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f288f6b3-8ad1-47fe-b060-18e77a1d2a0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2c4c135-7ab2-44d3-a1f0-86ae89bedf1d)(content(Whitespace\"\\n\"))))(Tile((id \
         66d415ef-4188-4da0-bfc4-f2ea17514fa0)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3738d9a6-6f06-4cb9-bd5c-ddcbfb3d86c4)(content(Whitespace\"\\n\"))))(Tile((id \
         436f593b-94fb-4233-a110-28c308abd7d9)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         488e4487-8b30-4277-b541-f0989fb4863a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1fcf8da4-24c5-4cde-92d8-e539731537ec)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e94c3b8c-9016-48ec-b524-652545f22c2f)(content(Whitespace\"\\n\"))))(Tile((id \
         70cfb4a0-87e4-4297-b574-aebd314e77f0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         91e36de0-e9e8-4376-992b-643155a38af4)(content(Whitespace\" \
         \"))))(Tile((id \
         d9d07d64-01bd-4769-906d-31cbced7aba9)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         38792163-c7cc-43bb-9d51-14f8fb9ee30c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6efe7c5a-5d91-48d9-b08a-6bd028a7cf46)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b055215-6961-405c-b96f-596bbbcca4f5)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9f5c203-58a6-48c1-b135-2c9076acdc0d)(content(Whitespace\"\\n\"))))(Tile((id \
         5d9259a0-8567-4e91-9c51-4e5c4016d99b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1841db38-138a-473a-8897-5a8771efce1a)(content(Whitespace\"\\n\"))))(Tile((id \
         154c71e2-a669-4b8e-bc26-2f9361d49423)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b68af3e-b1f7-4cbd-bb07-b396fa18a7be)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b0d404fb-7b98-4933-a4aa-e150ee9d64ad)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         33f13744-bba3-49bd-9e2e-ad37208b4e82)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4261b61-0245-4eae-ac8c-637a659663a4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5728a558-1b13-410d-b5d2-068d490475fd)(content(Whitespace\" \
         \"))))(Tile((id \
         9390a829-6c3d-42ea-ba37-4c71cde530fd)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         794aafbc-0b78-4e0c-a009-375c8b5f34b0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         33b66bbe-ba2f-4b5c-be70-42da6291b051)(content(Whitespace\" \
         \"))))(Tile((id \
         524cb92e-19c3-45ab-ad13-66250a468996)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54e8116e-60d2-4ec9-bd86-bec37cc8727e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3cac5ce5-4ac0-4c37-bcc5-3f11071ba3de)(content(Whitespace\" \
         \"))))(Tile((id \
         f1731a34-61ec-4386-93e8-3eae08361e87)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         4b06b253-49df-4f4c-99f7-0caa65e1fd3f)(content(Whitespace\"\\n\"))))(Tile((id \
         9d3fdf66-0c25-42ad-8aa2-3daaf45fe722)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         84aeef16-ab52-443a-8cd2-19120c2fcb67)(content(Whitespace\" \
         \"))))(Tile((id 14405bbe-bd12-4dee-ac54-53a986604e9e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         171937ce-fd48-417c-bdd6-366b29c3192e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         97df141b-41d7-43a8-a458-32159df98952)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c33d0960-ebfd-4cb7-8718-f97e44fc0062)(content(Whitespace\" \
         \"))))(Tile((id \
         8ddaf5c4-51aa-45e8-b89d-09f4447748b2)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         56c0c832-e416-40ad-aa92-b931a3383a1a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3cbf4b59-dfd6-4d14-884d-13ff63bbbf32)(content(Whitespace\" \
         \"))))(Tile((id \
         5c5dde6f-63de-4195-a2bc-c3cbfe6095e1)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b5e56b75-0dd5-48a7-a462-7694bf2da9fd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9bdbed6b-1d5f-4789-a0cc-b63aa14ca22f)(content(Whitespace\" \
         \"))))(Tile((id \
         cda3799e-1840-47c1-9271-1350cd6ed32b)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         00adc30e-3327-4a94-bcee-9737f279c0a3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c7d37758-a5f4-4ce5-a29c-d946b0559122)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# RUNNING SUM - SOLUTION #\n\n\
         # Uses fold_left with a tuple accumulator:         #\n\
         # (running_total, result_list_so_far)              #\n\
         # On each step, add current element to total,      #\n\
         # append new total to result list.                 #\n\n\
         let running_sum = fun nums ->\n\
         let (_, result) = fold_left(nums,\n\
         fun ((total, acc), x) -> (\n\
         let new_total = total + x in\n\
         (new_total, append(acc, [new_total]))),\n\
         (0, [])\n\
         ) in\n\
         result\n\
         in\n\n\
         test\n\
         running_sum([1, 2, 3])\n\
         == [1, 3, 6]\n\
         end;\n\n\
         test\n\
         running_sum([5])\n\
         == [5]\n\
         end;\n\n\
         test\n\
         running_sum([])\n\
         == []\n\
         end;\n\n\
         test\n\
         running_sum([1, 1, 1, 1])\n\
         == [1, 2, 3, 4]\n\
         end\n";
      refractors = "()";
    } )
