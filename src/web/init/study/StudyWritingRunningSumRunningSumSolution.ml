let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-solution",
    {
      segment =
        "((Secondary((id \
         769d3765-45b1-403e-bee7-722ba3ade1d2)(content(Comment\"# RUNNING SUM \
         - SOLUTION #\"))))(Secondary((id \
         2e56b65b-afcb-4531-b881-6a0ad70387f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3543124-a75b-42bc-bbc5-269e3283c6e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed6f8e85-0b0d-43aa-8ddc-8c1e3c877671)(content(Comment\"# Uses \
         fold_left with a tuple accumulator:         #\"))))(Secondary((id \
         c5ccdf66-52a0-4968-ba8c-33e42f6a3a24)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1c2762f-3fbf-4de6-bd94-b143a7c595b8)(content(Comment\"# \
         (running_total, result_list_so_far)              \
         #\"))))(Secondary((id \
         ca7ad0c2-f97f-453e-9655-e02ea66f6f29)(content(Whitespace\"\\n\"))))(Secondary((id \
         c01fc2da-e90f-48a2-8d1c-2f0377e72bd1)(content(Comment\"# On each \
         step, add current element to total,      #\"))))(Secondary((id \
         3393274a-61a9-40b5-b063-3261f32c078c)(content(Whitespace\"\\n\"))))(Secondary((id \
         32da4f8e-2a98-4079-98f1-49838e80b77d)(content(Comment\"# append new \
         total to result list.                 #\"))))(Secondary((id \
         412f6ba4-5902-49fe-91bb-e2fec7242d70)(content(Whitespace\"\\n\"))))(Secondary((id \
         2fdb4463-5523-4f55-8917-8c4470e3eaaa)(content(Whitespace\"\\n\"))))(Tile((id \
         ab8abcea-f002-419f-b73a-928b6e425cac)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         767ab9a7-5576-44c5-a314-6c395f76f6aa)(content(Whitespace\" \
         \"))))(Tile((id \
         172dc67a-a129-4db5-9c6a-2315f2440cf0)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a3aa96d5-4f3e-4d30-ba12-2082a3c024fb)(content(Whitespace\" \
         \")))))((Secondary((id \
         86f8cdb1-d3ff-45ad-8c36-2cd3a6544014)(content(Whitespace\" \
         \"))))(Tile((id bafedc03-0f85-455a-94bf-05be6ccc8877)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         2cc2a3dd-0467-4cf4-8f03-d149034925fd)(content(Whitespace\" \
         \"))))(Tile((id \
         b8c364dc-1b31-4a8b-abbc-4b45cf0478c2)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         fd1f28fc-3547-40d0-a938-207544003ef1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dc183fb8-bbb4-44db-baac-86c08dae750f)(content(Whitespace\"\\n\"))))(Tile((id \
         cb3aefbb-30d2-42c0-ba70-2485562aeef6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         15821112-839f-4178-b427-67a54c483e7e)(content(Whitespace\" \
         \"))))(Tile((id \
         8c1fb276-6dd6-4d85-8c4f-69beb71c9776)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         f6081d6b-53dd-4ddb-ae4c-37a6f4acffdc)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a02aff5b-f40e-4ed9-b752-e656044c001c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c1add54e-44bc-43a5-99e7-925bb0f462e6)(content(Whitespace\" \
         \"))))(Tile((id \
         fe0212c4-1790-4766-b490-2430d0421f1a)(label(result))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         2497866a-ebe6-4b88-821b-804a56fa2d1b)(content(Whitespace\" \
         \")))))((Secondary((id \
         9105e3a6-6135-4cad-9fce-7b7b8ac20c1f)(content(Whitespace\" \
         \"))))(Tile((id \
         940dc81b-a883-4dbb-873c-3c0abe501d5f)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b3719df-e162-4764-a688-af31e48af550)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4947454a-e799-4733-9db8-c69899c9ba30)(label(nums))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         94181e3b-35f9-4c31-b75e-60efe33110d2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1e877191-76f5-4ccb-ba98-db7defbe12cf)(content(Whitespace\"\\n\"))))(Tile((id \
         df52f4b4-8fca-4ebc-b2d0-753973f6f591)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5143ead7-8c7a-42ba-a5af-e2f3eb218ebd)(content(Whitespace\" \
         \"))))(Tile((id \
         4fb628be-5738-4210-9bfc-d5a1031aa2d1)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         cf5b5337-7b5b-4e73-83ee-3b9d913c6349)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4f77333f-8a7d-46dc-863e-3b1491b861cf)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         32299be0-c383-4840-81b0-c0563e4b6ca2)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         d1468619-ab6a-41ad-8a90-be0432405a09)(content(Whitespace\" \
         \"))))(Tile((id \
         38d61011-fddc-4bfe-8902-e281aabf1555)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         3c42d2af-db7b-4b19-af76-a6293856bbbc)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         c59eb0f8-f3cc-4caa-b062-0a3336b85b3f)(content(Whitespace\" \
         \"))))(Tile((id \
         faa0d695-a9e2-4bfa-bb5e-15330c5cf823)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         41cf0a14-62a8-4a1d-9e47-81bee5113f40)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4cd2abe6-6d75-4e6c-8cbc-eedab02383c5)(content(Whitespace\"\\n\"))))(Tile((id \
         17afa536-15a3-4e89-8e9f-a16c92674d4a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         902520b7-69de-401b-acfc-d9e68a5feb12)(content(Whitespace\" \
         \"))))(Tile((id \
         6f2ec0b8-8c78-46e4-aebd-87082d517eba)(label(new_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         36286ab2-5aee-4b2a-b3c5-193a38f39699)(content(Whitespace\" \
         \")))))((Secondary((id \
         a22ba6d8-c206-45e0-91d4-b5e156fb48e8)(content(Whitespace\" \
         \"))))(Tile((id \
         c2930848-0904-4363-9f1f-e01777e83161)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dca65009-6a9b-416b-a680-72f04499742e)(content(Whitespace\" \
         \"))))(Tile((id \
         59959f73-d309-43f1-bd85-8caf613a8ee2)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         668ef12c-da3a-4cd0-b228-d7efeafa1f22)(content(Whitespace\" \
         \"))))(Tile((id \
         e482eb95-b819-4ed7-8252-ffc38663711c)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e0cd7b6d-266d-4d33-a6ad-90462b3e42e2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         99ca34a1-220e-47e4-9af6-b976ccc3ac14)(content(Whitespace\"\\n\"))))(Tile((id \
         923abbb2-21e4-4dde-9ebd-ea1800a3101e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         18397990-92ca-4924-8486-4cb8090cb8a8)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6950245-3f61-464a-b1f8-ce5789f6238b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         edacbf20-be4a-4e0f-aa65-8e5632edc63a)(content(Whitespace\" \
         \"))))(Tile((id \
         07130e17-e100-4807-9e68-6871c75e25c8)(label(append))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d77ccc0a-4653-4acf-afa2-1b76a626b130)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5cd69053-9b9d-44f5-9201-77164ceaf16a)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4550ee8d-d0a9-43ea-8a8b-23130227a36f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         24f77c51-d09b-4cb5-aff7-d70c10239f7a)(content(Whitespace\" \
         \"))))(Tile((id ee3dc4ca-8780-42bb-a2e5-65f8a007f081)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ee4a04a1-ebe7-4f21-a876-13d425d124a6)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))))))(Tile((id \
         5753a424-f10e-43e3-8c57-fd76dd2fa212)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d47f2f31-a8d3-4903-9218-7724cc0211b7)(content(Whitespace\"\\n\"))))(Tile((id \
         a5123ec2-b190-4c91-8870-83a380b2c559)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         89592f8a-aee2-4034-b774-8b9bb3667cdc)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         090a1998-097e-4169-a4e2-af589615619c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e83a11d1-bd9d-46e1-94b4-b4e8a6618ac0)(content(Whitespace\" \
         \"))))(Tile((id \
         571b8272-d852-432b-b9e1-cab1f5fd3bb3)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2524c7a7-5cc1-4af3-b920-4fda3f0e445b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c383168d-3d3b-4d43-83c2-cc3d3005314f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4026db5a-d5bd-4e88-83c3-ee749d435abf)(content(Whitespace\"\\n\"))))(Tile((id \
         2ffbc2a3-9ea7-4b24-ae32-dc86fd31846d)(label(result))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         10882e61-7317-4bad-876e-c5e41834fdb3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a18893b8-a61e-4930-b7c4-798225bc22a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         5267b349-6b47-4d9f-af6a-0e98e7a956e5)(content(Whitespace\"\\n\"))))(Tile((id \
         71d7e739-4d98-41bd-8eb4-891bb2d0c634)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         90763713-83c6-485f-96b8-6af411e62e31)(content(Whitespace\"\\n\"))))(Tile((id \
         85d8cbc4-77cb-4a20-bf6a-ef0ada3724ad)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8fd41f3f-ed81-480b-a352-a3e7b5eee5f3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         372bf8b3-1163-4f65-89cc-7c508cf30687)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         81613575-cb49-481d-9e7e-cd936945cac7)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c40a6091-dcea-4fb9-a6eb-33a0062ffe66)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d62f7fd-971b-463f-86ed-abc59e5873f4)(content(Whitespace\" \
         \"))))(Tile((id \
         fdc2e873-9fc3-49b9-9136-3a02c183d33d)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fd821a6d-6b28-4f20-833a-120545ccfce6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         feefc349-0b87-4564-88be-3644071c1672)(content(Whitespace\" \
         \"))))(Tile((id \
         055745e2-04b2-4d42-a647-989fdd434393)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         f28f7053-c758-41bc-9925-7ee14d0a2194)(content(Whitespace\"\\n\"))))(Tile((id \
         2dc845cc-fe7b-40fb-9a2e-25711d4355d8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4c65cfe-1072-4488-aa76-1cfd0038ce90)(content(Whitespace\" \
         \"))))(Tile((id 5b1c46f8-2ae9-4951-9104-38f92d8b008e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         520c2f9a-0e6e-4d4f-a104-5cc07c2b5846)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5bf500fe-ea38-4f17-9799-8c3ad2f8e66c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8627280f-607d-4560-8238-bc0f952d473c)(content(Whitespace\" \
         \"))))(Tile((id \
         7d2c342a-bb9e-44f8-9e6f-840670b90bd8)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c2d5970a-50a5-4088-8795-b3cc37896d90)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19873d35-d9fa-4627-a354-63bf3cd29582)(content(Whitespace\" \
         \"))))(Tile((id \
         7f782813-35f1-445f-a71b-c2f7113c1714)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         591c123e-d025-4ee4-877c-133d6254aa25)(content(Whitespace\"\\n\")))))))))(Tile((id \
         eab63099-a0e8-49e8-8170-03ddee1056d0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3229dfd5-20ac-443d-8efa-7ab2fabb73e9)(content(Whitespace\"\\n\"))))(Secondary((id \
         174031bd-c8f7-4b1e-9c11-a316c6b2d2e1)(content(Whitespace\"\\n\"))))(Tile((id \
         dfe63f51-dd88-4d84-a974-df4e90f6d0ef)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ce9045a8-02b1-4a0b-adc6-53893f4bfc64)(content(Whitespace\"\\n\"))))(Tile((id \
         d10690b1-09e7-41bf-9ec7-bbc4dd5c0aa2)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad90f5eb-caec-483d-8607-6d6a07f259f3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d2879ef3-c092-492f-914c-8efd83cd22b8)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9e2a64b1-0ba6-4a09-9b83-ed03cbceb8b8)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         552ae50a-92a1-41c1-9b15-6cb6322fd6de)(content(Whitespace\"\\n\"))))(Tile((id \
         01495f5a-055d-46dd-8519-7a2a38a2dc70)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb2fb9f5-1067-436f-b3ef-619a8695ffff)(content(Whitespace\" \
         \"))))(Tile((id b34a9a14-8d46-4e5c-af7d-5abda1f58b15)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         797652f5-4be7-45cf-8e69-a03a86b8e677)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9642eb32-2601-4e6a-bd8e-5ac55a7110d6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         8ce25f65-4681-447a-b4ec-6cbe2d2353eb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a7f4e6a1-b8ca-4781-a11b-9580c6e3a4db)(content(Whitespace\"\\n\"))))(Secondary((id \
         0d5bc8ce-b943-4b4e-b5c2-201235e576f3)(content(Whitespace\"\\n\"))))(Tile((id \
         37ed6122-ae93-4704-9e22-5eae46f38d4a)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a399af20-63e7-4b2c-98b3-558a6a29b796)(content(Whitespace\"\\n\"))))(Tile((id \
         650d6f1d-7b32-48e7-bd60-b0f0cdb14cea)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cdd13dd5-24ab-4c4e-8fe5-3766adec4d8e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9eacb80d-31ef-4a92-abed-8e6d8afebd9e)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d0e57ed3-bef1-4c3c-83f2-6f6ea5fcca0d)(content(Whitespace\"\\n\"))))(Tile((id \
         7abf3146-775f-4a01-8a09-aa3ad7857c2c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e37952ce-9404-4f2a-9775-9a9586022228)(content(Whitespace\" \
         \"))))(Tile((id \
         90e0a658-38f1-4eb6-bbc1-9a54b8e38d6f)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1e6a1760-876b-443a-b4db-bbca71e9b5d0)(content(Whitespace\"\\n\")))))))))(Tile((id \
         3586ca66-370e-48c4-93a5-05316d4a9f5b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8ddc5535-1997-4fba-af4f-3d23ea94d2c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         88f903b2-0f54-43af-a33f-c495073f3e9d)(content(Whitespace\"\\n\"))))(Tile((id \
         60ac0299-9684-4db5-8792-6c21282e000f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0bbc3b05-10f4-4b7d-9de4-7cd435e3d34f)(content(Whitespace\"\\n\"))))(Tile((id \
         cf990dca-1e5b-4993-bf7c-7123f4c0514d)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f2b47cbb-f2bb-4b82-9f7c-ef889471a092)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3e08ab44-c3d8-4756-a603-eb8c770eec4d)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         32dd24cf-c505-420c-934e-40f80834f650)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c15f957b-e38b-4834-9a48-b3fb0b66e585)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         51cc9c14-bfda-49cd-9a97-f687cf34f378)(content(Whitespace\" \
         \"))))(Tile((id \
         f60e1dee-e51f-4841-987d-fe64da2728e9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82953d97-03b4-4f1a-9ce5-01a43f305a14)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f196ab54-7bce-4233-94e2-24de503bec85)(content(Whitespace\" \
         \"))))(Tile((id \
         e9e1f25e-199f-4ddf-87cb-b0939cc7206a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0dd3c18d-e302-410d-b184-b66d69b98e38)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         15e9138a-8ebe-47bc-9575-03c4111c5830)(content(Whitespace\" \
         \"))))(Tile((id \
         b355e993-cd43-4234-946e-63bdbe1ca5d3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         dfcc65c0-d910-4d09-b306-78be579f7bfe)(content(Whitespace\"\\n\"))))(Tile((id \
         9a1e8349-1b83-4a0e-9eb2-a8563c760e64)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4e92c44b-d8df-4140-ba6a-a99bc0cd0130)(content(Whitespace\" \
         \"))))(Tile((id b23ffd6b-0e94-4c97-951c-702da8ac39b5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         76a97d70-caf9-4f75-891d-cc5613b35429)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6d37b634-b232-45eb-8bf0-db1b65c08846)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f82d3a4b-f0af-4b60-b785-4fa42e23c62d)(content(Whitespace\" \
         \"))))(Tile((id \
         b8fab115-b2b3-463a-aacb-099052f34420)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1e0213fa-16bc-4798-8df7-443dbceba33c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         651c9661-0268-4126-a3b8-b53b6bc585f0)(content(Whitespace\" \
         \"))))(Tile((id \
         50cdb550-99c2-4537-a382-0b84e06e3a5a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         947ccadb-95c6-4c69-aed6-6b166de39847)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7a5834a4-92c3-4c3b-b09e-4dafd586d0ea)(content(Whitespace\" \
         \"))))(Tile((id \
         8f9405ee-0666-4632-8260-9a59933842a5)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         641a2542-de63-49c3-bcde-a0e4a85d333a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         890130d1-ae46-4cd6-8159-c96e245ac024)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# RUNNING SUM - SOLUTION #\n\n\
         # Uses fold_left with a tuple accumulator:         #\n\
         # (running_total, result_list_so_far)              #\n\
         # On each step, add current element to total,      #\n\
         # append new total to result list.                 #\n\n\
         let running_sum = fun nums ->\n\
         let (_, result) = fold_left(nums,\n\
         fun ((total, acc), x) ->\n\
         let new_total = total + x in\n\
         (new_total, append(acc, [new_total])),\n\
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
