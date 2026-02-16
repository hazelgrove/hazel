let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / last-element / last-element-solution",
    {
      segment =
        "((Secondary((id \
         06b7f650-e3f2-43f2-a443-eb1ff787fc84)(content(Comment\"# LAST ELEMENT \
         - SOLUTION #\"))))(Secondary((id \
         d7e41dc5-4fdd-4a92-b270-6163bec8d016)(content(Whitespace\"\\n\"))))(Secondary((id \
         a6434cbe-74a2-4736-bcbd-94dc5ef550d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         d249df2d-98d4-4113-9781-925d8456ca70)(content(Comment\"# Each step of \
         the fold replaces the accumulator   #\"))))(Secondary((id \
         a6716d19-e764-46b4-a58a-890ddafe332f)(content(Whitespace\"\\n\"))))(Secondary((id \
         a03cdb60-d34c-4569-8352-d48511431908)(content(Comment\"# with the \
         current element. The final result is    #\"))))(Secondary((id \
         7162a709-995c-4c87-863f-b27a1f8513fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         7caff3d8-2e56-4490-b8e2-9e366adc8389)(content(Comment\"# the last \
         element seen. For empty list, returns   #\"))))(Secondary((id \
         dc856fbf-cbd5-4ea5-bdba-9b2e3266cf79)(content(Whitespace\"\\n\"))))(Secondary((id \
         bcb1eb52-e66b-4daa-b3b7-39b2c6dfe757)(content(Comment\"# the initial \
         value (default).                     #\"))))(Secondary((id \
         71ae866a-e9ed-45e8-855d-7ae709b45fcb)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e94db0a-2755-4ad7-bf59-43ebddc35c4f)(content(Whitespace\"\\n\"))))(Tile((id \
         23c97672-d4ea-4a36-a139-20b145df935f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b6bffa18-001c-404d-a32d-4eb469676236)(content(Whitespace\" \
         \"))))(Tile((id \
         8312d4e0-6be7-404f-8679-7f9446494aca)(label(last))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         720498d6-8e4a-49fe-8c0c-3414b810172a)(content(Whitespace\" \
         \")))))((Secondary((id \
         444d42b3-8d91-4478-9efa-ed005acad199)(content(Whitespace\" \
         \"))))(Tile((id 2eaf0706-908b-4565-9846-09c3534a419c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         114d3ea2-cd7b-4f02-a3fe-29278198c795)(content(Whitespace\" \
         \"))))(Tile((id \
         0ca18c3f-ff46-4f02-867b-2c0105839c4e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         2d2503df-5f88-4062-844e-670155da4d72)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         70832e86-76f5-4c21-a644-714f7413da05)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         9ae9c742-caba-42f1-8caa-f4b0f4942802)(content(Whitespace\" \
         \"))))(Tile((id \
         9e8f90f7-453e-4e6f-a5db-6029d7fdc686)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         3d4808db-072f-4f3f-b480-9a5503429598)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b0376016-0565-46d0-9bd4-79744c2b8cf6)(content(Whitespace\"\\n\"))))(Tile((id \
         868b8c3a-1a1d-45ca-a86b-10e6f4a6c830)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         43784888-0a88-4017-aaf5-a28f42fcb87c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3cbdb7a0-4d5b-4477-8ae5-370bd66f6472)(label(xs))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b43962b-fdd4-4914-8120-dc214f63dbaa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         672862a3-d8b9-4cac-ab91-8858f7c17c5f)(content(Whitespace\" \
         \"))))(Tile((id 6f2a5d09-9d2a-43dc-8dfd-fa0e19c61426)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ddd22272-b830-4f1d-a63a-c569e6677969)(content(Whitespace\" \
         \"))))(Tile((id \
         20cfa4dd-930c-4b34-8040-aa9811f35a73)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a98369aa-1385-49c4-93fc-ea0891d187c8)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9510b5bd-af7c-4a95-a2a2-e563915e5441)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0b2eb225-5fed-461f-a344-e916fd22718a)(content(Whitespace\" \
         \"))))(Tile((id \
         24485624-056b-49cc-94cc-04faf18b8d0d)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         cac92406-6d81-4667-95f6-7c643ce4b103)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ae650c1f-a96e-4c1c-b7a5-5e91d34f1c32)(content(Whitespace\" \
         \"))))(Tile((id \
         b0f2adf2-c013-4847-b5cf-8a2ceffeb5ad)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1c78a953-03b7-4fe0-b1f6-39291336d5ea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3ace6fdd-662e-41d2-be3c-551ab4b1f1b3)(content(Whitespace\" \
         \"))))(Tile((id \
         50e2e6b2-d7b6-4993-8bf2-e5ea605a3cac)(label(default))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bf23861b-809c-4fcd-a002-ed60b81ecdb6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c00eddac-f69c-422c-a529-7547fead162d)(content(Whitespace\"\\n\"))))(Secondary((id \
         66c1961b-1756-405c-9470-efc5ea609a5d)(content(Whitespace\"\\n\"))))(Tile((id \
         5cbac74c-895f-49a2-9106-ffb56717f177)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         698cce64-f559-4e21-a68c-677d37942062)(content(Whitespace\"\\n\"))))(Tile((id \
         8f226694-a5ea-4e57-b47a-0cba9750d0b9)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         87a75fc6-f9fd-4fe4-9430-3c6d2249b432)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b32a88c5-63e1-47ad-a176-64343b7b7948)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         573da290-155f-465d-ae93-afa7147e7a66)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         907ae62a-c07d-4963-bafd-ae8af22e2ce7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f18bc401-92f4-48ab-b555-0efe07f4377b)(content(Whitespace\" \
         \"))))(Tile((id \
         25d40198-5c15-45be-9018-140cf9c826b0)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b9cfef8-9f4c-4978-b8d4-43faca0117e7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a732d104-ab4c-4e20-a997-3e801a3a5085)(content(Whitespace\" \
         \"))))(Tile((id \
         97e39435-f74e-4c50-a8d3-381c84a2e2a2)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ed264435-78da-4905-bded-fbcbb3f664cc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77e7b2ad-d7d0-4889-a256-873dacff4c8a)(content(Whitespace\" \
         \"))))(Tile((id \
         6e96cf1e-dc21-4bec-b42a-2f82f6778a25)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2a49e232-06e9-44c1-afba-e401201396b3)(content(Whitespace\"\\n\"))))(Tile((id \
         18d6ff74-ea8d-4e21-a9c1-cf8a53f9226d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f9072637-1c25-485f-b01f-a8d5698c9952)(content(Whitespace\" \
         \"))))(Tile((id \
         c52c3ff5-d30b-4762-bc80-474a68494e83)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1650ab4d-c651-4dcc-84ef-4d31a0c99774)(content(Whitespace\"\\n\")))))))))(Tile((id \
         6866d9c6-5450-4a88-a7b5-eab1adfd3edd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1a7ddc3a-9f12-4042-98fc-630164f2eaca)(content(Whitespace\"\\n\"))))(Secondary((id \
         888e9ae5-58fd-49aa-bb68-91a9e4342561)(content(Whitespace\"\\n\"))))(Tile((id \
         33092866-db68-424e-a3ac-3bb28e256fb1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2747fbc6-bfbd-48c9-a8cc-3caadc37bef0)(content(Whitespace\"\\n\"))))(Tile((id \
         41587372-bcc6-4445-80d5-3b65e4215f10)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d7bb966d-23fc-40b7-9439-3b844760130d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d153d882-88e3-4df0-8051-6b94cb6d8d15)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6a87910f-f751-4291-b0fd-eeb6963c0e13)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1da31f15-d24e-4b6a-a7e8-df9ec16204b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         20c5d3b5-2fdd-4e79-b47b-bca931248015)(content(Whitespace\" \
         \"))))(Tile((id \
         75145f8e-5b50-4251-a961-428b5b9f3f6f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d38910ca-778a-4824-b1aa-f87e2a6990fe)(content(Whitespace\"\\n\"))))(Tile((id \
         212d9af3-2845-4b7f-a08c-f0b13b186ddd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bdd90704-6128-46f2-b7e4-a471d667f6da)(content(Whitespace\" \
         \"))))(Tile((id \
         edb852fc-520c-4b8a-9ae5-c2b20fc71f75)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e198042f-dc87-4b4f-8503-a0425e6fd7ee)(content(Whitespace\"\\n\")))))))))(Tile((id \
         fc5964a6-17bd-4841-a7cf-e1192dd8e331)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe4fe383-1bfd-4a59-bf10-66fdf6dc814c)(content(Whitespace\"\\n\"))))(Secondary((id \
         168162f3-a1cb-4108-a97f-3ea66a28facf)(content(Whitespace\"\\n\"))))(Tile((id \
         87633c05-4081-4e11-8ec7-3a53186e8f23)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         832e931d-a6fb-4309-8aba-67f2eb6541e3)(content(Whitespace\"\\n\"))))(Tile((id \
         70ed379d-22af-4097-bd01-86dd808b2310)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b8fa91b4-c287-4097-bbc1-0e77979ec9ad)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         60bd3a32-c311-409f-8b42-34290223af51)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         752f4deb-329c-4683-b5b7-cbf3ef10ebde)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         659e5869-0f1c-4cdc-a1ab-3a888a9431be)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be500a6e-7f41-44a8-8b9b-95a9a97ae1b6)(content(Whitespace\" \
         \"))))(Tile((id \
         1518722f-f88f-4abf-8320-b33dc298b212)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2a35b506-0c38-4de5-a5cb-1b649f1a1ac7)(content(Whitespace\"\\n\"))))(Tile((id \
         78b570a5-7604-4775-b8f5-4cee4036c8dc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bafcce3e-713a-4f7d-8d31-92755498e13e)(content(Whitespace\" \
         \"))))(Tile((id \
         d24233b3-9692-40ac-b4c3-eb4ce5c331c4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d93576c2-abf4-4a5e-923a-d4c5c84ed1d6)(content(Whitespace\"\\n\")))))))))(Tile((id \
         085c80ea-031d-439a-bad2-952a02b330eb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         daf7cbba-4bec-4354-b3ff-63bd54e586aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3201cc6-aef2-44c1-80dc-668727c94be0)(content(Whitespace\"\\n\"))))(Tile((id \
         0f20a134-02a5-4aea-9b4f-d26848a321ac)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         4ef2cfa0-f00a-4627-86de-6d6f30f6ea6b)(content(Whitespace\"\\n\"))))(Tile((id \
         e6587314-c46f-4943-be98-e9f56a4dc7cc)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         46ffbc27-77d3-4149-a73c-9cbff54eb90e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cbfb64f2-4174-4f6f-9ab0-92d3cdffb51d)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2b87410e-49cc-4e98-82dc-8f9ad4356f83)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3a20b120-20eb-4597-9dd4-5b68a4011a3c)(content(Whitespace\" \
         \"))))(Tile((id \
         4efd60dd-c91c-4ec1-b8ee-1e6ecd2cec66)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1be10120-97df-4fa1-84e3-64c50bbdc724)(content(Whitespace\"\\n\"))))(Tile((id \
         2f525c61-059d-40bd-8430-7285727a731b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7550cb5d-249e-467b-912e-5128d2db13db)(content(Whitespace\" \
         \"))))(Tile((id \
         d5d96ef9-efa5-4979-aad3-0b0933367691)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6ccd77e5-de75-4a6d-9a2f-cb1bfb245013)(content(Whitespace\"\\n\")))))))))(Tile((id \
         dc0b80bf-8e9d-4208-823a-462c32a227e2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54619f47-1c75-4d59-a695-e849a0292099)(content(Whitespace\"\\n\"))))(Secondary((id \
         162e74d1-6951-416e-9457-7743cffe3333)(content(Whitespace\"\\n\"))))(Tile((id \
         a95ddf80-b3c1-43cd-b66d-2311bd091c47)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7f448f9f-8800-425c-ac30-bd63a6047560)(content(Whitespace\"\\n\"))))(Tile((id \
         d4886f4a-5be6-4779-85a2-e5c94e26e0f1)(label(last))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c838aa4-678b-40fe-aac5-83c352ced9c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7175ddb0-c68a-4471-b9c9-5ceab582af92)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e30d2d5b-45e2-49b7-bc6a-6a312c4cb8b8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f2c60d2-a9a4-4200-be42-4938d352f368)(content(Whitespace\" \
         \"))))(Tile((id \
         78a3898f-4d6b-46c1-84c5-31d9ee19d75a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         41326931-3f91-49ac-a545-1ab87560ff20)(content(Whitespace\"\\n\"))))(Tile((id \
         a48e071e-be53-4086-afa2-6c6b8d2916a3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c5d600f-a8a1-4066-860c-952225b5c826)(content(Whitespace\" \
         \"))))(Tile((id \
         4bf81ed0-cece-4a6c-94ac-3845c7be372c)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7774484c-8d15-49d5-9186-03be82119a37)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         883471c9-22ee-4bcc-8b4a-70f3691268f9)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# LAST ELEMENT - SOLUTION #\n\n\
         # Each step of the fold replaces the accumulator   #\n\
         # with the current element. The final result is    #\n\
         # the last element seen. For empty list, returns   #\n\
         # the initial value (default).                     #\n\n\
         let last = fun (xs, default) ->\n\
         fold_left(xs, fun (acc, x) -> x, default)\n\
         in\n\n\
         test\n\
         last([1, 2, 3], 0)\n\
         == 3\n\
         end;\n\n\
         test\n\
         last([42], 0)\n\
         == 42\n\
         end;\n\n\
         test\n\
         last([1], 0)\n\
         == 1\n\
         end;\n\n\
         test\n\
         last([], 99)\n\
         == 99\n\
         end;\n\n\
         test\n\
         last([], 0)\n\
         == 0\n\
         end\n";
      refractors = "()";
    } )
