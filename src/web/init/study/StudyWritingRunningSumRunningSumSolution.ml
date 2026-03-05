let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-solution",
    {
      segment =
        "((Secondary((id \
         c992651f-d2b9-42ac-bdd6-1c35cf7d0df2)(content(Comment\"# RUNNING SUM \
         - SOLUTION #\"))))(Secondary((id \
         cdea0736-7a40-4f68-a4cf-492d98b24ee9)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c7d251b-6ad8-4bd6-9c74-362a630f9bd3)(content(Whitespace\"\\n\"))))(Secondary((id \
         49efcbed-6b8c-4d6d-9e6f-61e7a8422458)(content(Comment\"# Uses \
         fold_left with a tuple accumulator:         #\"))))(Secondary((id \
         d5d91aac-7ce3-4c67-9494-c88a66126715)(content(Whitespace\"\\n\"))))(Secondary((id \
         411c08a9-b9ee-4282-a07c-6e8784bea47f)(content(Comment\"# \
         (running_total, result_list_so_far)              \
         #\"))))(Secondary((id \
         ba0703cb-3b83-444b-ba23-7ecf1e88d7e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         f92d4ded-3be5-4965-a930-593dd9825480)(content(Comment\"# On each \
         step, add current element to total,      #\"))))(Secondary((id \
         57591716-8b52-42dc-b88a-74b39479fcb0)(content(Whitespace\"\\n\"))))(Secondary((id \
         90784b72-ee69-4fb9-8c1f-c54959eda162)(content(Comment\"# append new \
         total to result list.                 #\"))))(Secondary((id \
         d974630a-cb06-45fe-b752-5982e9e2a3a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ab82786-0fdc-4eb8-b9c6-e5b532ee3a22)(content(Whitespace\"\\n\"))))(Tile((id \
         dc8f4736-eb5a-4deb-93f0-153715aff3d4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8d49f26c-6a5b-497b-a6d0-8d175bf458e0)(content(Whitespace\" \
         \"))))(Tile((id \
         8de778e3-5f66-4e60-b5d3-0a32878641a3)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f55b07d3-4d09-4aad-8535-4d3f92dfec94)(content(Whitespace\" \
         \")))))((Secondary((id \
         27bb9724-b037-4bee-8929-84b342473edb)(content(Whitespace\" \
         \"))))(Tile((id c0a89a7a-1099-4e49-ad62-6d73d03c256b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         c7929b90-70ac-4b90-96bc-50a3683983a7)(content(Whitespace\" \
         \"))))(Tile((id \
         d302ae21-5888-4cf1-8a8a-a2bdca4cfb34)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dfc27ec3-82b1-42ed-9920-6701711b7edd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         90bd2641-6a6f-4c69-83cf-5ce678b33302)(content(Whitespace\"\\n\"))))(Tile((id \
         337437bb-c233-4943-a02f-099a58ace116)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4d1e1b88-8c8e-46a6-aae4-5f3c2fd70927)(content(Whitespace\" \
         \"))))(Tile((id \
         f59dd375-c7a0-43bf-b583-5c183e607c0e)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a49cb1f1-61f5-4893-8594-1c3a7f211523)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a94d6b6f-aeee-4044-b1cf-90f2b1ec40f7)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         634a9e29-a7dc-409f-8f3d-87f94b70fb9a)(content(Whitespace\" \
         \"))))(Tile((id \
         92214895-bedb-496a-9d96-2b1de84bc739)(label(result))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         b5be8b96-0d8d-42a3-80fb-9229294f8165)(content(Whitespace\" \
         \")))))((Secondary((id \
         acae5223-1afe-47b6-9e12-f6021e8e94cc)(content(Whitespace\" \
         \"))))(Tile((id \
         4e28b0be-75fb-43f7-b2f1-0b1414427956)(label(fold_left))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d8bd481c-104a-4a88-8c6e-2e1c39815ad7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b61857e5-7b4b-4859-8135-80035e749e6f)(label(nums))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1b85d1c8-8069-42d5-833d-40d4f93c29cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         287d8b7c-27ea-4787-8922-3a1f9b5430d6)(content(Whitespace\"\\n\"))))(Tile((id \
         3b8c7cf7-792f-47b1-8b79-b0ba9b19bc82)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e60bba87-3648-4163-8bc5-08a041207fd6)(content(Whitespace\" \
         \"))))(Tile((id \
         ff0c318c-208a-4cda-8559-a0b55e84e453)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         95414518-92e4-4162-b4d1-911534d75e2b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         f7238362-7986-43c9-adb2-1b79285ed880)(label(total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a6d05456-19db-45e3-a910-e7ab187873da)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b35a58bb-1a19-4cb4-a933-c3e638a8c012)(content(Whitespace\" \
         \"))))(Tile((id \
         e2a9263f-fb0b-46c0-88fc-4d969ec5548f)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Tile((id \
         d8410688-ec47-4c81-bbfd-ee75f1e6a743)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         65799c45-b7e8-4118-a341-315d17757de5)(content(Whitespace\" \
         \"))))(Tile((id \
         9a0a7cbf-99ff-4189-ab4d-f9a3d5187eb8)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         be2e141f-af9d-4658-9d77-842dccbb7fa8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         55adedf7-7cb0-4b34-b497-890d40110872)(content(Whitespace\" \
         \"))))(Tile((id \
         4ea716ae-80cb-4880-b943-04330d737805)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         78d8b4a6-cb0e-4fde-a5d1-714f45e5e7ac)(content(Whitespace\"\\n\"))))(Tile((id \
         c3110482-0ea0-4ed9-8366-fdb8a20dc346)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d70a1c99-6c52-4d5c-9ab1-a8af5e399c23)(content(Whitespace\" \
         \"))))(Tile((id \
         e16a781c-2b9c-4a59-91a1-314a87e4b7c1)(label(new_total))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e8550fa2-78e8-4584-a231-b18b23331125)(content(Whitespace\" \
         \")))))((Secondary((id \
         8601ca25-85e2-411b-a508-b80427845068)(content(Whitespace\" \
         \"))))(Tile((id \
         9cd8a605-63df-4111-80b4-875ff22c2ad1)(label(total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         095fab54-b32f-4e20-88a4-82dc1dd0b713)(content(Whitespace\" \
         \"))))(Tile((id \
         9929436e-ea19-4cb9-ae01-ee0c151e59e3)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         82a00057-27b6-4eda-9262-067ccc41ac53)(content(Whitespace\" \
         \"))))(Tile((id \
         5f97187d-57b7-4656-8c20-055c869ea568)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         32717de1-84df-43b6-8908-caabd860f04e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ca553a4c-9edb-47d7-b2ea-edd079a14d5d)(content(Whitespace\"\\n\"))))(Tile((id \
         df2f381a-1353-4df6-a4d8-938bade22a7c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9eb926fe-596b-4693-96ef-ca404a69fe23)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53eb632b-6e02-4c23-842c-7626c104339b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         02f44149-e324-4136-9c72-88ea237d9da8)(content(Whitespace\" \
         \"))))(Tile((id \
         04dbcd6e-8a3a-4a8c-ab4b-c79204fbc2da)(label(append))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         088e3530-295f-4529-85b2-25422d506b22)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5b680e91-1f5e-4102-9e83-e53bc3cc919d)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         64119c83-bf88-4928-8c72-31a8eff79b49)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         949bd0a1-4ed5-4f39-806d-1f560bc5d139)(content(Whitespace\" \
         \"))))(Tile((id 91be771c-89a9-4cd9-ab10-209f25cc5ad9)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af3276d5-d01b-4325-8894-df38b72cc4d6)(label(new_total))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))))))))))))(Tile((id \
         126a3841-f40e-4ecf-b2d0-4dcb49dcfb08)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1be2188-1d68-4b49-914f-1f72dd04a776)(content(Whitespace\"\\n\"))))(Tile((id \
         eb75250b-1527-4f67-b011-742f6f589e47)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cf9ae8fb-48dd-4c4c-92ae-b950aeb6e367)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a40a72b-58c8-4f51-a6ed-b3e089493bf1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c03b9bac-394f-44af-a53f-c6c996b7b263)(content(Whitespace\" \
         \"))))(Tile((id \
         de7244d1-fa82-46e0-9054-78e10a4b6197)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a4a795d9-7506-46a1-9177-3147b1f82928)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a37d1951-7f51-4838-aff3-bb89e6ddfa5d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e9c05cc5-0122-4ba9-b150-f1c904334725)(content(Whitespace\"\\n\"))))(Tile((id \
         324bbb82-1680-4452-9a81-e5a8f8dc7e2c)(label(result))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fe48b6d3-e2da-44f8-b7ee-afb4b6b7b7a1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e0174b0e-8288-4b9e-9282-96645e24c922)(content(Whitespace\"\\n\"))))(Secondary((id \
         a95bb455-e2d6-4732-a23e-e24dee1fa737)(content(Whitespace\"\\n\"))))(Tile((id \
         49f12afb-a5f5-4f64-9341-ae178fe2cf3e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a0821dd4-4b5b-4c95-bf2e-2a26f7456e21)(content(Whitespace\"\\n\"))))(Tile((id \
         a4a33c8f-aed4-44a7-8d21-0d918bfda2f6)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         33ed95d9-4fb6-468c-b2a5-4ceb8e6c4434)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         631699ce-ecad-4134-b721-346f4e34f51b)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5b1d8d1e-6764-4225-b70e-5da440880ac3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff37e3c5-cdbf-435f-ad10-60d51d4959f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7173e5da-dc76-4f55-85f4-81a81bb5650a)(content(Whitespace\" \
         \"))))(Tile((id \
         c1526e3d-0fe9-4d62-84bb-6bff7c140e8e)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6ecd768a-2ae7-491c-bee2-622cd1d5543d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f1b1eff4-b221-4209-a6bb-1ec9f7107573)(content(Whitespace\" \
         \"))))(Tile((id \
         4699bb7f-f56c-4717-912d-14b1b98a1980)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         4576bc04-34c4-4173-a4d9-326715ef4dfe)(content(Whitespace\"\\n\"))))(Tile((id \
         07f770b5-d65b-49cb-a043-5ccc223a97b7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         401aaec0-cac0-42f7-91ce-f60bc6c2e8c2)(content(Whitespace\" \
         \"))))(Tile((id cc43871a-bcfa-4300-a89b-4c44135168aa)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         093df495-58af-440a-a2a1-6f656fbfe9a2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29d9c2b0-a5a3-48d0-937d-e9aa3e856934)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50549474-adcc-4a8c-8f59-950d6a45acda)(content(Whitespace\" \
         \"))))(Tile((id \
         9eb27c38-122b-43ad-a14c-60a19387db17)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         044ae355-a51b-4b91-ac8c-ff04d9e64afa)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2267af74-d24e-4500-88a5-5d9afe1f9c4f)(content(Whitespace\" \
         \"))))(Tile((id \
         8b51d3d9-198b-49cb-9d68-be3479c568b7)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c1aea88f-d9a5-486c-84cb-6a0a446f720f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         503864f2-af28-4990-a629-c835a4c7f1a4)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         22713da7-bbef-409a-8f8a-736f15d177ae)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f79712f-4b5d-45e6-a08b-fc7b0e48ecb6)(content(Whitespace\"\\n\"))))(Tile((id \
         c6c78d16-39cd-4ed5-b35b-c755e04ad200)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a33e007a-c11a-45f3-b8ea-7bdd27efa490)(content(Whitespace\"\\n\"))))(Tile((id \
         8e32aa13-6330-47b9-94e2-5ad9fb8656d3)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b1bb3121-e823-4603-8b98-3908a4fd76bb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         93924291-1134-4bab-ac56-808494331584)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         c31699db-6093-4b78-895a-c5deed1fb89a)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         74637f10-9e90-4e2e-8f63-35e70fade06c)(content(Whitespace\"\\n\"))))(Tile((id \
         6400b167-de4b-4d5c-87be-05097b7a8635)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1b7901bd-d09e-4d07-86ef-a7a1764010f5)(content(Whitespace\" \
         \"))))(Tile((id c83b065c-092e-4b8e-86a8-47cda8291bf5)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         73137eed-5fcf-4a38-96e0-88adb40d7c17)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9d7ce3d0-47f2-4607-b390-87ce91daa25b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4c755ae0-6152-47f9-966b-94fecd09d15c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c572e432-cd7f-43f7-9f96-ee5cb0ddbc1e)(content(Whitespace\"\\n\"))))(Secondary((id \
         327d8547-a16a-4f27-9ee6-b2a1a883a41f)(content(Whitespace\"\\n\"))))(Tile((id \
         3d5e4eca-643f-467a-9928-3bc6460c3c16)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5452a66e-08bf-4973-9527-d0a94ce11559)(content(Whitespace\"\\n\"))))(Tile((id \
         104e4acd-62b8-4627-a67f-15b8606e399a)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         13d1cdcc-ed5f-483f-85c1-80aeac2e01a7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a90d4177-0523-40a7-84fa-9a08edee072d)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1c952c6b-68b8-4cc1-8400-d3b66f3462f4)(content(Whitespace\"\\n\"))))(Tile((id \
         4a0c6b94-a1d5-4510-bc79-2795a8bf51bc)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f412a9dc-7cf2-4197-905a-581d9add8477)(content(Whitespace\" \
         \"))))(Tile((id \
         228d5c9f-44af-42e3-bd63-86cd7672fecd)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9b01a67e-cd39-4015-98dd-a9402c45a7eb)(content(Whitespace\"\\n\")))))))))(Tile((id \
         51b645d4-fbda-4ae5-affe-995459304989)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36e5256d-c810-4445-987a-758072d272e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa94b897-0165-47ad-a7da-fbb04aea29c0)(content(Whitespace\"\\n\"))))(Tile((id \
         9aafd052-e150-4b54-9dec-b938a2642a4e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f3e1aae9-6833-4e19-b6ff-0f36cfc85a6d)(content(Whitespace\"\\n\"))))(Tile((id \
         096ff51b-41ed-47c9-94bd-3804f11a370c)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aaae3314-d7f2-4c7d-b1f5-c819f85024ae)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         27889810-acbc-4e36-9b3f-6b56f79df208)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ce047e04-bad3-4df6-b5c0-f971039b9b5b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3b5833dd-d889-448d-af5b-1d6a2ff29771)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90aa4ba3-be30-4a5f-ab92-1b66883ef212)(content(Whitespace\" \
         \"))))(Tile((id \
         2e462159-3d02-4e79-b8cb-4fbef54d6b4c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         466c92ac-e60e-4889-b287-5ce066a918ea)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4f32a96e-0431-48f6-9bbc-654fa7b42c2e)(content(Whitespace\" \
         \"))))(Tile((id \
         8b707801-25c8-42bc-8f7c-4577fac94a34)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         76cc5c9e-3a7a-41c8-b26a-e8e3be24e131)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5623ab8c-1a5f-4f28-b9e1-1711ef7bb1f6)(content(Whitespace\" \
         \"))))(Tile((id \
         1bdd082c-af18-43b6-b601-99b92f1c1c35)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         37072d67-e3ce-4ba5-9fa4-586e68e34fcd)(content(Whitespace\"\\n\"))))(Tile((id \
         8dc73ae3-e278-4223-916c-5c875dc087df)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7eb7ada-be0a-48c2-b2f7-759c6105bcbf)(content(Whitespace\" \
         \"))))(Tile((id 8813bf53-a856-41dc-b63c-02bd774c5399)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         be491a46-bd82-4683-86be-f9b26c255bda)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         24897f40-58ab-4888-92ae-ec0d8f810f30)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aabf6df7-6ae4-4878-9178-a2aa1405d196)(content(Whitespace\" \
         \"))))(Tile((id \
         17723e73-8d2c-4764-ad8a-109c6b0f1cdd)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7ee72d68-c392-4c75-ae2a-99e83ea23f03)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3a0ceb9-f993-4933-8c9b-c50857d52a02)(content(Whitespace\" \
         \"))))(Tile((id \
         cd7e24c5-4abe-4c40-aee4-9ab519488481)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e962d7dc-1126-4e6f-8363-a9c758c4739c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ebcd4553-919d-4749-8d40-e22dfe20af2d)(content(Whitespace\" \
         \"))))(Tile((id \
         7a5709e0-d675-4cef-a612-6037e765a021)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1c512e5c-ddb2-47ee-9a86-dec24e26f289)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4b3ec9ec-a13a-4dfc-a931-281e57280238)(content(Whitespace\"\\n\")))))";
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
