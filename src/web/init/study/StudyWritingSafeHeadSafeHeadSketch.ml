let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / safe-head / safe-head-sketch",
    {
      segment =
        "((Secondary((id \
         93012893-3294-4304-9706-2f7f04f2f8a6)(content(Comment\"# SAFE HEAD \
         TASK                               #\"))))(Secondary((id \
         dca08cdc-1e38-4445-9522-945143ecaa4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         51db27fa-81ae-4eb9-a35d-e7bb9997fd07)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         38e006ad-aa3b-4dca-9824-cfd29017b37e)(content(Whitespace\"\\n\"))))(Secondary((id \
         da83a931-bc8b-4776-8e72-c059f5c33b09)(content(Comment\"# Implement \
         safe_head: get the first element   #\"))))(Secondary((id \
         3a0cb4f5-a756-44d9-9816-0e27ca346124)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b664112-1899-4a27-9568-da7cf25eb092)(content(Comment\"# of a list, \
         or return a default if empty.     #\"))))(Secondary((id \
         11ba32bc-b11a-4434-871b-0f2e5e91b229)(content(Whitespace\"\\n\"))))(Secondary((id \
         23b498c3-82a8-463f-bd7b-54bfb6715d76)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         ed7f4ea4-059e-4cbb-8a17-83398e7c2af7)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d7269a5-fcac-4156-a8ae-3f513a7ff5c2)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         66ef55da-202d-414b-8fd3-8a0b853bab11)(content(Whitespace\"\\n\"))))(Secondary((id \
         82208b81-9375-469b-b4c8-77f65693c52b)(content(Comment\"#   \
         safe_head([1, 2, 3], 0) == 1               #\"))))(Secondary((id \
         d583dd40-3fb1-4ab5-bcc3-805f1f3ccf63)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6699738-054f-49c9-94e0-854336cae04b)(content(Comment\"#   \
         safe_head([], 99) == 99                    #\"))))(Secondary((id \
         c35d939e-21a1-4a57-a408-d24232a2385a)(content(Whitespace\"\\n\"))))(Secondary((id \
         f71d39e8-4b27-41c2-8671-bb74ef17c6ad)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         147435a7-9900-48e2-888b-d2401b933553)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2b08ae3-6e1d-49d4-913c-4490fd363c63)(content(Comment\"# Available \
         syntax:                            #\"))))(Secondary((id \
         8128d8cb-9714-4710-a2b2-87220134dcee)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6d72286-0411-422c-88fa-feec28cd393d)(content(Comment\"#   case \
         expr                                  #\"))))(Secondary((id \
         b4d682fc-c448-4c36-84e2-92cfc0cf73db)(content(Whitespace\"\\n\"))))(Secondary((id \
         dbda0350-22b9-4276-ab03-099df5c157d3)(content(Comment\"#   | pattern1 \
         => result1                      #\"))))(Secondary((id \
         6939dc67-65e2-45da-9f71-fb504494fc71)(content(Whitespace\"\\n\"))))(Secondary((id \
         db043ea7-3338-4632-bb77-248e5844c347)(content(Comment\"#   | pattern2 \
         => result2                      #\"))))(Secondary((id \
         bd668777-1655-48cc-a05d-61836c351b3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         d75d6ba0-fe04-47a8-88db-f797326a6a91)(content(Comment\"#   \
         end                                        #\"))))(Secondary((id \
         be4be173-432d-4fde-bc43-272ace720f2c)(content(Whitespace\"\\n\"))))(Secondary((id \
         4c701d60-5a63-4e81-9db3-a74799abfc1b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         983516d7-5dae-414d-a96b-6090dde78a44)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b59e990-dfef-4d2f-b429-cc97f8c00050)(content(Comment\"#   List \
         patterns: [], x::xs, [a, b, c]        #\"))))(Secondary((id \
         ddbba7fd-0fcd-456a-a135-94f2b9a8c990)(content(Whitespace\"\\n\"))))(Secondary((id \
         d9524fdc-645e-405a-8c40-4d8afe0f71bf)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         f52512ba-d8ca-43b0-a245-8e3ac3a7af47)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6518994-6a05-40aa-b097-488f1cc5b2af)(content(Comment\"# Tip: Turn on \
         auto-probe to see which branch  #\"))))(Secondary((id \
         515bd3d0-f6e4-4534-b602-a0345881b514)(content(Whitespace\"\\n\"))))(Secondary((id \
         1692a18d-f9e1-4537-9310-ffcd7a641c64)(content(Comment\"# is taken for \
         each test case.                 #\"))))(Secondary((id \
         26e63ef8-995e-4cca-b867-b6aef9658cc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         d3d995b1-3a33-4107-8525-eeaffcdbcc6e)(content(Whitespace\"\\n\"))))(Tile((id \
         d5087851-54ce-4313-9ff1-9520c9d1f0d1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0dbcb06a-8feb-4af3-9396-1e65a332f1ad)(content(Whitespace\" \
         \"))))(Tile((id \
         0dfef2a1-13ab-4d52-89f7-bb33c1780245)(label(safe_head))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b556a5f4-cc4f-4119-bea1-b43d0c313978)(content(Whitespace\" \
         \")))))((Secondary((id \
         8780a0c9-e625-4675-9c16-353ac6c4df59)(content(Whitespace\" \
         \"))))(Tile((id 6bc07263-b72d-4dd5-a65a-663061aaf3f3)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e1c3df62-9deb-4497-acbf-9d7fe49a03bc)(content(Whitespace\" \
         \"))))(Tile((id \
         c1d2d1e9-c196-49ba-ada0-fe389fb4624f)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         986a2d66-820d-4cf7-a803-a9b2462af3f8)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c5d778a1-08fc-4dc6-bfec-c64d117dab2a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7eacb740-6bc3-49a1-9734-6fc6ef049936)(content(Whitespace\" \
         \"))))(Tile((id \
         439c645a-d7ea-4f78-9005-441d8c543d37)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         1bbc73c7-6ebf-4320-9a3f-80d4c71a1797)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         338660c1-f2f2-4fa0-be71-3c3370ea7508)(content(Whitespace\"\\n\"))))(Tile((id \
         b643c014-b2c2-41a1-b0a4-639b4b02c7bd)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         607f6072-6ea4-4f5f-81dc-6976c56c6ac1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c45cca8e-c65a-4993-bec1-ad3316a9c157)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e5d2a4a-d56f-4321-8ff0-9439cc765334)(content(Whitespace\"\\n\"))))(Tile((id \
         b4d7bf94-de3e-4dd4-b031-607cf5fdc5f7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fadc1fdd-b3b1-4d7e-af38-4f4a562db7e1)(content(Whitespace\"\\n\"))))(Tile((id \
         9869c1d3-0b22-4eff-82bd-9f5c790dde3f)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c55ebd61-ed4b-4d85-bf08-adc3b6ab6f07)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         aec690c4-8846-4502-8ae3-016166e14076)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0762d820-8743-4494-a177-06db603394c1)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dcdd0b4f-c322-46ad-8e8e-764f9a161917)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ae2815b-8fb9-46c1-8618-c26a48045c37)(content(Whitespace\" \
         \"))))(Tile((id \
         b8ac9846-f3b6-4419-a12f-6939536ca94f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         572f71e7-3236-4021-b081-3c5d59284472)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce20c6ad-a630-4b3b-a745-d8c0ed72409e)(content(Whitespace\" \
         \"))))(Tile((id \
         66858894-726b-4aea-8717-1b25db68b063)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         1a3b06c1-7125-450d-8696-73c7ce9b9eeb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3fb2d42e-400e-4d54-ac84-9be1f0c63115)(content(Whitespace\" \
         \"))))(Tile((id \
         dc8a9a48-0684-47c2-8136-f08cf35cb250)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         83b5c73e-e3ce-4421-bbbf-c53a7cf3d66b)(content(Whitespace\"\\n\"))))(Tile((id \
         7f295d65-2f32-4016-ae95-3f6cd5daeadf)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9f2e034b-b52c-4b06-891b-6f0397d2033d)(content(Whitespace\" \
         \"))))(Tile((id \
         455b8ada-e51a-4bc8-8563-dba6c842ca77)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6da4b717-964c-4bae-91cc-77d71ea5f42b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         60bc4995-bd75-4449-985d-c94a9da132fe)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         711a56c9-57e5-48e9-9127-73e4f573d16a)(content(Whitespace\"\\n\"))))(Secondary((id \
         05c07dd3-0fbb-44ac-b0c8-87c960e3b692)(content(Whitespace\"\\n\"))))(Tile((id \
         d4b7e181-e587-4a2d-a16c-d73f6931b3ed)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         09c9a4bf-96e5-4219-96ec-6eeb1bebf44e)(content(Whitespace\"\\n\"))))(Tile((id \
         d39b0a46-472d-426b-87e9-5a7936782891)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         009a07ac-e853-4f96-a482-8c61c2abe78f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b05a6ce7-2bfb-4aea-b525-5855f59739cc)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         231096ca-0fd8-4214-b643-9d9b94fa6c0a)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         39b2773d-79e1-4915-9f15-20efea6b77c3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c8791a1-401e-4005-9ddf-472ff6b337e7)(content(Whitespace\" \
         \"))))(Tile((id \
         da29a281-b412-43b2-8a89-e703b503d269)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         58405ca5-eb71-4f95-b0c4-84827cc74be9)(content(Whitespace\"\\n\"))))(Tile((id \
         fea8fff6-408d-40bd-8b6d-aca4713509b4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f20f1923-de95-45dc-85d8-d6f4754cc9fe)(content(Whitespace\" \
         \"))))(Tile((id \
         9fce8c08-edca-4ba8-9d28-8015de00a87a)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         203e68bb-467f-4dea-9b6d-4b43854c0ba9)(content(Whitespace\"\\n\")))))))))(Tile((id \
         aa8d18ef-22c3-4bf1-9efd-fd6aaade39c9)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bc365649-1966-4471-ac1d-bc065bdc1892)(content(Whitespace\"\\n\"))))(Secondary((id \
         1043ebf1-5af0-4b6e-a82b-74336e1e5f41)(content(Whitespace\"\\n\"))))(Tile((id \
         86decc7b-3895-445f-a873-3a88cbb74316)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         cc8da847-9585-4591-aa0b-f7f23d0b37e0)(content(Whitespace\"\\n\"))))(Tile((id \
         0e1f8862-bb1d-4fe1-98d2-ba5476f0435c)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e71482b5-cff5-4220-b841-09d751ae6328)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5ed155b0-ca97-45ce-a559-aaf5d9bf38e9)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3cfbbac4-c826-4369-9b6b-d5af53f023f2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71e20346-6e20-457b-ad83-e59b37b68977)(content(Whitespace\" \
         \"))))(Tile((id \
         350ec474-f871-4d7c-ad50-c52074e77c69)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         548554d7-54a3-4a95-8f6d-1dd1fe8f5bda)(content(Whitespace\"\\n\"))))(Tile((id \
         04bf7547-127e-4cd6-9a45-eb02ea7f15b0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1b35ab1-47c8-49b9-aad5-f38dbf4e1182)(content(Whitespace\" \
         \"))))(Tile((id \
         57eec630-7040-4467-875b-5939e3727942)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2d1f6382-e2de-427c-bffe-937750b9cb2b)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b034b97c-ab71-46e3-bc14-b60d8675b324)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2da32160-6898-4d28-ba46-066a7deeee9b)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b76acdb-9792-4d3b-977a-d10735061048)(content(Whitespace\"\\n\"))))(Tile((id \
         5a42161e-a608-4a11-b1c2-e5d98e4598de)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0634370a-057f-4785-813f-d4d1bc883258)(content(Whitespace\"\\n\"))))(Tile((id \
         e3ca3b5c-edd3-47df-a6ad-5e0507246f7a)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d226fa2-fcf4-4afe-9ef7-749054c82fd1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ab80acd5-8f0f-49a7-a938-6f9f4a7cc6ec)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f657cd41-00dc-4c07-bf69-c4ef2b44a064)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9952be85-8255-4f1b-a239-8e24a7bd2af5)(content(Whitespace\" \
         \"))))(Tile((id \
         7b72f644-9e8d-4c88-bc5f-0ede907aca67)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a4c8cad2-b880-4a0a-86d7-0b1e40447bc1)(content(Whitespace\"\\n\"))))(Tile((id \
         35dbe793-e4a3-41eb-94a1-0361d59a8b11)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         19956828-867e-44a5-b0e3-3f9debb1bdc1)(content(Whitespace\" \
         \"))))(Tile((id \
         45774d69-7972-4913-8484-cdea20e0be6d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bddeea43-928b-44cc-b7e6-540daf9d0130)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c2f25aeb-66b3-486a-a690-869ba36818ca)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# SAFE HEAD TASK                               #\n\
         #                                              #\n\
         # Implement safe_head: get the first element   #\n\
         # of a list, or return a default if empty.     #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   safe_head([1, 2, 3], 0) == 1               #\n\
         #   safe_head([], 99) == 99                    #\n\
         #                                              #\n\
         # Available syntax:                            #\n\
         #   case expr                                  #\n\
         #   | pattern1 => result1                      #\n\
         #   | pattern2 => result2                      #\n\
         #   end                                        #\n\
         #                                              #\n\
         #   List patterns: [], x::xs, [a, b, c]        #\n\
         #                                              #\n\
         # Tip: Turn on auto-probe to see which branch  #\n\
         # is taken for each test case.                 #\n\n\
         let safe_head = fun (xs, default) ->\n\
         ?\n\
         in\n\n\
         test\n\
         safe_head([1, 2, 3], 0)\n\
         == 1\n\
         end;\n\n\
         test\n\
         safe_head([42], 0)\n\
         == 42\n\
         end;\n\n\
         test\n\
         safe_head([], 99)\n\
         == 99\n\
         end;\n\n\
         test\n\
         safe_head([], 0)\n\
         == 0\n\
         end\n";
      refractors = "()";
    } )
