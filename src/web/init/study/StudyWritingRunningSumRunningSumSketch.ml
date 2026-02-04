let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / running-sum / running-sum-sketch",
    {
      segment =
        "((Secondary((id \
         0a600236-1669-46da-93f6-69d30a705c91)(content(Comment\"# RUNNING SUM \
         TASK                             #\"))))(Secondary((id \
         bcea7994-f262-451c-8933-bef3a8158b34)(content(Whitespace\"\\n\"))))(Secondary((id \
         46146622-b552-4567-a073-197e01d1ed4b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         e39de7a9-4aa2-49c9-8013-c5acd8e2d419)(content(Whitespace\"\\n\"))))(Secondary((id \
         808b8d80-d041-4b6c-9e70-d969bfd131cc)(content(Comment\"# Implement \
         running_sum: compute a list where  #\"))))(Secondary((id \
         cf44cc37-ef4f-4a21-87b7-f697e4198dbf)(content(Whitespace\"\\n\"))))(Secondary((id \
         82d64615-4619-484a-a0a9-55ed72dde29e)(content(Comment\"# each element \
         is the sum of all elements up   #\"))))(Secondary((id \
         bfd0962c-2b5b-45e8-86cf-f4a4b1c2c835)(content(Whitespace\"\\n\"))))(Secondary((id \
         188edf60-1d94-467d-86ea-edfdeb4e9218)(content(Comment\"# to and \
         including that position.              #\"))))(Secondary((id \
         ef9442bf-4ec1-41d6-b1f1-05c8825fe9d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         bcff1c8c-e912-447e-8fa2-0392031196b9)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         87677b65-d0c0-4d6c-834b-1daa7d995279)(content(Whitespace\"\\n\"))))(Secondary((id \
         6bd9cabc-7dd7-4d3d-8f17-869f72017e6e)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         7407aed0-2ff9-4996-83c3-402e80fccb6f)(content(Whitespace\"\\n\"))))(Secondary((id \
         75ca5792-9bd1-485d-a119-91e9f34b4634)(content(Comment\"#   \
         running_sum([1, 2, 3]) == [1, 3, 6]        #\"))))(Secondary((id \
         f18fde77-61ea-4dad-8b9a-4f633e262888)(content(Whitespace\"\\n\"))))(Secondary((id \
         dc2e82a5-562a-4b10-8906-ae3e55a9b4f6)(content(Comment\"#   \
         running_sum([5]) == [5]                    #\"))))(Secondary((id \
         58e003dd-22cd-4110-946f-db21368f7fdd)(content(Whitespace\"\\n\"))))(Secondary((id \
         1077f30e-0a4a-4ba3-a5d4-b5d514400d2f)(content(Comment\"#   \
         running_sum([]) == []                      #\"))))(Secondary((id \
         0044506c-f41e-4fff-962b-593e68a6e7e4)(content(Whitespace\"\\n\"))))(Secondary((id \
         adf78abe-971b-4225-be5d-cde96b847a87)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         2202f642-5e86-4c6e-9d2d-1119d3593b7a)(content(Whitespace\"\\n\"))))(Secondary((id \
         3b2858cf-5582-4b29-80e4-2683cb4b5edd)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         f686cfb5-925b-4b16-a7a4-a30453146028)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a40ac70-bf84-47ca-b428-346696cb22f0)(content(Comment\"#   \
         fold_left(list, fn, init) -> result        #\"))))(Secondary((id \
         484bba0c-191e-4f9c-b1ea-dea2cc6b5a13)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab8e91a4-0806-482f-889a-f8a95c6f2385)(content(Comment\"#     fn takes \
         (accumulator, element)          #\"))))(Secondary((id \
         1a005b92-d39c-41ca-8fa1-36c28296725a)(content(Whitespace\"\\n\"))))(Secondary((id \
         18b54209-4a9d-4c03-84c9-b8035c271523)(content(Comment\"#   \
         append(list1, list2) -> list               #\"))))(Secondary((id \
         d8dc0d4b-c7b3-43f5-90b3-9a4734c7f518)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d58b6a9-eae3-4b53-9128-a6b4b44075e1)(content(Comment\"#   rev(list) \
         -> list                          #\"))))(Secondary((id \
         a34af0bc-847b-4d86-ac07-6ba2842c9e7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         4feab0de-fe0b-4fb1-942b-93b4e6e573d6)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         99e0e8bf-ae44-4ef0-bf9e-9693478f6cd1)(content(Whitespace\"\\n\"))))(Secondary((id \
         a297b11e-bc7e-459e-95a3-961119dae152)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         afc9bdc7-9529-43b8-b00c-c068c9bdf6c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         e2058ad8-be53-4062-9909-c5506c4875a4)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         593dfa39-e91e-4944-8204-64c0b853e875)(content(Whitespace\"\\n\"))))(Secondary((id \
         7430d92c-9216-4d22-b215-c013d1023f39)(content(Comment\"# Syntax \
         reminders:                            #\"))))(Secondary((id \
         7fd1412e-0331-4d98-9887-dc5908df96c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         1f155c27-cd49-4e09-9b3f-66652091020f)(content(Comment\"#   Tuple: (a, \
         b) = ...                        #\"))))(Secondary((id \
         e0956a00-7b4c-429c-ac94-61edcac769a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8506f48-40d5-4f7d-a686-7b131ee7dc8d)(content(Comment\"#   Tuple \
         access via pattern: let (x, y) = t   #\"))))(Secondary((id \
         68d6d88e-b8c0-4e81-86d4-8235f4692c18)(content(Whitespace\"\\n\"))))(Secondary((id \
         a320fe9b-2df4-4f88-8efd-4e6b8ce2d201)(content(Comment\"#   List cons: \
         x::xs, List literal: [1, 2, 3]  #\"))))(Secondary((id \
         ac950cbf-5e13-4d19-ac62-df21466bb766)(content(Whitespace\"\\n\"))))(Secondary((id \
         00b3ed81-e783-4899-9c85-a9752f027b56)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         2f5a7a8b-1feb-4a0c-8dd4-b07b4b1162f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         077989b7-bd33-4e34-932c-421e6a9cf114)(content(Comment\"# Tip: You may \
         need to track both the running  #\"))))(Secondary((id \
         f86708a0-10b1-4f84-8058-1905e637292e)(content(Whitespace\"\\n\"))))(Secondary((id \
         835ecba1-7772-41ec-952c-aa32fb6f8b8a)(content(Comment\"# total and \
         the result list in your fold.      #\"))))(Secondary((id \
         e20421b7-06c9-4b70-84b5-03ba55764b4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b01c026-bf99-4324-b720-0a9d16c5674f)(content(Whitespace\"\\n\"))))(Tile((id \
         337dcb21-9c8a-4b7d-8d79-af511ddff998)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         99a1ce70-a895-42bc-a6c5-b475127b0583)(content(Whitespace\" \
         \"))))(Tile((id \
         a02c3e71-f664-4477-94ed-9239c65eba0c)(label(running_sum))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7b6d983b-7927-4681-9918-326fef966bb1)(content(Whitespace\" \
         \")))))((Secondary((id \
         35b9eb92-7354-4f53-9b40-5d20501797b9)(content(Whitespace\" \
         \"))))(Tile((id c1bf445f-1246-4988-b7f1-c6e4a551aa60)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         feca0170-9d1d-4891-8513-bdb2fff13cbf)(content(Whitespace\" \
         \"))))(Tile((id \
         11219207-93f2-4493-bbfc-c8543620cf4e)(label(nums))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         109360d0-2679-431c-9628-a9564488e954)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7bcdd867-db9e-46d3-9882-f2f12c814bbe)(content(Whitespace\"\\n\"))))(Tile((id \
         9ecc722c-7f22-485b-b2ac-24a7aca70f79)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         fcef239e-166a-4980-b831-120bfbd30bb7)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         23e78f78-3962-4552-beec-9acb7e78f6c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         19b6089e-104b-4379-8237-e0c5bb00c511)(content(Whitespace\"\\n\"))))(Tile((id \
         ed2e7303-8b76-4828-9aef-3175035659fc)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         bc939379-0662-4152-9c6b-78f9927cd0b3)(content(Whitespace\"\\n\"))))(Tile((id \
         89f2b3e4-8ffa-4cc7-a000-5d040edc5c52)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         05c4904d-31ca-4847-82ad-e8ee5f6fba36)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         03183ca0-c871-46d3-98e8-f7af56ab6e8b)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3ba16c8c-1ddf-4a84-b5d0-9171ee3f0d5a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0ce6379a-a75b-4a18-9f9b-3fdf05080e84)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5f68736-61ff-4018-a33d-963dea5a94b3)(content(Whitespace\" \
         \"))))(Tile((id \
         6fd3c57f-cfe0-4b0a-b3e4-098431b7662f)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         71774b86-e37a-4d48-ab6b-2cbb4d3e54f3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17c043f5-c4e4-4875-a03c-f8768e413490)(content(Whitespace\" \
         \"))))(Tile((id \
         7b9bfb37-2825-41cf-a444-12d757415d56)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d98a14b8-2b19-431a-8b4c-b14e9c75aae4)(content(Whitespace\"\\n\"))))(Tile((id \
         dda19539-c094-4fb0-8a49-a9ef1f2e992f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f54f8536-1bf9-4116-b0e0-15575f2313a0)(content(Whitespace\" \
         \"))))(Tile((id 6b5afc3d-2faa-4416-a73d-4027ccad7ff0)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         009ba1dc-6900-47b4-8939-2edb26f81864)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1bd5a5a7-86f3-4a79-8ef8-c827d8884f6d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50396cb7-e50f-46bb-b231-68fa5664b5e6)(content(Whitespace\" \
         \"))))(Tile((id \
         8b2fa74a-2669-49e5-8916-049e45b1a26c)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6f454ec-14b2-44ef-a7f1-71e74b3e2051)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         37d6ebb7-7136-4f6a-b70e-3daba060ff10)(content(Whitespace\" \
         \"))))(Tile((id \
         0054ebc8-82ac-40f8-a2df-9b652265127f)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6797af6b-0c02-42e3-8a37-80107a188869)(content(Whitespace\"\\n\")))))))))(Tile((id \
         42d7dca3-a331-42e2-a08d-cc91de79a1ed)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2f55cef3-f004-42e4-9968-b55485b91b1b)(content(Whitespace\"\\n\"))))(Secondary((id \
         66a2a9d3-632d-435e-8424-1e563bd7609f)(content(Whitespace\"\\n\"))))(Tile((id \
         f2ce33f2-d6d3-492d-8844-d6ece570fa74)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b5b20fd8-b9c7-4a29-823a-2c750df88070)(content(Whitespace\"\\n\"))))(Tile((id \
         96e1f3c9-0d4c-4d7a-88f6-c779901b1239)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         151a9025-47bb-4053-885f-62bca3166cd8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1c11e23f-d936-4a36-a800-d2ef20bb0846)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ca6b910d-f93d-46a5-93d5-b04d56a11582)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         1258964e-5b46-4639-8973-1413e7a03efd)(content(Whitespace\"\\n\"))))(Tile((id \
         0b33efe1-1c13-4085-b466-2e5683a2b75f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99a2a312-bc2e-484f-8bff-feac5ed95233)(content(Whitespace\" \
         \"))))(Tile((id 926dc6d3-a9b9-41bd-b297-f80b50a8a8ab)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f91203e7-d9dc-4fab-9ffb-d90d948ed283)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         fdde1e77-412b-434c-973d-c51d17006972)(content(Whitespace\"\\n\")))))))))(Tile((id \
         53e3592c-8cc7-4191-a596-e9ba17b6f3ab)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac0ae1c5-1e31-4045-9be0-703ea50f9094)(content(Whitespace\"\\n\"))))(Secondary((id \
         9013652f-be06-4a47-86e1-9a505f3110e3)(content(Whitespace\"\\n\"))))(Tile((id \
         8ea65bdd-e6c5-479f-8ead-32a2a007de8b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6c9a9f9b-9810-41b6-8f3e-e0ee63969e6d)(content(Whitespace\"\\n\"))))(Tile((id \
         90d606b9-7c26-4fee-8947-93905aebc844)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5a86181f-fa0e-486d-95df-cdf57836ce52)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9918a278-80cb-4041-8632-818a519e234d)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         20c340e0-5510-4380-8784-cf0d5d210b68)(content(Whitespace\"\\n\"))))(Tile((id \
         79d18160-c754-4ee5-9bfa-798791dbcdb0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77034799-f8ac-4533-8243-0f63e11bca6c)(content(Whitespace\" \
         \"))))(Tile((id \
         f3aaf10a-e1f4-4848-aa9b-31a0a131eae2)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         eda6a3b9-f3f7-47b0-805b-02a6db83120d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         599dc8e7-1025-4ea0-8a24-550343ea8ad3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb3af8e5-0859-4b92-a864-57576e87f030)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1303050-9a0f-4871-bcf4-b5730fe7146f)(content(Whitespace\"\\n\"))))(Tile((id \
         dc096765-d196-49c3-bf91-304f0970fc4b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         787c22fc-97af-42cb-8456-3fc3167456a8)(content(Whitespace\"\\n\"))))(Tile((id \
         8de6742b-ced4-42cd-9d84-6f7c4e8f8bfb)(label(running_sum))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7cd957f5-7914-481a-b111-b3f0fd7eff8d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ffec1c2c-e400-4b81-96be-d74f7d65f2a7)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         996475c1-f66e-4df3-9f08-445605bc9552)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5b7d4d3d-74e1-4766-88da-cc197f4ea4b3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad189f82-9e5c-42ca-b955-116b76e99ab9)(content(Whitespace\" \
         \"))))(Tile((id \
         7f95aecb-5dc7-4700-957d-14a9ae675834)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a741ab8-760b-4fc9-bc73-67432b15eba5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42f3ccde-b971-4f19-987e-1b80051fbe28)(content(Whitespace\" \
         \"))))(Tile((id \
         7732de47-42d4-415a-9a83-aae1c8321588)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d14e67bd-0cb4-4902-b24c-6b9d52ce3b06)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         96475b28-1022-4154-a6ef-519a2951ce9a)(content(Whitespace\" \
         \"))))(Tile((id \
         43c47c3f-8196-4a37-96d3-508a0315b5e2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         d253a511-df23-4682-a4e9-fcbf4fdb3353)(content(Whitespace\"\\n\"))))(Tile((id \
         08d79f47-4141-4db6-9d35-99cc2036968a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fca6c2d8-3e7c-45b4-9781-a70562935faf)(content(Whitespace\" \
         \"))))(Tile((id 070419ee-ef9a-4c07-99ee-f134c62a20dc)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         6b8d6a93-44de-4477-9ab0-60cb04e101b0)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         179f5881-4aaa-49d4-a48d-1da6f047a8f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         75a08023-a15c-442f-867a-6ff4bcdc8df0)(content(Whitespace\" \
         \"))))(Tile((id \
         e6ce329f-a7d2-4f37-bd0f-5a15f66035a3)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         886812e0-ff07-415d-b228-8484020ca270)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         18de6eed-749a-4789-b5ff-31cccdf2bb52)(content(Whitespace\" \
         \"))))(Tile((id \
         82b708b8-cf06-44c6-997a-065d523c395a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         30dc6e0b-bb79-492b-a509-3c8bb0504b21)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2b0410e3-a52e-4866-8211-025d27157354)(content(Whitespace\" \
         \"))))(Tile((id \
         2458bad7-d7cb-46ae-a5b0-10772156d0d9)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b6476484-80a7-472a-b5af-7e8602e805e2)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         b9866d30-092e-4e33-b61e-848240edee77)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# RUNNING SUM TASK                             #\n\
         #                                              #\n\
         # Implement running_sum: compute a list where  #\n\
         # each element is the sum of all elements up   #\n\
         # to and including that position.              #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   running_sum([1, 2, 3]) == [1, 3, 6]        #\n\
         #   running_sum([5]) == [5]                    #\n\
         #   running_sum([]) == []                      #\n\
         #                                              #\n\
         # Available functions:                         #\n\
         #   fold_left(list, fn, init) -> result        #\n\
         #     fn takes (accumulator, element)          #\n\
         #   append(list1, list2) -> list               #\n\
         #   rev(list) -> list                          #\n\
         #   map(list, fn) -> list                      #\n\
         #   length(list) -> Int                        #\n\
         #                                              #\n\
         # Syntax reminders:                            #\n\
         #   Tuple: (a, b) = ...                        #\n\
         #   Tuple access via pattern: let (x, y) = t   #\n\
         #   List cons: x::xs, List literal: [1, 2, 3]  #\n\
         #                                              #\n\
         # Tip: You may need to track both the running  #\n\
         # total and the result list in your fold.      #\n\n\
         let running_sum = fun nums ->\n\
         ?\n\
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
