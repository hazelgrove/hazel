let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-sketch",
    {
      segment =
        "((Secondary((id \
         9dfe509c-5390-4be6-83e2-cb075473353a)(content(Comment\"# MENTION \
         EXTRACTOR TASK                        #\"))))(Secondary((id \
         b722ae2f-ab41-4919-8195-07ade58e2c14)(content(Whitespace\"\\n\"))))(Secondary((id \
         d82f59d4-2667-4985-a763-6e40c9a9a30d)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         27366345-3617-4bf0-8848-39935152c414)(content(Whitespace\"\\n\"))))(Secondary((id \
         71021e73-e47b-49e3-bed0-1c924a292337)(content(Comment\"# Extract \
         @mentions from a garden message.      #\"))))(Secondary((id \
         de667970-b345-4b10-8e96-e2da77dca725)(content(Whitespace\"\\n\"))))(Secondary((id \
         feb91cd8-c948-44cc-9f68-3bcea2c8fbad)(content(Comment\"# Given \
         \\\"Hey @luna the moonblooms are opening\\\", #\"))))(Secondary((id \
         133fa9d0-d96e-4331-8e9f-869df5c32731)(content(Whitespace\"\\n\"))))(Secondary((id \
         a0f11f0b-454c-4c96-b5de-76bc1cb09552)(content(Comment\"# return \
         [\\\"luna\\\"].                              #\"))))(Secondary((id \
         08aea4b0-8f6d-40d1-a7d7-5a1e818efd5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         040dc182-0bc3-455a-a1a6-c6b80d66c156)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         866e07d1-197b-41cc-a0e1-9098fa742cce)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b89c4b1-1158-4366-9f8c-e5b7d5abf9c1)(content(Comment\"# \
         Steps:                                        #\"))))(Secondary((id \
         7fe92391-1afc-4c37-9501-be2bade057bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         cded0e7e-688f-4d46-a51b-906f83cde9f3)(content(Comment\"#   1. Split \
         message into words                 #\"))))(Secondary((id \
         5ca0b717-89bf-4b08-93e1-27ca788ee8e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         74cb0c91-9be0-4123-b333-29f858c0017f)(content(Comment\"#   2. Keep \
         only words starting with @          #\"))))(Secondary((id \
         9285bb7a-b1fa-4800-b719-c469a768bed6)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb0b34c3-9526-40c8-91bd-2a58b12ffce5)(content(Comment\"#   3. Remove \
         the @ from each                   #\"))))(Secondary((id \
         b6ba8368-b6f1-48ae-ade8-58818cc70266)(content(Whitespace\"\\n\"))))(Secondary((id \
         83133103-2cdb-4299-9770-52d74bb331b6)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         94b408bd-769d-4dc2-a915-a13f0d9ed46f)(content(Whitespace\"\\n\"))))(Secondary((id \
         36d5260f-53ed-4524-bea6-da9249514174)(content(Comment\"# Available \
         functions:                          #\"))))(Secondary((id \
         dbb5ef3f-577c-416c-bddd-62627e3a294a)(content(Whitespace\"\\n\"))))(Secondary((id \
         d73631f7-8ee4-4cc3-90e8-dd37e9f2596f)(content(Comment\"#   \
         string_split(sep, str) -> [String]          #\"))))(Secondary((id \
         f5525174-de97-4d05-a22d-41df661b7cbe)(content(Whitespace\"\\n\"))))(Secondary((id \
         1cb8ade5-4783-419f-9ab6-a9a3f0578011)(content(Comment\"#   \
         string_sub(str, start, length) -> String    #\"))))(Secondary((id \
         6ecf2a67-48fe-490d-91ee-a84f9fedd551)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c5e90c4-2844-4309-9f65-ac1a3e1d41e4)(content(Comment\"#   \
         string_length(str) -> Int                   #\"))))(Secondary((id \
         aedacd59-54e3-4bb4-848d-a165adf40ea2)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b1a3de8-50d0-4ee1-8439-3d9b05c2f126)(content(Comment\"#   \
         filter(list, predicate) -> list             #\"))))(Secondary((id \
         22afb72e-76a5-43fc-a076-7f723a2c6d4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         44a6dbed-0f29-457e-bc8d-3232adf84e95)(content(Comment\"#   map(list, \
         fn) -> list                       #\"))))(Secondary((id \
         ce57cf5b-7b96-496d-837b-36754ddd117c)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf1a19e8-dda2-40c4-a992-1389975d0741)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         321ddf88-8452-4f4a-8c6a-ad745fa561f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         b35734ce-bf03-4250-b065-c911645b2164)(content(Comment\"# Syntax \
         reminder:                              #\"))))(Secondary((id \
         8f43002e-3c9f-4f8a-849f-8eccd46a0ae6)(content(Whitespace\"\\n\"))))(Secondary((id \
         6042e88d-cba6-41aa-922d-8bac1b4278db)(content(Comment\"#   let name = \
         expr in body                     #\"))))(Secondary((id \
         42e7d029-a209-465e-9127-8482412d16e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c6d4548-af33-41b5-88bd-e81ebf46c266)(content(Comment\"#   fun x -> \
         body                               #\"))))(Secondary((id \
         cc5af19b-da7c-4712-92e1-7d6aa10edcb1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f6ff7cf-0021-4644-a02c-28efd03ccb6a)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         5b9c8646-8423-4c34-92fd-c528396f70dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         2721af64-b776-47ca-a698-7ea7337f8ce4)(content(Comment\"# Tip: Build \
         incrementally! Write one step,    #\"))))(Secondary((id \
         5bca29ca-f8f1-45a0-b9b9-0b44f3ba03df)(content(Whitespace\"\\n\"))))(Secondary((id \
         666f5c0d-e946-498d-be1f-3d6ca73f5ecb)(content(Comment\"# check the \
         probe output, then add the next.   #\"))))(Secondary((id \
         f397e8c2-1d5e-445a-8266-0b510641c6da)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ab5613e-07bc-4d88-8be8-18039e5df851)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbf5ffc2-4701-44ef-8e31-18466ff5328c)(content(Comment\"# Helper: \
         check if a word starts with @ #\"))))(Secondary((id \
         5b5ad1d6-e665-48d9-b681-65fede58f157)(content(Whitespace\"\\n\"))))(Tile((id \
         f04e28ce-cf7f-47c8-af1a-9c51d675047e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ea0577d0-fcbc-4ec4-ba55-68b0e261ec7e)(content(Whitespace\" \
         \"))))(Tile((id \
         68de3a21-942d-41af-ad10-afb3c9ab2aff)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         299874ca-8323-43d8-8a9b-2aa23c9fdb1c)(content(Whitespace\" \
         \")))))((Secondary((id \
         c8ce6a1d-8c18-48d4-9620-0cfe8d6b5be4)(content(Whitespace\" \
         \"))))(Tile((id 8c17cee5-1a11-4fc7-967b-831581f4963e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         191c3748-88d6-4d9b-935b-643dbe9ec24d)(content(Whitespace\" \
         \"))))(Tile((id \
         42222d85-551f-4197-84b0-01a8ef5650fe)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         83f7346a-f0e5-4d18-8c66-68343d5c8af6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         bef67699-a7c4-4196-9111-f11f05054b5f)(content(Whitespace\"\\n\"))))(Tile((id \
         641fa464-112d-4909-a7e0-036e59308ce0)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f655dad9-c772-47c2-9c71-16a701392833)(content(Whitespace\"\\n\"))))(Secondary((id \
         9842c4a7-b3a0-4e86-a3af-a866cfff12e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         344a1af2-2dde-4ade-b76d-46b1e206f78f)(content(Whitespace\"\\n\"))))(Secondary((id \
         6941bdd8-3bcf-4135-922e-bcdcb5edad69)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         085c012e-1afa-4a25-8349-4629645231f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         014be51e-af47-4ef0-abca-85ca7c4ccbc5)(content(Whitespace\"\\n\"))))(Secondary((id \
         b18ada43-d35a-47f2-8b41-50f09aa2d576)(content(Comment\"# Helper: \
         remove the @ prefix from a word #\"))))(Secondary((id \
         5dc65de7-efca-47bb-81bb-9cff6fe04667)(content(Whitespace\"\\n\"))))(Tile((id \
         940685d7-4125-44ef-8f11-5d330feeac76)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2a06d7c5-68eb-4d79-9fd3-06c2e9f18ead)(content(Whitespace\" \
         \"))))(Tile((id \
         66fa44cf-e9eb-4e18-ba04-ef458c618917)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0b928630-0e8e-46e0-927c-a97290275e8b)(content(Whitespace\" \
         \")))))((Secondary((id \
         6c73155c-7f47-4a1e-bf6f-ba7e9b9d6492)(content(Whitespace\" \
         \"))))(Tile((id 9b95deb8-5c30-488c-aa18-0fb2ff051d2e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         48154e27-39b2-4943-a6d2-bd551a02f1f3)(content(Whitespace\" \
         \"))))(Tile((id \
         f19ecccc-cb07-4ffa-b1a1-382c87688822)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         83d6214f-c858-438f-81ad-606e3535a9b5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         01446903-269d-4b64-b2ca-dcd68fe981f7)(content(Whitespace\"\\n\"))))(Tile((id \
         889b85e1-70da-4272-947f-e9c977054371)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         554ac013-fd65-4186-aefa-e568e3ec3b4f)(content(Whitespace\"\\n\"))))(Secondary((id \
         c4e59330-82da-4aef-816e-ee242fa4dd9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         4c896947-a5d5-451c-bef5-17f139a8b891)(content(Whitespace\"\\n\"))))(Secondary((id \
         93418535-226f-4f0d-a10b-ff0fa322fbac)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8fe1fdae-6f6f-4b08-b728-be521732250f)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d4204e7-d5b9-408f-b883-5537cfbc6347)(content(Whitespace\"\\n\"))))(Secondary((id \
         918f26ec-9e00-4257-b4a8-6547cf03fbf7)(content(Comment\"# Main \
         function: extract mentions from message #\"))))(Secondary((id \
         b643f079-470c-4ea2-9071-b8dbb81c58cd)(content(Whitespace\"\\n\"))))(Tile((id \
         6616502b-0115-4994-b518-a148bf58dbb2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e3ad77c7-07b5-424c-a8d8-41eab5f80580)(content(Whitespace\" \
         \"))))(Tile((id \
         49a1bcb4-a9f0-43f2-896f-d8c015cf1d8b)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8afa19a9-46d7-48a5-af15-8cdff6fa4102)(content(Whitespace\" \
         \")))))((Secondary((id \
         900ac434-b16d-4465-8fd7-7c0dae50c3af)(content(Whitespace\" \
         \"))))(Tile((id 96fa5796-7d63-4a7b-bfd4-65d6bdba1547)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         12f34173-d0fd-440b-9d90-b986a02465cd)(content(Whitespace\" \
         \"))))(Tile((id \
         80d4c1fe-6ec0-45f0-b372-ed2ad383d3f0)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         147768a9-ce0a-492d-a65f-97495227d9e7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c818c4ad-0b18-4d5f-9989-db790f0320a1)(content(Whitespace\"\\n\"))))(Tile((id \
         1056b746-98dd-4d03-b0f3-0bc5b65901c3)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d32ac731-289e-40e5-9f7d-53fc7f4eab41)(content(Whitespace\"\\n\"))))(Secondary((id \
         46b501e6-7dc9-4521-9bb0-9c6ce13a3adc)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2e77e8f-73be-465d-ae29-45d7f62f6c2d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d823b5d-9977-41a4-9f00-8f4d0e720b64)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4cbdf8dd-ef30-418d-b584-c98593e10bea)(content(Whitespace\"\\n\"))))(Secondary((id \
         3889a73f-49d5-495b-b164-6b3bd7b105a5)(content(Whitespace\"\\n\"))))(Tile((id \
         f900953a-35eb-4c65-a211-1ab6dad9485f)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1fe706e6-64ba-460a-b69e-533fa54ba95d)(content(Whitespace\"\\n\"))))(Tile((id \
         bb34eb65-8778-493c-8d89-0149d8a74db9)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         efff736f-d670-4098-8f63-2c5f7352f013)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b9eef1b8-a9f4-4df3-b9ef-bf0683aeb47d)(label(\"\\\"Hey @luna the \
         moonblooms are opening\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         538125b6-4a94-46ce-a453-2ad40acb57cf)(content(Whitespace\"\\n\"))))(Tile((id \
         7199edf2-04f9-46c8-9e49-c665b88153e0)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5f4c74f1-b563-4ae7-bc0c-b28055586fba)(content(Whitespace\" \
         \"))))(Tile((id 576909b2-d06b-4445-987b-f31dbb8b1eee)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         503921ba-28b3-4498-ac0b-4436d3fab8e2)(label(\"\\\"luna\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a1a5aea3-7755-4cf0-b843-f4d41a7a8a22)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4d95451a-334b-449a-a34c-d9a4e34ce550)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0a8c9668-aeb5-4c5b-b8df-a8915d725c40)(content(Whitespace\"\\n\"))))(Secondary((id \
         63d7c246-9d4a-4cfe-ae2e-9a20086289a1)(content(Whitespace\"\\n\"))))(Tile((id \
         2580cbd5-0a59-4d67-b74e-ae2c0eafab49)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         1217e5b3-d9a7-43d1-a600-247631cc8e11)(content(Whitespace\"\\n\"))))(Tile((id \
         f85faa04-256f-44f4-926c-caee12df6150)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c6d419f5-b911-440d-ad2d-d1202372dd4d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ba2c8de7-fe68-4d47-8971-f50f06c7d7f9)(label(\"\\\"@thorn @moss check \
         the greenhouse\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
         Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         63158cea-4cbd-46d8-8c17-22255ee76018)(content(Whitespace\"\\n\"))))(Tile((id \
         c6ea7647-44bd-4979-9f1b-367541ad44e5)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9acbb599-a111-47d7-a050-13bb08de1d71)(content(Whitespace\" \
         \"))))(Tile((id bd1489f4-1616-413d-9447-7a68cf955e64)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5b8161cf-7fd4-43f4-96a9-2e635ab4d30c)(label(\"\\\"thorn\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4334dd77-4445-4383-8084-97bc6aeef5cb)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         387680c2-6b5d-4407-891e-532a99168fab)(content(Whitespace\" \
         \"))))(Tile((id \
         99ca752b-7816-479b-940d-aecdce2ce815)(label(\"\\\"moss\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         89fdae05-1dc6-4ecf-8d63-d7148fc60bda)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e95cbe72-34b7-4cb9-9e5a-0e0e529f723c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         044c1bdb-a4b1-4291-985c-9b5eb53e4872)(content(Whitespace\"\\n\"))))(Secondary((id \
         517c95ac-d3d6-4a8c-89e4-8ee8e8044728)(content(Whitespace\"\\n\"))))(Tile((id \
         d4749102-a177-4787-b185-e59e694a4fc5)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9b348c28-7295-42f4-9f90-43c61bd9fa4c)(content(Whitespace\"\\n\"))))(Tile((id \
         a2442663-95fb-4af9-96c9-dadaf9ddd8eb)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29fc1b2c-2f59-4fff-89d4-9dcf36f6fab9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         210cd473-e568-49a8-b039-07970544565a)(label(\"\\\"the night air is \
         still\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4b6387fa-b627-4a46-829b-cc9868b3870d)(content(Whitespace\"\\n\"))))(Tile((id \
         373d4f3c-8694-446c-b78b-61bacf2afcac)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6d2a9c7a-0927-4976-b27c-881ddb6554f4)(content(Whitespace\" \
         \"))))(Tile((id \
         619e54ac-9a91-4fb9-b18e-f8dbc1481b02)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f8823217-a9f8-4285-a526-704802085389)(content(Whitespace\"\\n\")))))))))(Tile((id \
         43ef30cc-8000-4757-8abc-0341e679d0bd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74630ef1-adf1-402c-a1ff-6a232d64a7c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a60abcc-1a14-487f-b504-8f9a9bba144a)(content(Whitespace\"\\n\"))))(Tile((id \
         305cf984-8f0f-49de-b7c6-6d71981bd710)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9740aea7-ed97-4c0d-bebd-46d6c124eee0)(content(Whitespace\"\\n\"))))(Tile((id \
         59c161d3-8441-4fec-a1f4-f13a4e610e55)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         377a9a5f-4bef-4edb-bf9b-8da6c500a351)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a204fc03-c201-4deb-b54c-e85b4eebbfa6)(label(\"\\\"@fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         af2bdd92-e3db-4b8d-a961-839d5a6c517a)(content(Whitespace\"\\n\"))))(Tile((id \
         ecb7155d-0b67-49c4-9873-6e27fc9b3f01)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8f26b68e-cda2-4e5c-a12d-df06891e023e)(content(Whitespace\" \
         \"))))(Tile((id db6f8182-61c8-461f-96eb-f9de650e2bfe)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         34a130f7-3dc6-4792-aac1-0085e8905fda)(label(\"\\\"fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c71f8359-bf6b-4146-bf5d-d638932133bb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         53487829-716f-45a2-8b22-ca6ab3ad10c1)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MENTION EXTRACTOR TASK                        #\n\
         #                                               #\n\
         # Extract @mentions from a garden message.      #\n\
         # Given \"Hey @luna the moonblooms are opening\", #\n\
         # return [\"luna\"].                              #\n\
         #                                               #\n\
         # Steps:                                        #\n\
         #   1. Split message into words                 #\n\
         #   2. Keep only words starting with @          #\n\
         #   3. Remove the @ from each                   #\n\
         #                                               #\n\
         # Available functions:                          #\n\
         #   string_split(sep, str) -> [String]          #\n\
         #   string_sub(str, start, length) -> String    #\n\
         #   string_length(str) -> Int                   #\n\
         #   filter(list, predicate) -> list             #\n\
         #   map(list, fn) -> list                       #\n\
         #                                               #\n\
         # Syntax reminder:                              #\n\
         #   let name = expr in body                     #\n\
         #   fun x -> body                               #\n\
         #                                               #\n\
         # Tip: Build incrementally! Write one step,    #\n\
         # check the probe output, then add the next.   #\n\n\
         # Helper: check if a word starts with @ #\n\
         let starts_with_at = fun word ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Helper: remove the @ prefix from a word #\n\
         let strip_at = fun word ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Main function: extract mentions from message #\n\
         let extract_mentions = fun message ->\n\
         ?\n\n\n\n\
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
