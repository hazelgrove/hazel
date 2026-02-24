let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / mentions / mentions-sketch",
    {
      segment =
        "((Secondary((id \
         28266734-8051-45f0-aa82-3efafbc98e3f)(content(Comment\"# MENTION \
         EXTRACTOR TASK                        #\"))))(Secondary((id \
         c4e2c8be-3089-4fe8-8117-f8981cd4be09)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5fe07f9-0ebb-447d-beab-7c57c9eae097)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         47764073-e595-4585-a236-4ac2404f6714)(content(Whitespace\"\\n\"))))(Secondary((id \
         b65fb36b-f76c-4d47-9a56-c6f49bde65f6)(content(Comment\"# Extract \
         @mentions from a message string.      #\"))))(Secondary((id \
         ae3d579b-38bb-4574-a27c-b180e421b6fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd54459b-5a49-410b-9a56-46d7a35dbdde)(content(Comment\"# Given \
         \\\"Hey @alice and @bob\\\", return           #\"))))(Secondary((id \
         dc4d7f67-6687-468f-b220-78910a82f23a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a9719da-7e42-440d-977d-6a0755ae6c6d)(content(Comment\"# \
         [\\\"alice\\\", \\\"bob\\\"].                             \
         #\"))))(Secondary((id \
         b9af584d-1c3f-4703-8464-5af316c73995)(content(Whitespace\"\\n\"))))(Secondary((id \
         216abb2e-d02d-45e7-9576-97e42698e6af)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         be568872-7dd1-41b5-9709-034bd3fdbcd3)(content(Whitespace\"\\n\"))))(Secondary((id \
         407634a9-fb31-4a34-bc3b-a151c7a55bce)(content(Comment\"# \
         Steps:                                        #\"))))(Secondary((id \
         f6caa3c0-7e06-4d49-a7dc-4c6d87ca78ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ed753dd-354c-4ea4-8106-ec18e0ef8578)(content(Comment\"#   1. Split \
         message into words                 #\"))))(Secondary((id \
         26703cb9-05cd-43c1-a676-39cab961a96a)(content(Whitespace\"\\n\"))))(Secondary((id \
         7898d7b4-150a-41c9-9d97-1cc5b0c10afe)(content(Comment\"#   2. Keep \
         only words starting with @          #\"))))(Secondary((id \
         a6d5250d-4ac2-4ed6-85db-d75d51bedc4f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7cfb8ef-7aa0-438b-9788-4192cc191995)(content(Comment\"#   3. Remove \
         the @ from each                   #\"))))(Secondary((id \
         5a8fd8af-3eaa-4d9c-8132-f49d22661432)(content(Whitespace\"\\n\"))))(Secondary((id \
         b66a48f6-2a04-4d0a-be07-2fe7194d82d1)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         a506223b-cbd5-44d9-a213-68f7f4b53d16)(content(Whitespace\"\\n\"))))(Secondary((id \
         80242f39-1ebb-4b9e-a149-5a81d882a14f)(content(Comment\"# Available \
         functions:                          #\"))))(Secondary((id \
         bf722839-7d4e-4e67-b3df-8b4edbe14d7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         f07325a6-1f45-4858-bb94-4594475e4ac3)(content(Comment\"#   \
         string_split(sep, str) -> [String]          #\"))))(Secondary((id \
         9974a1d7-b1bd-47b4-be6d-7accbe553403)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6e620fd-3f06-46c9-99f3-54c51cac529a)(content(Comment\"#   \
         string_sub(str, start, length) -> String    #\"))))(Secondary((id \
         4106b146-0544-4a69-a8f7-186d6b790ea2)(content(Whitespace\"\\n\"))))(Secondary((id \
         74c1c842-5441-4222-8fbb-799ce568d978)(content(Comment\"#   \
         string_length(str) -> Int                   #\"))))(Secondary((id \
         d7983844-2d3e-47af-906d-5b86405ebe8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         a572da67-9ec2-4660-b87c-4a88eb2d7b7b)(content(Comment\"#   \
         filter(list, predicate) -> list             #\"))))(Secondary((id \
         e612034b-7838-4843-a750-1cc588751a01)(content(Whitespace\"\\n\"))))(Secondary((id \
         5560a58e-c653-42cd-bde3-d381ca6cf3ea)(content(Comment\"#   map(list, \
         fn) -> list                       #\"))))(Secondary((id \
         8765f7a8-f02f-45da-b506-e4f4cabab590)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6724f89-677d-4b9f-9856-4554a86c0233)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         69e5cfef-0677-43a8-a1d0-2a66acde48d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         c587b6d4-bc5a-41cb-a561-4e0623dc8404)(content(Comment\"# Syntax \
         reminder:                              #\"))))(Secondary((id \
         bd85cbc7-0239-4642-8925-508a506f2f73)(content(Whitespace\"\\n\"))))(Secondary((id \
         d10f97f0-49d0-4f25-b12a-f8affdc56b75)(content(Comment\"#   let name = \
         expr in body                     #\"))))(Secondary((id \
         36814c33-d9d4-4760-a8bf-7b2b4fa2ae96)(content(Whitespace\"\\n\"))))(Secondary((id \
         da5dc7e1-8469-4494-8f43-8117cbe07f72)(content(Comment\"#   fun x -> \
         body                               #\"))))(Secondary((id \
         763f5772-62bb-4dc3-927e-4a46b3c89e1e)(content(Whitespace\"\\n\"))))(Secondary((id \
         3f61ba46-2f90-4856-a0f1-b22e59d6fb09)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         8acd5098-d782-4cd4-9caa-3c713457abef)(content(Whitespace\"\\n\"))))(Secondary((id \
         815c0c33-1f6b-4de5-b23c-bf0abdb7a285)(content(Comment\"# Tip: Build \
         incrementally! Write one step,    #\"))))(Secondary((id \
         a6276536-cc9d-4f50-ae30-5fa53f3460b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d00e7ec-2f13-4bbd-9cef-6f4ec6b855e6)(content(Comment\"# check the \
         probe output, then add the next.   #\"))))(Secondary((id \
         f3ec0cba-22a6-41bd-8569-94d2e86a9df6)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d70a329-1ff7-4068-bf68-7fa7653b2c05)(content(Whitespace\"\\n\"))))(Secondary((id \
         686fcb96-1e24-4715-b5b1-c2aa21a6e8d3)(content(Comment\"# Helper: \
         check if a word starts with @ #\"))))(Secondary((id \
         ec4676ea-b5d4-43c7-b614-757af5801ebe)(content(Whitespace\"\\n\"))))(Tile((id \
         c6a269a1-bbf0-4885-9cd4-cc6ace8e51ba)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         07320e68-0cbe-4a03-9937-ffa365e4a1c3)(content(Whitespace\" \
         \"))))(Tile((id \
         10e5600a-7e01-4619-aef3-1e1ac775a429)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6243b4c5-06b9-45fb-99bd-0248431d1a83)(content(Whitespace\" \
         \")))))((Secondary((id \
         91fa489a-207f-4bd1-a173-edc47e0be839)(content(Whitespace\" \
         \"))))(Tile((id e81622b3-057c-4769-9824-29266c02393d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         064793c4-44b3-4754-8e97-16bba8e60e45)(content(Whitespace\" \
         \"))))(Tile((id \
         11bd2570-2c6a-4ec2-8c32-2eb5015e2b24)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d937d86c-e569-4b7f-ac06-a4b297347312)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         13b99ccb-3f10-4e5f-b469-2c0ba6c9610f)(content(Whitespace\"\\n\"))))(Tile((id \
         6d9d2dab-010a-4419-8170-f42fed21b8d4)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e403b253-6027-41a4-8397-66eed1619b7a)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1972fdaa-fcf0-4b8d-9077-21727b22bac8)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb493309-b683-418e-8de9-f33567e44ddd)(content(Whitespace\"\\n\"))))(Secondary((id \
         32cd38a4-0ec7-4866-aa5d-0b1c195608a3)(content(Comment\"# Helper: \
         remove the @ prefix from a word #\"))))(Secondary((id \
         21c0ca7c-5f78-473a-b1f7-fc3397b93d79)(content(Whitespace\"\\n\"))))(Tile((id \
         eafdccf0-7ecf-4a53-b83f-19c4fde07379)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d941f393-3ffb-429b-8472-628fe84e132b)(content(Whitespace\" \
         \"))))(Tile((id \
         c243aacd-6a19-44da-af8c-b16f63bd2ab4)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3c2390fe-a443-4cca-b712-6e7f00a4d7f5)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d78c654-ebb8-4261-a564-576b33b92f8c)(content(Whitespace\" \
         \"))))(Tile((id 56cfc01d-d4c9-4fa0-803d-d07a855377b0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         459a3356-b0ec-4e5c-acfd-ef1e9488946d)(content(Whitespace\" \
         \"))))(Tile((id \
         0f201477-f339-403b-a02b-c8c1f5ec5798)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bce18110-ee8d-44a0-81e0-48fbfd2aad87)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9de4f9fd-7dbe-4e84-bf02-b50f5ab5620a)(content(Whitespace\"\\n\"))))(Tile((id \
         28000709-1503-47dc-9bf0-932ba09982b2)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c63d21c2-bd11-4409-a426-b1fe02485aa6)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cb521ca7-10c2-43a1-b317-16901df11e98)(content(Whitespace\"\\n\"))))(Secondary((id \
         141bd0f4-bee8-4115-810c-be695907349f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4ed87d88-bc00-423d-8c22-5f5a615ce9c0)(content(Comment\"# Main \
         function: extract usernames from message #\"))))(Secondary((id \
         6c001836-5461-4b9c-99e0-4bc6bb9fc1c0)(content(Whitespace\"\\n\"))))(Tile((id \
         4a85c4d3-b374-469c-8bf5-36465ea57f20)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         044d1063-2882-4adf-8962-5c2df83df576)(content(Whitespace\" \
         \"))))(Tile((id \
         e85c0d63-d4a4-4048-a8a4-c2632ecc5b02)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e6d958b5-839c-4e57-8121-8000efe4fa59)(content(Whitespace\" \
         \")))))((Secondary((id \
         339707c4-f579-43e3-9146-34908f504d76)(content(Whitespace\" \
         \"))))(Tile((id ac375cb3-bcab-47f6-a91c-c7c8abd27865)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f6718b1d-acff-4796-8263-3ed5a32497b1)(content(Whitespace\" \
         \"))))(Tile((id \
         a2f4fe11-5b03-451c-9ff2-c494b8b90593)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9acbdb1d-a222-4298-9465-51dac44630b6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         51544e51-6dee-481d-96f2-d25907f17765)(content(Whitespace\"\\n\"))))(Tile((id \
         b3213c13-2de6-4eae-b912-d1d236b93bca)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         21c3a7a5-8203-4abc-bcfa-21520a93caac)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8a793c61-81b5-4664-a5ad-fbd11a478d8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         721e6d7c-461f-40b8-8a80-4d72ead43a74)(content(Whitespace\"\\n\"))))(Tile((id \
         40dc5bc1-6408-4e07-84c1-0cb6b6e0f8b6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         eab4d50a-4b58-441c-8e0c-686dbc830e20)(content(Whitespace\"\\n\"))))(Tile((id \
         5cd0efaa-c9f3-4919-8f28-f8288e81bd9f)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         778315cd-75f1-43b9-8ae2-1c503a4d79ed)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         905cc0f1-3666-4182-aba4-43fead8e4f38)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3b0292a6-8304-49d8-8870-73350e8e133c)(content(Whitespace\"\\n\"))))(Tile((id \
         4dfebf93-984e-490e-ba0a-542c854b13d6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6e4abb4-ce1e-4dc0-9612-d96e0723959e)(content(Whitespace\" \
         \"))))(Tile((id 3b494140-31f6-4c39-b4aa-db2d5fd1c2e7)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b20b0205-5ccf-4aae-946e-a6509580be87)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5b32ba79-4776-4b12-9b16-8a116797a485)(content(Whitespace\"\\n\")))))))))(Tile((id \
         35dbcc9a-1506-4754-99c1-3332e8eb9c37)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8aefc56-bb97-4cb8-9083-039bd4b44140)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6268181-056e-45b4-9a05-4ed8ed8757d5)(content(Whitespace\"\\n\"))))(Tile((id \
         ce8a1278-9f07-4ab3-84f4-b20908af6450)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ff418f3e-c1c0-4c86-9289-d691347d2d3e)(content(Whitespace\"\\n\"))))(Tile((id \
         31eec38d-2259-4426-bf31-5dfa98c263af)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f7ae7e8-2b2f-49ca-8f82-82ef600cc2d1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e9256d11-8f02-46a5-b4c2-db4b1bb5caf5)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6c9f3226-905b-48e0-a9e0-b37c6874d622)(content(Whitespace\"\\n\"))))(Tile((id \
         7e5574d9-d094-4403-a33b-6461618a68bd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d25b3fe4-0df7-44fb-8948-04dac36453f5)(content(Whitespace\" \
         \"))))(Tile((id fa630b03-21b6-4a31-a51c-b8930ec0eed8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5c538323-fde1-44b1-a89e-25c502a6c902)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28b6b312-3a57-448a-bc70-3fe3b31059f1)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ba19555b-9598-463f-a010-352cfcbb5e9e)(content(Whitespace\" \
         \"))))(Tile((id \
         5d0870a3-04aa-466c-a778-372a559e416a)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         33178682-d0f7-4657-a3f7-a476d30e44ba)(content(Whitespace\"\\n\")))))))))(Tile((id \
         678c961d-3613-47ea-82a6-72e368d413e7)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         12876b11-a60c-428f-8ca1-57452a107d98)(content(Whitespace\"\\n\"))))(Secondary((id \
         168fdfe2-496a-4fd2-bafc-e5d59ac2086c)(content(Whitespace\"\\n\"))))(Tile((id \
         b86c51bc-826d-456b-9a4f-b8e3f799b9a4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5ce0c0ef-3c24-4c83-bf27-9c2cc2b18cb3)(content(Whitespace\"\\n\"))))(Tile((id \
         8bb79409-a1e0-4f32-a2c7-0ee2539bf5df)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         034bc5c7-aabc-441f-92ad-e4ea19e2820b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         13a54d28-1db2-486c-9ca1-749e5c94e991)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bedc1f28-f7df-4c98-8a5b-2d287547c54c)(content(Whitespace\"\\n\"))))(Tile((id \
         fa66a719-968a-4c03-aeac-f9485f856f89)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de84aebd-4415-43f8-9afc-e4017ddb88d9)(content(Whitespace\" \
         \"))))(Tile((id \
         689d70eb-b1e3-463d-a342-10e1fa9bd062)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2316500a-7b54-42ba-a75b-2c0e280ea026)(content(Whitespace\"\\n\")))))))))(Tile((id \
         2e1d2814-dc96-4c6a-800c-517c6035f217)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4697ccfd-efa4-4ec8-9c49-cf8608f05261)(content(Whitespace\"\\n\"))))(Secondary((id \
         6980ffc8-e4e2-425a-b8ec-0ddeafa441b5)(content(Whitespace\"\\n\"))))(Tile((id \
         fa40d5e3-4e4d-4812-84ab-1cffafdec947)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d9078ee8-c3af-478a-a026-f8cbbab41ee1)(content(Whitespace\"\\n\"))))(Tile((id \
         143394ec-7a59-49ff-8e1d-dd06d8b40413)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         47f35ad6-a938-47f9-8919-08ec3ce1fbb5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c3508bb8-5fd1-4970-9c4e-441657c469c6)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2ef95e3d-4ca3-4fe3-b88c-12d1e3f08f81)(content(Whitespace\"\\n\"))))(Tile((id \
         da9387d9-aeae-4621-84bb-f4d74fd968f6)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4bba02c-109f-42a8-b111-140f162c6708)(content(Whitespace\" \
         \"))))(Tile((id 131ff47b-95e1-493a-b6ce-71eeb1a24a3e)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8187791d-4cdf-467c-aee1-fa22b709d684)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d6087c50-49c6-43ce-b20c-f38f0a00dac0)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         43cba8a4-eeed-4b6e-8c79-078586b5cab7)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# MENTION EXTRACTOR TASK                        #\n\
         #                                               #\n\
         # Extract @mentions from a message string.      #\n\
         # Given \"Hey @alice and @bob\", return           #\n\
         # [\"alice\", \"bob\"].                             #\n\
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
         ?\n\
         in\n\n\
         # Helper: remove the @ prefix from a word #\n\
         let strip_at = fun word ->\n\
         ?\n\
         in\n\n\
         # Main function: extract usernames from message #\n\
         let extract_mentions = fun message ->\n\
         ?\n\
         in\n\n\
         test\n\
         extract_mentions(\"Hey @alice\")\n\
         == [\"alice\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@bob @carol hello\")\n\
         == [\"bob\", \"carol\"]\n\
         end;\n\n\
         test\n\
         extract_mentions(\"no mentions here\")\n\
         == []\n\
         end;\n\n\
         test\n\
         extract_mentions(\"@dan\")\n\
         == [\"dan\"]\n\
         end\n";
      refractors = "()";
    } )
