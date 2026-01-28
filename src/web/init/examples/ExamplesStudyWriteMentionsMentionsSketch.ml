let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / mentions / mentions-sketch",
    {
      segment =
        "((Secondary((id \
         0f54d483-2395-4986-9cc8-6f748cb25c1e)(content(Comment\"# MENTION \
         EXTRACTOR TASK                        #\"))))(Secondary((id \
         5851e6b8-9ddd-447e-b451-a0b755228896)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee8c09b8-ef2b-44d5-bccb-f29d774e1837)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         2d1b9c81-9702-468f-99b2-8f5dbb6d3698)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f87a9b9-7680-4c23-af2e-6f933d9f5d8a)(content(Comment\"# Extract \
         @mentions from a message string.      #\"))))(Secondary((id \
         f51d909b-8961-4b42-a9e5-3a3042c87f59)(content(Whitespace\"\\n\"))))(Secondary((id \
         b76bb75e-1ef2-4729-9b93-24db1f4eb014)(content(Comment\"# Given \
         \\\"Hey @alice and @bob\\\", return           #\"))))(Secondary((id \
         57cb1fa7-21b1-4027-b6bd-bf6526685bd5)(content(Whitespace\"\\n\"))))(Secondary((id \
         07cac085-3143-42da-8610-c93487fa8343)(content(Comment\"# \
         [\\\"alice\\\", \\\"bob\\\"].                             \
         #\"))))(Secondary((id \
         69a6ea37-9c9b-406f-9f51-f4eaa22283e1)(content(Whitespace\"\\n\"))))(Secondary((id \
         f430e134-d653-4354-92e0-9827892c02c6)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         089ed2a1-e3d4-4baa-aae5-217a1517e958)(content(Whitespace\"\\n\"))))(Secondary((id \
         09bac513-32cb-4b3f-9bfb-d39171e4f5a9)(content(Comment\"# \
         Steps:                                        #\"))))(Secondary((id \
         f52f5d21-b7e9-44f2-bedc-178a8d7aa7b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ca9526d-c567-42d1-afa8-cc3118f19bfb)(content(Comment\"#   1. Split \
         message into words                 #\"))))(Secondary((id \
         4c0a52bd-88a4-4e07-b9d4-68ac62017946)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8a41edc-defe-4595-8f12-07f4450ed43e)(content(Comment\"#   2. Keep \
         only words starting with @          #\"))))(Secondary((id \
         fc643804-7ec7-408c-99e8-aed64edc0f63)(content(Whitespace\"\\n\"))))(Secondary((id \
         d5753e1c-d8a4-418b-aaf9-28aa0af6a3ec)(content(Comment\"#   3. Remove \
         the @ from each                   #\"))))(Secondary((id \
         90d3efe5-57c9-4060-a776-b023b609598b)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbba0d84-43e9-45e5-b22a-7910e77fcc13)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         8498b070-cccb-4d60-a320-824fadbad76d)(content(Whitespace\"\\n\"))))(Secondary((id \
         290b2a64-692c-41f1-aef4-958a086fcf24)(content(Comment\"# Available \
         functions:                          #\"))))(Secondary((id \
         f5833fb3-7bdb-488a-8125-d2b62721c826)(content(Whitespace\"\\n\"))))(Secondary((id \
         955570a6-d20d-42ee-88ac-b85bb77d8d9e)(content(Comment\"#   \
         string_split(sep, str) -> [String]          #\"))))(Secondary((id \
         985b5e1a-82bd-426a-ae0e-9ad216827e6f)(content(Whitespace\"\\n\"))))(Secondary((id \
         802bbe6a-1175-4e8a-b364-f1ad72e1d23c)(content(Comment\"#   \
         string_sub(str, start, length) -> String    #\"))))(Secondary((id \
         7944891f-fdeb-4913-bee8-52985806afcf)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd3d08d0-cac0-4288-bb86-572ab927a92b)(content(Comment\"#   \
         string_length(str) -> Int                   #\"))))(Secondary((id \
         ec53b054-2872-4263-868f-de73a08e1ec8)(content(Whitespace\"\\n\"))))(Secondary((id \
         705bbbaf-94f6-4976-92ed-253ff9abbbf2)(content(Comment\"#   \
         filter(list, predicate) -> list             #\"))))(Secondary((id \
         d604596e-5440-4874-8198-abf71ee229b6)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbe36f06-7f9e-47e0-9935-77a283c96963)(content(Comment\"#   map(list, \
         fn) -> list                       #\"))))(Secondary((id \
         d6fa86f2-5d32-4b9d-b3f8-db61f45491ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e1a8037-40bf-4831-9724-1f00bdfea43c)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         63587a7b-2d30-48ee-8ee3-3eaac33e65b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8ed2386-6bf4-4f24-b7ce-4e7b2462e345)(content(Comment\"# Syntax \
         reminder:                              #\"))))(Secondary((id \
         3b311904-d6d1-4ae8-aa3e-fa5b19dd2cce)(content(Whitespace\"\\n\"))))(Secondary((id \
         14d0bb9d-6cbe-4f08-9c8a-9bf5fcca402c)(content(Comment\"#   let name = \
         expr in body                     #\"))))(Secondary((id \
         2fa696b7-76c7-47bb-9399-9f41be1e0aa8)(content(Whitespace\"\\n\"))))(Secondary((id \
         0cb54b4d-c318-4716-ac04-62410ac1f848)(content(Comment\"#   fun x -> \
         body                               #\"))))(Secondary((id \
         6342833d-4cef-41ca-9293-a644e243e5b1)(content(Whitespace\"\\n\"))))(Secondary((id \
         63210129-7bbd-437b-938f-6a321e616700)(content(Comment\"#                                               \
         #\"))))(Secondary((id \
         ad711f2b-9dc9-4226-b36a-f170d90b2247)(content(Whitespace\"\\n\"))))(Secondary((id \
         b5a7b46a-ac78-49a2-a20e-800be5d9eb34)(content(Comment\"# Tip: Build \
         incrementally! Write one step,    #\"))))(Secondary((id \
         ae73418f-e3d6-4217-bc2f-a423cfec2647)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b1f0aa0-8eb4-41dc-9fd6-ebd7cc35aad6)(content(Comment\"# check the \
         probe output, then add the next.   #\"))))(Secondary((id \
         cfe24307-40f3-43b4-9d7a-8b0dce34bc4b)(content(Whitespace\"\\n\"))))(Secondary((id \
         49093065-c3b9-472e-a6e1-a1815d746b69)(content(Whitespace\"\\n\"))))(Secondary((id \
         0527a390-d6ad-4fed-940d-87b02b500899)(content(Comment\"# Helper: \
         check if a word starts with @ #\"))))(Secondary((id \
         5fea3892-bdc6-401d-95d9-e4999e5a412c)(content(Whitespace\"\\n\"))))(Tile((id \
         c4f4d23a-76fe-4465-85f1-1bd5c5a7206d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         fff05403-5c45-4ce6-9af9-12bd44794598)(content(Whitespace\" \
         \"))))(Tile((id \
         f1036181-8541-4bb9-8c19-77552d10f0d5)(label(starts_with_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         98d00acc-d5e0-4519-9b99-099c3a50b8c7)(content(Whitespace\" \
         \")))))((Secondary((id \
         9ca799a8-b2d0-4389-9de6-0b811bb82bcd)(content(Whitespace\" \
         \"))))(Tile((id 0e83cc52-dfeb-4fa9-ae5f-10112494d3ba)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ca31286d-79ac-4026-ad60-68f7bcaf3fb3)(content(Whitespace\" \
         \"))))(Tile((id \
         5db82f6a-041a-4044-b0a9-94a437e8a222)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d1490f05-896f-4055-b80f-f2ef7a1013b5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         80890143-a75e-4eeb-9921-23a975049a9f)(content(Whitespace\"\\n\"))))(Tile((id \
         1b766042-161b-41f5-bd1b-68da4802fff6)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6b8a7e0c-4ee7-4eff-9241-d3ff7b75af76)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c5a72291-7a7b-4228-8569-f526a2f117bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3939aab-5b68-441e-b255-ab6eec419e2f)(content(Whitespace\"\\n\"))))(Secondary((id \
         efcb9138-f92e-4830-b76e-4f1b06666852)(content(Comment\"# Helper: \
         remove the @ prefix from a word #\"))))(Secondary((id \
         a9953a0c-bb79-47ba-a878-8f1f0a348c8d)(content(Whitespace\"\\n\"))))(Tile((id \
         bf468ac9-bafa-4a71-9dea-20aa61b511d3)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         85b62c17-defb-4d2b-b663-99970a44c464)(content(Whitespace\" \
         \"))))(Tile((id \
         84993827-ea1d-4702-bbd8-c574b1ca72dc)(label(strip_at))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f54a19e4-503b-42ef-ab4e-133514d8f793)(content(Whitespace\" \
         \")))))((Secondary((id \
         3d26edee-ae6d-4bf9-8cf7-ba1b15338c4f)(content(Whitespace\" \
         \"))))(Tile((id e7bbc1b8-0018-45c3-a8cd-bb015a38d699)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         ac065b40-a60c-4802-8557-b0c929935182)(content(Whitespace\" \
         \"))))(Tile((id \
         1fabcd0e-33a9-4086-b75f-f63be255ac66)(label(word))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8574cfc2-18e2-4cf8-bcdb-bbe2989670a8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0b64060d-9ab7-44db-bc15-77f443c67a86)(content(Whitespace\"\\n\"))))(Tile((id \
         511ed978-520c-44f0-b512-30f0cc4266c3)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0d1e690b-d9ad-4288-b695-f6e7bc7a86ab)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         a0be62b0-1eda-4825-92e5-550ad4a55aa4)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f706d73-e722-479b-8668-4dc43cb9757f)(content(Whitespace\"\\n\"))))(Secondary((id \
         050d22c8-09cc-4f2c-907d-12ef0039cb51)(content(Comment\"# Main \
         function: extract usernames from message #\"))))(Secondary((id \
         e322f5b8-bd01-42c3-8a19-ad26564622d0)(content(Whitespace\"\\n\"))))(Tile((id \
         649485b2-4e20-4c51-b9e7-0e828dec40af)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e3fbc049-b348-4996-9401-685717a589c9)(content(Whitespace\" \
         \"))))(Tile((id \
         36554fcc-6cbd-48cc-9633-6e56f3d63f3d)(label(extract_mentions))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8bc36a25-d455-439e-9fef-137b9b51ef9b)(content(Whitespace\" \
         \")))))((Secondary((id \
         b6b115f7-cf6b-404b-b9b7-c5bd4445103b)(content(Whitespace\" \
         \"))))(Tile((id 345c7cf7-7d5c-499f-9804-b2b3dec94b5b)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5ad5d6f7-6f49-422b-a5b7-eeb48c017c92)(content(Whitespace\" \
         \"))))(Tile((id \
         2dd24bad-7b9c-4580-95f3-8b4f7c1bd2f7)(label(message))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c709a0ad-91ca-495e-9777-113cc40f8df7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c83250ff-12cf-432d-b360-a17da6a1a8ee)(content(Whitespace\"\\n\"))))(Tile((id \
         3812575e-d1ac-44dd-8a02-f34b56fbdf32)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         413a9ef9-f782-43ae-b862-0cdb011010c1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8acf4c43-5f51-4170-961e-4f3b21440efa)(content(Whitespace\"\\n\"))))(Secondary((id \
         00d5e51f-23e0-4d4f-9625-b13e6947795d)(content(Whitespace\"\\n\"))))(Tile((id \
         68c73088-eb15-4689-a69f-a502b1f1b7e5)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7cee0964-cad0-493e-93bf-e2bd117b10f2)(content(Whitespace\"\\n\"))))(Tile((id \
         00a20286-6ede-4a7b-b194-ec34290c8f1f)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         696b86fc-6cf8-4e9c-9c4a-3481c5fb5cf8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bbde002e-8c1c-4ab9-bca8-699e7151a9a9)(label(\"\\\"Hey \
         @alice\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f4bacccd-2d87-4532-a485-4bc8ac179f6f)(content(Whitespace\"\\n\"))))(Tile((id \
         7c02cb34-f349-45e4-8fb9-035cb2569ef8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f17946b0-8d31-4310-8010-928bfc100db4)(content(Whitespace\" \
         \"))))(Tile((id a63f1244-734d-4f15-946a-956086e20099)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         573b8a24-d3b7-47c3-b220-81c2108efdbc)(label(\"\\\"alice\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a6fb1365-442b-4dc8-b549-bc954b79807d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         bee03121-9218-48fb-82bc-ef2a2ea9def1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df909b60-8137-4fac-90d9-09833c355592)(content(Whitespace\"\\n\"))))(Secondary((id \
         559a5ef7-6407-454b-b316-71c8f7471b20)(content(Whitespace\"\\n\"))))(Tile((id \
         b6fa2ecf-1e36-4822-9f02-f3e0513ace56)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9fdc7d66-439f-47da-af45-930d75936776)(content(Whitespace\"\\n\"))))(Tile((id \
         0ffdb5d5-e976-4a4c-b548-dcaa848206ba)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         777532e0-b895-4852-9dc9-ad4b7c60b234)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f12882c3-a799-41f1-b444-6a740c671b5c)(label(\"\\\"@bob @carol \
         hello\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8d4322a8-6606-4195-a920-22c92d58a21c)(content(Whitespace\"\\n\"))))(Tile((id \
         6bba847b-f7cf-4b31-bfd3-d66171cf4881)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86126ce6-3740-4566-acb6-8786c887a4b7)(content(Whitespace\" \
         \"))))(Tile((id 17e58998-f97f-4778-8b6e-60e696383007)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         461bb05a-2dc3-4baa-a572-9a1515fc7beb)(label(\"\\\"bob\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fb219e4c-e12b-49e3-8152-9e3c2e4b5de3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         af345c69-ca46-44af-8cfe-ce2270232f4e)(content(Whitespace\" \
         \"))))(Tile((id \
         d13f688f-0988-45ee-a721-5393a495aa28)(label(\"\\\"carol\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         a002897e-fb1b-41d4-8d14-e010fa03b682)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c22c9d37-a979-4457-9b4a-521b74c78a1f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         30f4b6a7-179d-41d5-b6db-3d13b1020733)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8100454-b231-4059-a491-3c2eaff7578b)(content(Whitespace\"\\n\"))))(Tile((id \
         a893808c-4012-4eec-a761-e3c00d4ed645)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5d556285-9b18-4b2a-b8a1-8139fa016882)(content(Whitespace\"\\n\"))))(Tile((id \
         807bdb01-3ff8-4bac-812c-1df09263d39f)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f5ec527e-5cd1-47d6-a9c1-61585baa7d91)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b17007d4-1c70-4c65-a870-60eb14b90b5e)(label(\"\\\"no mentions \
         here\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7b9be081-7367-4ff3-bd7e-d41dd4bc6b2a)(content(Whitespace\"\\n\"))))(Tile((id \
         752c4a1e-ba19-493c-be63-30b5b12fcc68)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         795a1b64-86b9-4370-a57d-3b932b10ddc2)(content(Whitespace\" \
         \"))))(Tile((id \
         d0657faa-5a4a-4a6e-b360-9a698bb2d11f)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3263bcf2-86e8-49fa-b4e2-93b765aef978)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9ae76b54-633c-4b74-9a32-67a4720ab076)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b83f6aa8-e754-4300-9816-fdf78e2c086a)(content(Whitespace\"\\n\"))))(Secondary((id \
         b85cb768-6348-4737-99b4-7efe19dcbf50)(content(Whitespace\"\\n\"))))(Tile((id \
         bbfa74c5-0014-428c-95ab-ac7c9376b4a5)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         afdb00a6-5e5e-414d-9037-826324e1560b)(content(Whitespace\"\\n\"))))(Tile((id \
         1e7c0a7c-eff6-49e9-9c66-6c6888d56d92)(label(extract_mentions))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5751a00b-c9a5-4f0c-8bc6-d0c60fb9d4fe)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         746e1a97-9d03-40b8-895c-c5ca676f2dd8)(label(\"\\\"@dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7a1ad20e-e6c5-4e1c-8e94-a1d3b4823c51)(content(Whitespace\"\\n\"))))(Tile((id \
         1dd44a6e-7353-4117-a8d8-2f876cd9a009)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a9b54929-8f1b-4f45-ae3b-7aec604b4d0c)(content(Whitespace\" \
         \"))))(Tile((id 4ea4c14b-d277-4b11-a875-b040c1d745e8)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d7b12b09-3ce5-4734-a14f-3aa09814e64e)(label(\"\\\"dan\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e9e3e872-1b42-4659-8199-9cc1ac04ef41)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ee471281-b46b-4429-8fc5-45bf2ccfe8e2)(content(Whitespace\"\\n\")))))";
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
