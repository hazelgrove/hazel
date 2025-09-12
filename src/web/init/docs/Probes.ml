let out : string * Haz3lcore.PersistentZipper.t =
  ( "Probes",
    {
      zipper =
        "((root Exp)(selection((focus Left)(content())(mode \
         Normal)))(relatives((siblings(()((Secondary((id \
         cb2ee082-765a-4dc4-af90-936779f6ba68)(content(Comment\"#  \
         _____           _                #\"))))(Secondary((id \
         76a735c9-5109-464a-820c-28ff643f03f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         2157a9d5-e2d0-4623-af1d-58ff66230e4f)(content(Comment\"# |  __ \
         \\\\         | |               #\"))))(Secondary((id \
         95a12a4b-3a1e-42fe-a225-054ecb5ba5c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ecef163-0017-4eee-9d74-44a90b4d13e1)(content(Comment\"# | |__) | __ \
         ___ | |__   ___  ___  #\"))))(Secondary((id \
         50b8469b-02b9-4f35-9843-a1a12c6d6926)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d95e8a1-837f-4005-bff6-837afde9380f)(content(Comment\"# |  ___/ '__/ \
         _ \\\\| '_ \\\\ / _ \\\\/ __| #\"))))(Secondary((id \
         8e6ceed6-6451-4ad3-87bc-d80e37706a38)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ec607d0-3656-4c34-968c-e3113f118de6)(content(Comment\"# | |   | | | \
         (_) | |_) |  __/\\\\__ \\\\ #\"))))(Secondary((id \
         d68156f8-75b0-4279-9637-85b2c151ea53)(content(Whitespace\"\\n\"))))(Secondary((id \
         aee57be4-c7bd-4673-a45d-a65450fe3230)(content(Comment\"# |_|   |_|  \
         \\\\___/|_.__/ \\\\___||___/ #\"))))(Secondary((id \
         f7dbc611-bb83-4a43-a087-281e9330f6fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         b27e459e-4f51-486b-af8e-caae64ad274b)(content(Comment\"# INLINE EVAL \
         WITH PROBE PROJECTORS #\"))))(Secondary((id \
         b65aa319-6ffc-4070-a248-556b8be9da60)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3f9951f-a9eb-4822-bc4a-50364feb23ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         d98141a9-1e3c-4ffa-bb3d-ed3b2a4e6af9)(content(Comment\"# INTRODUCTION \
         #\"))))(Secondary((id \
         79e1e9a8-64dd-47e0-8522-e36c0461c7db)(content(Whitespace\"\\n\"))))(Secondary((id \
         11e0acae-c3bd-4d84-9f49-7768bd88ad18)(content(Whitespace\"\\n\"))))(Secondary((id \
         698842ef-53df-4817-b87c-84d391729cb8)(content(Comment\"# Probe \
         projectors are a kind of inline evaluation, #\"))))(Secondary((id \
         4548b41f-eb52-4520-81fd-1da977b1a7bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         06904753-1702-4cd6-82bb-45c5a8efddaf)(content(Comment\"# similar to \
         value hints in Emacs or IntelliJ. #\"))))(Secondary((id \
         0ba20dea-e7c8-4177-b75a-448f0c5dcd48)(content(Whitespace\"\\n\"))))(Secondary((id \
         86077495-db05-424b-83b9-9c8d3db695d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         94da791e-b541-4618-9048-6e6e7b2ac4ad)(content(Comment\"# You can put \
         these on any expression or variable binding to #\"))))(Secondary((id \
         f2bf66e1-f107-4e15-9351-31e8b34327f7)(content(Whitespace\"\\n\"))))(Secondary((id \
         82768d04-9028-4f39-af76-1592f2397046)(content(Comment\"# see a list \
         of all values taken on by that expression/pattern. \
         #\"))))(Secondary((id \
         19eea45a-78a3-4b5e-a34c-b1e6af00b5fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         b3dc9838-f8f8-4cf6-be7b-59fda5fe6a29)(content(Comment\"# By default \
         values are sorted by left-to-right by most-recent. \
         #\"))))(Secondary((id \
         cf109593-cca0-4c05-8c7f-42115f16bdb4)(content(Whitespace\"\\n\"))))(Secondary((id \
         5ba652fa-bfad-4c23-9aba-8a47dd042b0e)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3163bd9-c1b0-41e8-88e9-430c7ed43114)(content(Comment\"# More \
         generally, each cell represents a stack state, #\"))))(Secondary((id \
         ab7e596d-feb0-4d20-ab45-94cc10b1d47a)(content(Whitespace\"\\n\"))))(Secondary((id \
         a18be61b-18c0-4f91-aa39-fe574199c4e3)(content(Comment\"# including \
         the top stack frame / closure and hence the #\"))))(Secondary((id \
         75a62372-452c-4b31-81ea-f0e7ffe0a4a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         1c9920bf-e116-4f35-b0b4-f4ecb1b91ed6)(content(Comment\"# expression's \
         value, the values of environment variables, #\"))))(Secondary((id \
         914e4a28-7f02-411f-b3eb-4080783ffd1f)(content(Whitespace\"\\n\"))))(Secondary((id \
         57613548-32f7-4d84-93d3-4d2ebb1d421c)(content(Comment\"# as well as \
         the surrounding call stack context. #\"))))(Secondary((id \
         b318f686-d9d4-458c-99a2-05580c74a005)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce953579-90a3-4aa2-8055-e51b5ae9f27d)(content(Whitespace\"\\n\"))))(Secondary((id \
         e7bde322-3ac4-4593-85e6-36e68501adeb)(content(Comment\"# When a cell \
         is selected, you can hover over it to see #\"))))(Secondary((id \
         012372a4-f915-4ccf-bcaf-bb898f6a47a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         836e7a33-7e22-4c2e-b3eb-4503346983c9)(content(Comment\"# relevant \
         environment variables, and all /other/ cells #\"))))(Secondary((id \
         496b946e-3b66-4b36-85ff-f02467e08600)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f9b8413-598e-42b4-a36c-231e745fc474)(content(Comment\"# are \
         decorated according to their relative position in \
         #\"))))(Secondary((id \
         93ebf2ff-8986-4637-a558-2293b3af635d)(content(Whitespace\"\\n\"))))(Secondary((id \
         89709547-6b23-4707-9f81-09ea400542b6)(content(Comment\"# to the \
         selected cell. in the context #\"))))(Secondary((id \
         95c355ba-ebc7-4e96-8c54-8aacae3a8f9c)(content(Whitespace\"\\n\"))))(Secondary((id \
         8bb7caea-b426-4319-b539-f11e38a6141d)(content(Whitespace\"\\n\"))))(Secondary((id \
         5605e1e1-f3a2-4245-8f34-be2c472f4ab1)(content(Comment\"# Probe are \
         intended mostly as a println replacement #\"))))(Secondary((id \
         7f8306c0-5f01-42e7-a9f9-a325472e1e44)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec887286-51ed-48ea-bdab-f809bd0e4a25)(content(Comment\"# for exposing \
         intermediate values, with the above decorations #\"))))(Secondary((id \
         a0e7c708-8ad6-48b8-9448-685efe05faf6)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ebdaaea-bc20-438f-b56c-989c81f7ec56)(content(Comment\"# as a \
         supporting feature to help maintain context when \
         #\"))))(Secondary((id \
         a8aebd77-d6a8-4d33-a145-3506f73e50f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         007f5be9-170d-494b-8f43-491721cbbf04)(content(Comment\"# navigating \
         between multiple probed expressions, which #\"))))(Secondary((id \
         01d17ac5-1251-41d1-be9c-22c570babe16)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a64508f-9d21-49bd-8cea-95490277fb43)(content(Comment\"# may take on \
         many values across nested or recursive functions. \
         #\"))))(Secondary((id \
         0711b721-d2bd-4588-8c1e-3d4322c0b042)(content(Whitespace\"\\n\"))))(Secondary((id \
         3cc5c96c-bc09-489f-bd23-829f819b45e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ec5c961-35d3-40ad-8cfe-3bff639f6b6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         b1121688-76e4-4c5f-9c65-1bb9d6679748)(content(Comment\"# TUTORIAL \
         #\"))))(Secondary((id \
         245b7b15-9422-49f4-8f3e-a300fc02281d)(content(Whitespace\"\\n\"))))(Secondary((id \
         4418ce94-f3e0-4380-947d-3b782d04362d)(content(Whitespace\"\\n\"))))(Secondary((id \
         c16f1586-ffd2-442f-8aaf-44fd99d024af)(content(Comment\"# The \
         expression (10 * 10) below has a probe.  #\"))))(Secondary((id \
         72090698-8a2d-4311-bc31-4d8cc95b14ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         dfcff693-2268-4f12-985a-402bcd56950c)(content(Comment\"# Its value, \
         20, is shown in a cell to the right. #\"))))(Secondary((id \
         82aa02b2-918c-473d-999b-23d59ae09e1c)(content(Whitespace\"\\n\"))))(Tile((id \
         222146f7-08b7-461d-8642-b3d6350870f9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b4145506-16da-48da-97ba-08c3fa5edd7b)(content(Whitespace\" \
         \"))))(Tile((id \
         c5aca7b8-2ac2-4ad7-a9f6-b96ff585f08b)(label(chips))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3996e033-1efd-4bc3-803e-27f41a698310)(content(Whitespace\" \
         \")))))((Secondary((id \
         465e8bbb-22af-4b37-ae61-900909b85b3e)(content(Whitespace\" \
         \"))))(Projector((id 0c75675d-f59a-4ad7-8c1e-8d44a4c041ad)(kind \
         Probe)(syntax(Tile((id \
         424de87d-6585-40e3-9ab5-9e601ef55819)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         749f3ec7-1e85-4ce4-9de6-d7a3c9cafb86)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ef6c156a-4adf-48a2-8b68-bdbc30777e98)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4a4e56d6-66e5-4b95-9012-e2e213d38982)(content(Whitespace\" \
         \"))))(Tile((id \
         725931d0-45ac-4316-826e-656c65ab58be)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1f38bbb4-87db-4eb0-9f93-99ab138ff474)(content(Whitespace\" \
         \"))))(Tile((id \
         cad9f261-ff20-4684-a14a-106a75464920)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         8adb5f90-0106-483c-8db7-16182b34df3b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         cb81aeef-4eef-4b52-b222-2137c525cd39)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b6b60c8-f2f0-45f1-a812-b2af2a47ee93)(content(Whitespace\"\\n\"))))(Secondary((id \
         a936be56-43bc-478e-96b0-f910060a2c68)(content(Comment\"# To probe the \
         below expression, put your caret to #\"))))(Secondary((id \
         9e84fc29-c46d-4099-9fd9-5fb0f1df817f)(content(Whitespace\"\\n\"))))(Secondary((id \
         f4c7809a-3409-463e-87b3-6f99939d4c59)(content(Comment\"# left of the \
         `(` and press option/alt-v (for value), #\"))))(Secondary((id \
         061fdaf5-5f8e-42ff-a28c-b79064681190)(content(Whitespace\"\\n\"))))(Secondary((id \
         56343da7-727a-42c0-8c9b-dc0bf473b6d0)(content(Comment\"# or select \
         `Probe` from the lower right corner menu: #\"))))(Secondary((id \
         f28417f5-f346-47e7-bb96-be9408005a5b)(content(Whitespace\"\\n\"))))(Tile((id \
         e7f5c246-e4cd-422f-8fa1-77b0ea912dc5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1068bd1b-4318-4954-8e0c-b4f76366aad2)(content(Whitespace\" \
         \"))))(Tile((id \
         4268ce54-6c85-437b-9b15-63122e6fea7c)(label(mult))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         55dc475f-6b9a-45a2-a6d1-3e7fc27baccb)(content(Whitespace\" \
         \")))))((Secondary((id \
         8126f4fc-8499-4d12-aba2-1bffbdcca6af)(content(Whitespace\" \
         \"))))(Tile((id \
         64596846-4f01-4672-aa7a-f4af654918ab)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3d3a76a5-a2b6-4b6b-85fa-3ee8a3968b56)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         08364b1d-039a-454d-9d97-ca9a55f37526)(content(Whitespace\" \
         \"))))(Tile((id \
         f0905aec-f787-4cf6-a5f7-cd101235be28)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69d83d7f-8c5c-4530-b97e-a6ebcda89fd7)(content(Whitespace\" \
         \"))))(Tile((id \
         51caee3e-221e-40db-ad65-e637a439247a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab662101-d421-4ed8-bd99-617975036d92)(content(Whitespace\" \
         \"))))(Tile((id \
         fdc01b2a-a2e4-47db-8810-7ba5a46289cd)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9f21dbb-5756-4722-9874-451aa3fc05c5)(content(Whitespace\" \
         \"))))(Tile((id \
         0ca3b65f-3b11-407a-b417-2e78e5438eaf)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c82a70fd-9ac0-4220-b2c1-64930270b062)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a065731a-02ea-4ca4-add0-ad1ab9c52ce5)(content(Whitespace\"\\n\"))))(Secondary((id \
         cfbf77f2-ba73-4a86-92ee-868da9cb56f6)(content(Comment\"# The \
         expression should be encased in a green block #\"))))(Secondary((id \
         d7e99542-6b3b-477b-8170-2cf78b9cb346)(content(Whitespace\"\\n\"))))(Secondary((id \
         09bb4321-093f-4569-ae57-6dc83592eb50)(content(Comment\"# and a cell \
         reading `7` should appear to the right. #\"))))(Secondary((id \
         5a908fc0-874d-43e5-a432-bc329fc96e50)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d3d68fb-c8fc-4dc8-a1e7-104710bf14ec)(content(Comment\"# The same \
         shortcut or menu toggle removes it. #\"))))(Secondary((id \
         7f735f2a-3bb6-4e07-829c-711ef33d8b74)(content(Whitespace\"\\n\"))))(Secondary((id \
         c5400404-997c-4fe9-8e70-02be988796e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         c56fe926-1575-4528-8832-fd3fc672da36)(content(Comment\"# Click the \
         below cell (with value 21) to select it. #\"))))(Secondary((id \
         9930c97b-a3ad-4c5a-8f67-4854370feeac)(content(Whitespace\"\\n\"))))(Tile((id \
         d8e57171-adad-423f-b408-53a9ff123648)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d5b0240e-3bde-49f5-8c32-946520660248)(content(Whitespace\" \
         \"))))(Tile((id \
         5aa6e946-03ed-4a3b-9d24-491cd8a5864b)(label(score))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6c5d5b02-44d2-41ac-a98e-05d00534c557)(content(Whitespace\" \
         \")))))((Secondary((id \
         628cdd6f-7884-40dd-bb49-890f12459719)(content(Whitespace\" \
         \"))))(Projector((id e4f8cb0c-d263-4f93-a5fe-d268d34ade3d)(kind \
         Probe)(syntax(Tile((id \
         e972eeab-da83-47d4-9973-f11ef797b360)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         58dc5a43-1ed4-4b8e-8e94-b3cbef7a633b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7986153c-3621-4dd7-815f-7a64d7ec5202)(label(chips))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8a91ffa7-f418-405e-88d8-7fe5ffd83a02)(content(Whitespace\" \
         \"))))(Tile((id \
         7ea1bf97-3230-4ed9-ab6e-bc99029c6663)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5a958849-1bd0-4a52-a6b0-8970ad1394ee)(content(Whitespace\" \
         \"))))(Tile((id \
         0a6bfd32-8250-4d83-bd61-f518e0c1b49c)(label(mult))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         8c61b923-d6c5-4ecd-ab47-00ac3c618898)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4083810d-e1ac-459b-a17c-ba648865af75)(content(Whitespace\"\\n\"))))(Secondary((id \
         be26e80f-09b6-4d4c-be9e-471a778b63a0)(content(Comment\"# Notice when \
         you hover over a selected cell, it #\"))))(Secondary((id \
         7074b3eb-cadd-4815-ae81-6d80cb1f13ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         e789f7c2-17cf-4669-9a35-6c2fa2a076e8)(content(Comment\"# shows the \
         values of any contained variables. #\"))))(Secondary((id \
         e2ffc36f-c495-4d67-a721-19f2fd34fe17)(content(Whitespace\"\\n\"))))(Secondary((id \
         56bef59c-ba28-4610-8409-f60abc956afb)(content(Whitespace\"\\n\"))))(Secondary((id \
         c505d761-7503-4b78-a964-739900e52030)(content(Comment\"# Probes only \
         have cells if the are evaluated. #\"))))(Secondary((id \
         e74a8d92-63e2-4b10-828d-cbad0c20fd4b)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a712db8-be66-4130-83c2-1c64924f98fd)(content(Comment\"# Below, only \
         the first case branch is evaluated. #\"))))(Secondary((id \
         6d30e434-e82b-4c6e-97ad-4d8f80d5ac48)(content(Whitespace\"\\n\"))))(Tile((id \
         a322e530-863c-46e1-9ff0-780a02e08962)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a4fbb3cf-9886-4b10-8d2e-97ef2a94a677)(content(Whitespace\" \
         \"))))(Tile((id \
         7cb44819-5b51-46d9-a0c4-e853ae2850bb)(label(check))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5c40c967-5a01-45b5-88f5-89ff3bf67bd4)(content(Whitespace\" \
         \")))))((Secondary((id \
         d15061c4-0776-47fe-9475-b14d2d395d9e)(content(Whitespace\" \
         \"))))(Tile((id e13fe2d7-3eb3-4c4b-9a3f-3ee86eff3f5c)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         067473b3-8a94-4982-a142-61d0706e7fd6)(content(Whitespace\" \
         \"))))(Projector((id b0e9bfcd-3e96-468e-8a61-6ea3fd457e91)(kind \
         Checkbox)(syntax(Tile((id \
         7db455c8-22a6-4f4d-830d-cc2cc024c574)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         aef7b358-b2b8-42dd-a2ea-3f3a339cf570)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         cf66949f-92c6-463e-b0ce-5f45cc8ca376)(content(Whitespace\"\\n\"))))(Tile((id \
         517d95ce-056d-439f-92fb-0615ce863054)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c817e3bd-d6ac-4ecc-9853-b04d7ae41f77)(content(Whitespace\" \
         \"))))(Tile((id \
         dffa8450-f604-4b3e-a839-b7a5f010b357)(label(false))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b11d2e5c-b410-454b-9e05-3008b0d7828a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         20c7f399-dd1d-4793-bdb5-abe4c85dc3ef)(content(Whitespace\" \
         \"))))(Projector((id 3fe43e9f-decb-41b4-9932-de0e1c31c9f9)(kind \
         Probe)(syntax(Tile((id \
         9e9c33f9-c36c-442e-980c-8db5d95625c6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f88f17e5-5cb4-4990-a2b3-4fe672be97a6)(label(\"\\\"checks \
         out\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         b58bd98d-f681-4c5f-bdaf-134d68d97d1a)(content(Whitespace\"\\n\"))))(Tile((id \
         0679ccb1-afd2-4519-bd00-a376f4041474)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a3f3472b-eadb-4e18-a48f-b988b15e8b73)(content(Whitespace\" \
         \"))))(Tile((id \
         9dcb9a2b-b52e-48a9-b23f-fe35e8ecfd84)(label(true))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c13ac757-5b90-4fca-a855-7dabb6320446)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         39c5c786-be2a-496e-b5cf-ddaec81ff6fc)(content(Whitespace\" \
         \"))))(Projector((id 717d81be-d850-4591-b9e5-6c1981be6430)(kind \
         Probe)(syntax(Tile((id \
         ca0eb86a-4f84-4428-97eb-ac4b6f21040e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         62dee821-0e00-4347-92bf-c6c24e2d502e)(label(\"\\\"you \
         cheated\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         6197f558-28da-4505-9dcf-f1e4ed4acd08)(content(Whitespace\" \
         \"))))(Secondary((id \
         b39eec47-7289-4655-9a45-f678d0958e3c)(content(Whitespace\" \
         \"))))(Secondary((id \
         74e124e1-03e0-4ef9-b2bc-3598e380e671)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         032785dd-88e1-410a-a1b2-fa8405bd4298)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         785d2242-b041-4da0-9127-01e5b42e11ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         32e8ca38-d278-44f2-b7a2-6b41477e2eeb)(content(Comment\"# Note the 2nd \
         branch probe has a zero to the right. #\"))))(Secondary((id \
         2a1afe79-9c1a-4961-81a2-affe24241c69)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b2fcb44-f219-44d3-ac4e-b05efd22f16f)(content(Comment\"# This is the \
         cell's collected closure count, i.e. #\"))))(Secondary((id \
         cca1a9fc-88ac-43d8-90c0-ff5fe6636259)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1eb24f2-cbfb-476f-8767-f14ab908c7f1)(content(Comment\"# the number \
         of times the expression was evaluated #\"))))(Secondary((id \
         142102fa-09de-47b5-86e8-9c45559f37bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0d5b78b-141c-4181-a9b9-af6a7cea96f0)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ec09049-d0f0-4c21-9fc8-0222f7fd4562)(content(Comment\"# Probes can \
         be placed on expressions: #\"))))(Secondary((id \
         45b0c140-0c21-4eff-8118-bbc1fbcd7ce9)(content(Whitespace\"\\n\"))))(Tile((id \
         f8811d72-7f1d-41ee-b4b7-d73ec536dffc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ecadec6e-122a-4364-b188-1f42ceb3ef6f)(content(Whitespace\" \
         \"))))(Tile((id \
         7d9f17c3-9fda-40ba-8933-a3518b9cafc7)(label(pow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b4348e4a-e5c6-4da9-9851-6e9c0fe28e86)(content(Whitespace\" \
         \")))))((Secondary((id \
         6a6fb48c-60bb-4910-8535-a6330e3acf18)(content(Whitespace\" \
         \"))))(Projector((id 2176d787-e889-4321-be8f-6b583adfc316)(kind \
         Probe)(syntax(Tile((id \
         8ee40d37-65da-4e49-a258-e3b9ac55b636)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b35c1912-6812-4c33-9353-14aa49d56d44)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         874c8461-a7d0-4e17-8dc1-0f9f82a28a3e)(content(Whitespace\" \
         \"))))(Tile((id \
         e761245c-6d5b-45d7-9ae3-f4be0720344c)(label(**))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e5229ae-f5a8-45bd-be8f-31cb1df49b24)(content(Whitespace\" \
         \"))))(Tile((id \
         a91233b5-0276-49f5-871d-7a5cc02e29a9)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         8210b9ba-d02d-4a4e-ac9d-a74b1fd725aa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0a120c02-1b38-4f82-8f62-95c69e85d06c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7f48c215-8742-409a-892f-a988e47343f1)(content(Comment\"# And also on \
         patterns (e.g. variables), shown in blue: #\"))))(Secondary((id \
         da2f7477-c028-4e32-b592-c0628353105f)(content(Whitespace\"\\n\"))))(Tile((id \
         69df9ae7-c388-46aa-9cb8-d5af5f5d4d94)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5f96e73a-debe-4e28-9606-9f493ce16c0e)(content(Whitespace\" \
         \"))))(Projector((id 955d0b29-80c8-4f2e-bc41-efea51092eee)(kind \
         Probe)(syntax(Tile((id \
         2baac08c-08d5-46ce-a7c7-abd7a3d8fd27)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3cf7465d-1389-4875-bfdc-9aa75ccdf5bf)(label(pow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         91ebb5e6-f9a7-40aa-b526-0e131028bf9b)(content(Whitespace\" \
         \")))))((Secondary((id \
         14b5dd20-eb6a-47e5-a9ac-d5a3b220cbd3)(content(Whitespace\" \
         \"))))(Projector((id 3af485ca-bed2-4412-8377-40f205ce90d9)(kind \
         Slider)(syntax(Tile((id \
         a2df38a1-1107-442a-83ba-0ff8972aad5e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         db8820d3-2147-42ad-808f-769e4b92f9a3)(label(54))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         3d6d3783-de3e-438f-a08f-de1bf72286cb)(content(Whitespace\" \
         \"))))(Tile((id \
         9af94ebb-8057-4346-8f6d-7c4c3d3e6e8b)(label(**))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         05887500-ee8c-4d39-8458-938b1cd2d029)(content(Whitespace\" \
         \"))))(Tile((id \
         03fb3345-cd3f-47f4-bb8a-d5cdbcd2f607)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1c4f0570-1794-4234-ab15-600f7b52a6ca)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         aecd4b91-8ac9-4cbb-ad60-49b170fe3ed4)(content(Whitespace\"\\n\"))))(Secondary((id \
         661dd998-be4f-4340-b492-2b80ab0e6e48)(content(Comment\"# Expressions \
         currently CAN'T BE EDITED WHILE PROBED #\"))))(Secondary((id \
         3d8fb7a5-d625-4ea2-af73-854173c3a451)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1fc3983-570c-43ee-bca6-0f2db2c475d8)(content(Comment\"# So probing a \
         name instead makes iteration easier. #\"))))(Secondary((id \
         c301ed95-2313-4f4b-8d69-08e0031f20d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         5aec97ee-0fb6-4bdb-9fd2-934c0d021d8f)(content(Whitespace\"\\n\"))))(Secondary((id \
         488315d1-3b0e-4fd6-bec1-5deca4606c51)(content(Whitespace\"\\n\"))))(Secondary((id \
         52ead464-f8b5-4803-a072-c1f7def9d67f)(content(Comment\"# FUNCTIONS \
         #\"))))(Secondary((id \
         720e3060-7b64-4de2-afc7-29190198c6c3)(content(Whitespace\"\\n\"))))(Tile((id \
         e494dd32-b371-4803-b58f-0823129ae5db)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d538b6ca-69c3-4ffa-9dfc-d82cf76b965f)(content(Whitespace\" \
         \"))))(Tile((id \
         25ebe172-a5b1-476c-a690-648d272f8d44)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d21a28db-ee31-4d8c-9d30-4f63926b3fbe)(content(Whitespace\" \
         \")))))((Secondary((id \
         9a8a46a6-2f3c-4750-a12e-6049bdd5f647)(content(Whitespace\"\\n\"))))(Secondary((id \
         8250afae-114f-4e46-b213-c812a286e874)(content(Comment\"# Because \
         functions can run multiple times, they can #\"))))(Secondary((id \
         b8015ac4-7c76-49fc-80ce-a4354d8aec62)(content(Whitespace\"\\n\"))))(Secondary((id \
         ee885d4f-d562-46ca-9174-b74562fb79c1)(content(Comment\"# have \
         multiple cells. Note the closure counts below #\"))))(Secondary((id \
         291b1c86-fec2-440d-a227-c7dee9008946)(content(Whitespace\"\\n\"))))(Secondary((id \
         6bb29d48-820b-461d-8266-9e0210d9f805)(content(Comment\"# are all 2, \
         indicating each probe was evaluated twice. #\"))))(Secondary((id \
         c47ee062-e10a-40f3-9f7a-de4c8de0b6b1)(content(Whitespace\"\\n\"))))(Tile((id \
         ad1780fd-8050-4532-b516-cb18e385a3c8)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3fc15b74-8faa-4515-bc09-d648aaa3882d)(content(Whitespace\" \
         \"))))(Tile((id \
         f65dfcb1-f93d-49f8-9a52-6a0fda7fba24)(label(celsius))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         78cdf831-48b1-45e8-a163-f51556d6ab9f)(content(Whitespace\" \
         \")))))((Secondary((id \
         3dc9d766-66ea-4612-9767-28621d45369f)(content(Whitespace\" \
         \"))))(Tile((id 69277366-f06a-461d-b828-d67b227405ad)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         0d72ca78-141d-48aa-83c0-74487eff52ab)(content(Whitespace\" \
         \"))))(Projector((id 5a1f8757-8535-4b53-871f-57772bc2e647)(kind \
         Probe)(syntax(Tile((id \
         82b72590-f6f0-4a09-9e42-9ed8feff7c83)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         432678e2-b003-4c8f-929d-aa39171671ae)(label(farenheit))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         2f2603fb-a650-4356-a562-f01fdbf284df)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         69d50be5-3721-4598-95d3-91134f5315b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         86db07fe-3b44-453e-a766-a2e05d91f63b)(content(Comment\"# Click to \
         select the cell above reading 72.5 #\"))))(Secondary((id \
         0161cdb6-3826-47cd-9a86-6b7dc6c67ca9)(content(Whitespace\"\\n\"))))(Tile((id \
         22af0763-da87-4889-8f31-a836e2902097)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0d091a5e-4334-468c-93c8-9c5498c94dca)(content(Whitespace\" \
         \"))))(Tile((id \
         a1f3b0a1-4cb4-4f74-9fd3-1adbdc351376)(label(diff))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         54774f62-c35d-4436-8174-0733eea33b66)(content(Whitespace\" \
         \")))))((Secondary((id \
         fc5fe903-4d6f-421a-8b88-baf52df74d13)(content(Whitespace\" \
         \"))))(Projector((id a2ca3aa2-ef85-4fab-bfc0-c60d2320a674)(kind \
         Probe)(syntax(Tile((id \
         5a31bde1-15d9-4ad1-ae98-d6481e04881a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         5274acd0-d78f-40d1-af3a-cdc064ab63e8)(label(farenheit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         26723c3c-978a-4247-8dad-ee5954042dd1)(content(Whitespace\" \
         \"))))(Tile((id \
         c32208e1-7d05-432e-aaae-307e0e647f47)(label(-.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dc21d95b-e184-488f-bb04-77c39068127a)(content(Whitespace\" \
         \"))))(Tile((id \
         1c7c00b6-5add-4c37-a4ed-81250d6f020e)(label(32.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         cabef356-1b42-442f-a9a7-999e7b247285)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         626b7352-6d31-40e0-8b43-8b61621d6dd8)(content(Whitespace\"\\n\"))))(Secondary((id \
         f7af3354-c00b-41e9-a6ef-bfb504150235)(content(Comment\"# This \
         highlights cells below corresponding to the same \
         #\"))))(Secondary((id \
         c6f3a9f7-4b96-4c20-a40b-43a3183ab11d)(content(Whitespace\"\\n\"))))(Secondary((id \
         109decdc-dbf7-4ecc-9136-bcf293385b3c)(content(Comment\"# function \
         call: the cells reading 40.5 and 22.5) #\"))))(Secondary((id \
         01dbc434-c2b1-4d5b-a8ff-63300c7e22e2)(content(Whitespace\"\\n\"))))(Projector((id \
         79ab9a1a-4839-4149-b346-ad3940986d82)(kind Probe)(syntax(Tile((id \
         214ef4d1-33ff-4d13-a01e-b0600b504975)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         70134f76-dc4d-45a2-9f99-720223c113e9)(label(5.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b458af94-8e78-4a18-b005-95fdbbf2efa6)(label(/.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4ee632a7-36f7-4be7-941d-3393c98a3d12)(label(9.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc518752-a7c2-40e3-b418-4bc5b6844a1d)(content(Whitespace\" \
         \"))))(Tile((id \
         7bd1c308-ff36-4909-8263-d544bbb4db9b)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         04e9bc36-3dce-43e1-87d9-445a8747ad29)(content(Whitespace\" \
         \"))))(Tile((id \
         80ea6324-0f71-4290-9898-f699fc1341a7)(label(diff))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         f6c6848b-c1b1-42d7-bdf7-5bd14e3d43b2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         175ff5be-2fe3-4689-b06f-8edc96057d84)(content(Whitespace\"\\n\"))))(Tile((id \
         7f243cad-1d58-465d-835b-2e854c6a5984)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         83a9b277-fbe5-41d4-93be-d25e5339d648)(content(Whitespace\" \
         \"))))(Tile((id \
         a7459006-323b-4371-9dbd-42f303b6fcbb)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         73edca44-fb61-48a0-b826-a60e82fb5ba9)(label(t1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         65d8fba4-2aaf-4f49-a854-820b8e6288dd)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         7197c8f4-1b6a-4461-bed9-8c482b516c7f)(content(Whitespace\" \
         \"))))(Tile((id \
         71e0752d-be87-4420-a5f5-48c2a42d900f)(label(t2))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         f565853c-a318-43de-b822-b3e42128f988)(content(Whitespace\" \
         \")))))((Secondary((id \
         c41877b1-46a8-47c5-88cc-98fa35d01fbc)(content(Whitespace\" \
         \"))))(Tile((id \
         7036a8ce-17a3-4c8f-a938-6f83433e1993)(label(72.5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fcd193db-079d-4d93-b8b7-68eea70370b7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b41685c8-cea6-4e2f-be5d-746722b32b27)(content(Whitespace\" \
         \"))))(Tile((id \
         3c93db20-43f0-4ede-ae21-b8ae656af464)(label(103.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bcf10524-b8ac-4694-989c-d4a60add7e81)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e3a84ce8-f2b5-4b73-b455-d52aaa68c9c2)(content(Whitespace\" \
         \"))))(Tile((id \
         25f55cc8-453c-49ce-9925-66dcbff41073)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         cfe22abc-f9c0-4b5f-b1d5-92c331fbcde7)(content(Whitespace\"\\n\"))))(Secondary((id \
         6784f83e-84b8-46cc-93a9-761c54b752b8)(content(Comment\"# It also \
         highlights in purple the cell #\"))))(Secondary((id \
         5ba2168a-6d46-4f47-809e-d56ef73652b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c8b660b-481c-46f5-9800-96b2b8e001a5)(content(Comment\"# of the \
         function's call site#\"))))(Secondary((id \
         7fbd15ec-7cbb-4451-9eb6-0512f9fade3a)(content(Whitespace\"\\n\"))))(Projector((id \
         88d0aba6-4c3a-4741-8b2e-9912be4f1165)(kind Probe)(syntax(Tile((id \
         b5d88252-85cd-45b5-88f3-a319150aad80)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         330e8372-5e9f-4049-9020-a6885c9d10be)(label(celsius))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         844f3cca-f187-437f-ba24-ed2ac48e6890)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4587999b-0aeb-4cde-aa55-07380b68538a)(label(t1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Tile((id \
         00b52878-2ddc-4b56-9fde-edf0842ac46f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         59fc6a7b-27d4-4ff2-86ba-69bb08f039bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3f3b276-3ff0-44b1-a4c4-a7cef950ef28)(content(Comment\"# Now select \
         the cell above reading 22.5 #\"))))(Secondary((id \
         f919ff60-54f4-4955-80df-80b8fdfee1b4)(content(Whitespace\"\\n\"))))(Projector((id \
         298771ff-4634-4d41-ad94-785a836f347f)(kind Probe)(syntax(Tile((id \
         638db290-005e-4c03-b4b0-b7ff870f2d81)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         423f10e5-c69f-4c77-877c-9a1045b60c63)(label(celsius))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4b8f419f-d1f2-444f-989f-a7bee1bb6718)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         da303f8f-8e5d-4b7d-aeec-9aa4434e4f77)(label(t2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         d3886971-0923-4db2-952f-71c578558273)(content(Whitespace\"\\n\"))))(Secondary((id \
         da986599-34e8-4fb4-ae6d-54d01f2c9134)(content(Comment\"# Note the \
         72.5, 40.5, and 22.5 are no longer green-highlit \
         #\"))))(Secondary((id \
         4fe58a75-447b-4dbe-8820-039e9860fbcd)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e1ae050-c423-4355-a3b1-a354a5b5ab27)(content(Comment\"# as they are \
         not part of the same call as /the expression/ #\"))))(Secondary((id \
         4d1ff22c-250e-4cde-9098-29abe2a9529e)(content(Whitespace\"\\n\"))))(Secondary((id \
         02ea3b1b-053e-435d-b305-7f9b6f542ad6)(content(Comment\"# \
         `celsius(t1)`. However, they now have purple hats, indicating \
         #\"))))(Secondary((id \
         a96fd606-68b1-4b63-819b-9c9c4a8fb809)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f0a9437-9a9d-46b3-909d-67312323e37e)(content(Comment\"# they are \
         below that function call in the call stack #\"))))(Secondary((id \
         5cefe26a-8be7-4c07-9705-da0601292d31)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         41728997-2b51-40c7-a698-35dd30927a8e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8d337f08-7974-4d9e-b4e8-f236bac4f1e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2d78f99-8f7f-4fe1-a2a1-412f2fff5350)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf3be17b-46d7-4068-b4f4-9de0f4a9c794)(content(Comment\"# BRANCHING IN \
         FUNCTIONS #\"))))(Secondary((id \
         d0c6b347-708b-4723-ad00-892a988aea06)(content(Whitespace\"\\n\"))))(Tile((id \
         57dccee0-f064-4dc2-8b5e-eaa1d1e06853)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         be28b984-35f0-4c01-b77f-4816ee6ca97b)(content(Whitespace\" \
         \"))))(Tile((id \
         184cfaa4-5502-477a-90b2-e3cfbbdb7e39)(label(cases))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e16e0852-07b6-45d3-aba4-008951717cee)(content(Whitespace\" \
         \")))))((Secondary((id \
         629bbac2-ac4f-4611-908a-92f60d17e305)(content(Whitespace\"\\n\"))))(Secondary((id \
         094d42fe-1342-4ab0-8da8-3954cc7072c7)(content(Comment\"# Select `6` \
         then `5` then '4' below: #\"))))(Secondary((id \
         62517a48-5ecc-4e88-89ec-48d644e023f7)(content(Whitespace\"\\n\"))))(Tile((id \
         a292c5fb-6d4c-4035-b200-adcf926eb605)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2d7da9b4-11e1-4078-abdb-9a8e90ce6b2e)(content(Whitespace\" \
         \"))))(Projector((id 5bfe50b9-1339-43f9-8e66-169145a9f7a9)(kind \
         Probe)(syntax(Tile((id \
         b4269b63-ee87-4614-8b44-053f428a661b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         0626cd00-4c2d-426c-988a-ff577e24b50c)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         d9ef79dd-d8c6-40a4-b4e8-e8e13d9c5f52)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fff7fde5-5d9e-4bdc-8529-4110cd566ddc)(content(Whitespace\" \
         \"))))(Tile((id da922388-f9fb-45a9-99a8-24d0ae22ac3c)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         febab30d-ccb1-41b3-b6e8-94fda2ffbd8f)(content(Whitespace\" \
         \"))))(Tile((id \
         49591759-40f8-4171-8690-b400dde5ecce)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bb8757be-4207-44da-9f7c-61b619e1752b)(content(Whitespace\" \
         \"))))(Secondary((id \
         5c026e5e-df02-4606-9030-ece53ebf85c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5d66f89-7b0e-458b-a35a-795395df1a51)(content(Comment\"# Note how \
         each activate exactly one branch below: #\"))))(Secondary((id \
         aa031699-3854-4660-b5f5-4e2d96f302be)(content(Whitespace\"\\n\"))))(Tile((id \
         ef1d5d7b-61e7-43da-8dce-62e80cbf2727)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0c6989b0-214b-46f7-b1a6-2da5ef74ca47)(content(Whitespace\" \
         \"))))(Tile((id \
         63e90f36-c417-431e-8650-73183dd8ce10)(label(4))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c6added0-30b6-4fcd-ae76-b5f6f0cf3ffb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e1a58592-7397-49b0-a926-7b5f4063d185)(content(Whitespace\" \
         \"))))(Projector((id 4bdfa21a-2da5-42b0-a152-8a1d86540d5d)(kind \
         Probe)(syntax(Tile((id \
         0e55fb5a-adcf-4a32-bdfa-89fe4d68cc40)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7bff023c-81e0-40e4-b35c-d31f668c1694)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         58e1df2b-7ffe-4088-86e0-8a66b67c5bb5)(content(Whitespace\"\\n\"))))(Secondary((id \
         6854d5fe-1a91-4587-ae0c-25dd8d0cb81d)(content(Comment\"# Select the \
         `5` above and then the `false` below: #\"))))(Secondary((id \
         fa1368d5-fdec-41c4-9cea-6fbc23cdd468)(content(Whitespace\"\\n\"))))(Tile((id \
         70e835b3-e7a0-4bf8-91a0-1e110153a9cc)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0bb8dfc9-9ca8-403e-b46e-dfba09bc7d94)(content(Whitespace\" \
         \"))))(Tile((id \
         5223a7d5-d431-45f1-9b73-bae0a887d775)(label(5))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         71befb5d-6dc8-44e5-a9a2-a3875b63e0fb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a2bbb70a-8a3a-4700-b5b3-29fed0ba5348)(content(Whitespace\" \
         \"))))(Projector((id 921ef383-d677-481e-a965-65500b37e0aa)(kind \
         Probe)(syntax(Tile((id \
         caedd916-27b3-4c74-bb09-24942705750b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4e4d4917-2ec5-4fc4-831e-8c80b55e59ab)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         9a852733-ec04-4ad5-b3eb-615c4f7447a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         b05bef7e-8736-4125-a3f7-2211e62bcd6f)(content(Comment\"# Note the \
         same things are highlit as both cells are #\"))))(Secondary((id \
         cd0f303f-e764-4d59-844f-d492f7c6ef90)(content(Whitespace\"\\n\"))))(Secondary((id \
         2652d6b5-8222-4d03-ab5a-1e9e0a058c54)(content(Comment\"# from the \
         same call to cases#\"))))(Secondary((id \
         a3303bd8-1f94-4b69-a565-2104a24991a9)(content(Whitespace\"\\n\"))))(Tile((id \
         ce499445-6d5a-43f9-875e-b8fceadf7c9d)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b904440f-8a05-4b34-b494-41b9b84aa60a)(content(Whitespace\" \
         \"))))(Tile((id \
         79f75f34-5323-4007-9015-50052b98139d)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         171bf311-84b3-43cb-9488-4eea8632c0f2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9ee37e1b-17bc-4ff5-995f-c074cf3a6967)(content(Whitespace\" \
         \"))))(Projector((id c204a30d-1af0-471e-9bdb-07292ca4c320)(kind \
         Probe)(syntax(Tile((id \
         a65407e8-82b6-4928-94f9-cf2b4093f5fc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         ff10e8b0-9129-485b-be57-b4c3db20f9fd)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         6fc55f71-f937-457f-92bd-2f31c4da94f2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         37124cf8-ad96-4f37-9f5e-197750ab299b)(content(Whitespace\" \
         \"))))(Secondary((id \
         80b60b67-d76c-4035-8004-4ea1af6b2363)(content(Whitespace\" \
         \"))))(Secondary((id \
         4d654ad9-293c-42a0-9d52-e4a1f6bb11b8)(content(Whitespace\" \
         \"))))(Secondary((id \
         d62f6a50-3742-4933-b989-b8bc83098d32)(content(Whitespace\" \
         \"))))(Secondary((id \
         1cb415b0-3c22-41f7-bd66-a059c8108edb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e42dc954-f723-4d22-8076-94974677cc29)(content(Whitespace\" \
         \"))))(Tile((id dfb15bab-90ca-42bd-b283-d0a6ac7b47e2)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 40))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         827956ed-6700-4bd4-8e19-e53c994dfdd3)(content(Whitespace\" \
         \"))))(Tile((id \
         a3ffec75-3c7e-44c5-b793-352633592b71)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         acb2b259-bba1-4436-8950-100ecc2999d3)(content(Whitespace\" \
         \")))))((Secondary((id \
         f81ccc40-8745-4b1e-845d-afc668156576)(content(Whitespace\" \
         \"))))(Tile((id \
         f92804e2-0b64-43d1-9aaa-998f5112f385)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e68b109d-29df-4650-9a20-109232cd3651)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6bcdbb3-0410-402a-bfbd-fd5069195832)(content(Comment\"# Select \
         `true` below and then the `4` cell #\"))))(Secondary((id \
         66738c8a-ca47-4f41-bae0-9beefa4853ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         913ace86-495c-4eb0-8375-1b68dd150c27)(content(Comment\"# for the \
         argument x to `cases` above. #\"))))(Secondary((id \
         a8a3aa3b-735c-477f-9714-d6b827727e80)(content(Whitespace\"\\n\"))))(Projector((id \
         ed002d4c-74d0-48a0-a3a8-75f69c451b1a)(kind Probe)(syntax(Tile((id \
         b029cc7e-b48b-414c-a968-b880c897b6d8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7c5710dd-0de9-4e93-a16c-1a6d3d926162)(label(cases))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5f88e23-0af6-44c8-8314-b2715acddf1a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d0b5fc5e-6e99-4edd-8efd-772d0ad6ee63)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Tile((id \
         b564721d-e215-4d12-9a16-8b1db11e8f87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8e94c859-61c9-4f06-8ef9-9847c47d3d87)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7867f0c-32e4-4604-b7d0-49cc5eb62f54)(content(Comment\"# Note how the \
         same cells stay indicated, but the kind #\"))))(Secondary((id \
         9b2d940b-e3d2-4a75-b0ee-fe013bc05a75)(content(Whitespace\"\\n\"))))(Secondary((id \
         0bfc6cfe-b67d-44ed-8e92-174ea5548b6d)(content(Comment\"# of \
         indication changes. The `true` below the `4` above \
         #\"))))(Secondary((id \
         15d8e14a-396b-49d6-970a-77ace6fee5cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         85cccabd-f4a7-4fd7-b096-e02c895dd9f5)(content(Comment\"# goes from \
         purple outline (created by the cases(4) call) #\"))))(Secondary((id \
         23aef987-0121-4037-bcdb-b421d86de5d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         c0aed9f4-9473-4de2-93dd-b0867857f4ab)(content(Comment\"# to green \
         highlighting (part of the same call as `4`). #\"))))(Secondary((id \
         6191ba6a-4114-49a6-97cb-f51e4f7a263c)(content(Whitespace\"\\n\"))))(Secondary((id \
         65f0c0ff-4590-48c9-a2c9-7a2ee330c0ad)(content(Comment\"# The formerly \
         selected lower `true` is now highlit in #\"))))(Secondary((id \
         3e90c98e-292e-4204-a3a5-628000e8d338)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b392224-1a45-458c-ac9f-403aaaa71d55)(content(Comment\"# purple since \
         it indicates the call where `4` lives . #\"))))(Secondary((id \
         b78c8720-3e61-482a-b3c2-61b5737f0512)(content(Whitespace\"\\n\"))))(Projector((id \
         9b8aea83-73f3-4f5b-aa0a-9f3788d37f7b)(kind Probe)(syntax(Tile((id \
         80b1ac2d-f0ac-438f-8909-62734e3b4560)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6cefd566-da5e-4ddd-bacb-27802361084d)(label(cases))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d5b36fad-cc60-4fc3-bd5f-965e2a4b851c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fc4d9314-ef93-429e-bae8-ca347b65e436)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Tile((id \
         0f44dcda-4be1-4e56-9db3-cfc97ba054dc)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         80ca5f13-607c-4974-a988-30c808349fab)(content(Whitespace\"\\n\"))))(Projector((id \
         9c02de73-923a-429e-8bb6-7389e012c6d4)(kind Probe)(syntax(Tile((id \
         4e1c7e3b-7347-48c1-a6e0-6166ea4c9e3c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7442905f-f3e9-4c5e-a91e-172a9b5cb1e0)(label(cases))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2e9f13dd-bf9e-4460-aa4a-595aec41e4d4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ca51ab21-5f9f-431b-9189-e3dea8272dcc)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         395aec95-5274-457a-8bd5-1aa6004fa810)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2ba74583-7090-4845-9555-cdb3cdaf048e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d9a78466-9342-44ea-a6be-ca6e8c8d2176)(content(Whitespace\"\\n\"))))(Secondary((id \
         4114635d-c94e-4332-aa18-a1729fbc7112)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d99beab-cf8e-4fc5-8d75-9d4f906b4279)(content(Comment\"# FUNCTIONS \
         CALLING FUNCTIONS #\"))))(Secondary((id \
         b484296a-a226-49d4-92e9-6b8a5c7394f0)(content(Whitespace\"\\n\"))))(Tile((id \
         3e536c99-e851-4d05-9251-7bfded462fd9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         22cef74c-60ac-4e3c-a606-54d36583de0e)(content(Whitespace\" \
         \"))))(Tile((id \
         101f448b-0c85-487a-b0ea-7a93e461ea54)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f75dd506-7aa3-4297-87c8-d1150e43fe00)(content(Whitespace\" \
         \")))))((Secondary((id \
         1c7e2fea-6100-4cd2-ac05-ef92670bddca)(content(Whitespace\"\\n\"))))(Secondary((id \
         f44cec80-cc10-4cb6-82d3-4592ca51c536)(content(Whitespace\" \
         \"))))(Secondary((id \
         d3020e3f-5a6e-43ec-9de6-fe928aaf6369)(content(Whitespace\" \
         \"))))(Secondary((id \
         97f3a660-9e86-4975-8af5-4f9feaca3d60)(content(Comment\"# Select `9` \
         below. Note four cells below become purple #\"))))(Secondary((id \
         672d6238-955a-4195-a1c6-90f82a58b40a)(content(Whitespace\"\\n\"))))(Tile((id \
         79f6e744-aec0-4243-8957-23bc53a9d24d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3fdcf596-935e-474a-9293-265a401d551d)(content(Whitespace\" \
         \"))))(Tile((id \
         a7c2ec78-b9ca-4d86-8cb0-3d7d9e3539aa)(label(fourth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8a6c951a-7647-479f-8485-73b23f00ed9c)(content(Whitespace\" \
         \")))))((Secondary((id \
         b0f2f303-99c5-41a2-851a-412381d16f21)(content(Whitespace\" \
         \"))))(Tile((id f2f9e2a5-9628-4e1a-a4af-2a59773c3fc2)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         361a51c4-0edd-4cc2-b117-8fb3ed4a620b)(content(Whitespace\" \
         \"))))(Tile((id \
         283dfaa2-dc73-4e43-9766-5ea17b0f7144)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3422f8c0-871f-412b-82a8-08ed390a4d3c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         be4d4fab-16ca-4224-a8dc-076e77762427)(content(Whitespace\" \
         \"))))(Tile((id \
         00248684-1785-4fc0-b510-07b597d4130c)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b6aa2666-abca-4aba-bc5b-a68bc8a28994)(content(Whitespace\" \
         \"))))(Tile((id \
         409a38fb-28d0-4bce-88c6-f7271dbc4bc5)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f57d453d-1522-4185-9e4d-3eae59be9021)(content(Whitespace\" \
         \"))))(Projector((id bb745176-acec-4a27-a712-1f096ebe6200)(kind \
         Probe)(syntax(Tile((id \
         af6ada45-2565-4cf4-9a87-61c17c55b060)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fee57206-6adf-4779-85b3-5e32590f7984)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         5feb0106-e532-4a16-b903-cb4d7f5f2ef8)(content(Whitespace\" \
         \"))))(Tile((id \
         710d4256-c05b-4fd7-b92e-b4576fd7e0a4)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b2754f00-78d2-40fb-8d20-358914d98c79)(content(Whitespace\" \
         \"))))(Tile((id \
         5ce632e2-368d-4b60-8265-7dff74ed586e)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3fcba413-7add-4a8f-baac-be628d238102)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e2c20e8e-8b85-4889-bb11-5f8a0160d0a5)(content(Whitespace\"\\n\"))))(Secondary((id \
         40b10a72-b736-4ff3-b985-8a92f9383789)(content(Whitespace\" \
         \"))))(Secondary((id \
         885af32a-2ffb-4fa7-a3af-ce5ee8f25a2b)(content(Whitespace\" \
         \"))))(Secondary((id \
         37eb677b-72a8-41ec-a47f-36b217173adb)(content(Comment\"# This is \
         because they represent function calls #\"))))(Secondary((id \
         d6b141ae-ad65-4885-95b0-c55882c0346d)(content(Whitespace\"\\n\"))))(Secondary((id \
         74669f66-8914-43c2-b76e-577a51fb8766)(content(Whitespace\" \
         \"))))(Secondary((id \
         df7a3071-70dd-4d06-9e93-e10ba2ac5e14)(content(Whitespace\" \
         \"))))(Secondary((id \
         c6d6544c-69b4-46c6-afdd-57ba806fa2d1)(content(Comment\"# above the \
         `9` cell in the function call stack. #\"))))(Secondary((id \
         ef769296-e176-4dd4-a2c0-006988c67f77)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a0c99a5-8b93-4c35-a9ef-10a595dc33b6)(content(Whitespace\" \
         \"))))(Secondary((id \
         dec94409-12db-4673-96ad-c1a04e0a25fd)(content(Whitespace\" \
         \"))))(Secondary((id \
         2f6538d1-147f-4bb4-b2c7-7b97a981dccd)(content(Comment\"# For example \
         32 below represents the call producing `9`.  #\"))))(Secondary((id \
         40c5b516-3fbe-48f3-8b49-f896d7583a1a)(content(Whitespace\"\\n\"))))(Tile((id \
         def2e3a4-66d6-4d58-9ddf-80c3575f0bd2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         984d0ed3-232e-4caa-ac9f-60500af7e428)(content(Whitespace\" \
         \"))))(Tile((id \
         df00ee64-2091-4170-98e7-d01fc02b1ac3)(label(third))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a3770512-f0c0-4b21-ab21-ab70fd3e3801)(content(Whitespace\" \
         \")))))((Secondary((id \
         ae1d5d14-bbe9-45bb-a626-efa769ecbd9e)(content(Whitespace\" \
         \"))))(Tile((id 5c7755fd-4196-4d46-9a1c-5f3c1f4628f0)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         72b98e7f-ceca-4976-9517-aef65c3f524a)(content(Whitespace\" \
         \"))))(Tile((id \
         4dffc221-bdfc-4b1f-bef2-ccc125be0d6b)(label(t))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         15da9cf6-4b47-4d61-8e83-0298b548046a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         34a6b9de-4a4c-4e80-bcee-1c2ed534c4ee)(content(Whitespace\" \
         \"))))(Projector((id fc3df5d1-75f5-4595-b9e7-bc360fea0272)(kind \
         Probe)(syntax(Tile((id \
         102d7a41-2d56-4970-8ff2-f47a015ff475)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         00a17bd2-d8ae-4727-b9dc-b69239605684)(label(fourth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc40a889-5015-4f96-b4c4-c241dc99dcf8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0bb0bcec-f448-4bf4-9636-2589684e8d3d)(label(t))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3422010b-d57b-43b0-b11f-db7548b3fd1c)(content(Whitespace\" \
         \"))))(Tile((id \
         4e135b5e-bea9-4195-81a8-2d1af35c3fd5)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         44c24372-0ea5-4fcc-a9e0-b753006ff7ce)(content(Whitespace\" \
         \"))))(Tile((id \
         ef072a75-985b-41bf-8e9f-dbd615af38e0)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         bed1855a-17c7-4842-ace1-2b4e1bb25090)(content(Whitespace\" \
         \"))))(Tile((id \
         96584ced-3666-48a6-a471-f97035d79157)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         43f8143e-cbe6-469a-91dd-597917e9aea7)(content(Whitespace\" \
         \"))))(Tile((id \
         c51a5645-11c5-4f50-a827-7e133aa4af8a)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         771f699b-e030-4f31-84e7-d63a995688f7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         35d7fd9f-fbe7-4e9a-b67d-86086a1eb205)(content(Whitespace\"\\n\"))))(Secondary((id \
         40f59d5c-fad3-4b4f-a141-b55ba3d7f174)(content(Whitespace\" \
         \"))))(Secondary((id \
         2920a2d7-c6e2-4bda-88f7-4ec351c53462)(content(Whitespace\" \
         \"))))(Secondary((id \
         55d5d59c-70a3-4ce2-80ab-a4aa817c272c)(content(Comment\"# Now, select \
         `32` above. Note the 9 now has a purple hat. #\"))))(Secondary((id \
         999f2b1a-25b0-48cc-ac4d-a54a0b4042db)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2210431-986d-49e8-b41d-184573cc447a)(content(Whitespace\" \
         \"))))(Secondary((id \
         ad5af3be-588c-4ece-ac74-1092a754a2cb)(content(Whitespace\" \
         \"))))(Secondary((id \
         fafeb877-3e8c-4ca1-9842-a9cc6fdf6917)(content(Comment\"# This \
         represents that it is below the `32` call in the stack. \
         #\"))))(Secondary((id \
         9d507c84-e09c-4d9c-84ee-fbe924f86e99)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5bed63a-b093-4ad3-aa23-421353f9b357)(content(Whitespace\" \
         \"))))(Secondary((id \
         d4bc4691-3464-4aa3-a694-775f7a3045bb)(content(Whitespace\" \
         \"))))(Secondary((id \
         f7500b13-cdd4-4b03-a231-d165f6fbd912)(content(Comment\"# Now select \
         `10` below, which is a call to `third`: #\"))))(Secondary((id \
         9a69ec65-33a3-475b-be5c-b437be0022bc)(content(Whitespace\"\\n\"))))(Tile((id \
         b3352667-d2e1-4040-8dac-404e5697be38)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7e3a894b-5712-499f-9491-6706f8e5ec68)(content(Whitespace\" \
         \"))))(Tile((id \
         d497eace-caa4-47ad-b2b6-420ab3dc4b8b)(label(second))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7ece225d-b70c-439e-be3f-39643a1cd830)(content(Whitespace\" \
         \")))))((Secondary((id \
         be721dd7-6f78-45d4-852d-84ffcd5ebc45)(content(Whitespace\" \
         \"))))(Tile((id 75cb83d4-77a2-454d-95d1-e5729e8632db)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         dbf60b2f-095a-4157-910f-da08a1175396)(content(Whitespace\" \
         \"))))(Tile((id \
         65b5c2ad-44f8-4987-aac5-933eace8f60b)(label(s))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9721d121-e28b-4201-a4db-8adf084cef1c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         57cbac08-fe14-437d-b032-391626018c4a)(content(Whitespace\" \
         \"))))(Projector((id 34643502-e4b7-49be-8dbf-ae6bcd41b2b9)(kind \
         Probe)(syntax(Tile((id \
         192f45ba-b50a-445a-99b8-17527936e7e3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f376eef8-0d05-4582-b12d-e8e17744bed2)(label(third))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b90334ea-5ed9-4fb5-8ed3-9e50663acabc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         01929910-0800-40b4-b2e9-791cd05514a1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4f8bfa03-95e2-41c6-8e4b-30410e553595)(content(Whitespace\" \
         \"))))(Tile((id \
         0a949bf1-2f7c-43ce-90f2-4b5413329fed)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         175d4d6c-1ea2-466b-897a-bf4901b32980)(content(Whitespace\" \
         \"))))(Tile((id \
         8d3853ff-c961-4ecc-b37c-1f5118847c04)(label(s))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         4d6fbb9b-cffa-4d3a-af39-8c963eb8a1fe)(content(Whitespace\" \
         \"))))(Tile((id \
         20a50407-3759-4c48-b28b-77a93c9889cb)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bdd08ff2-5e16-4902-ac54-3619195d0a4e)(content(Whitespace\" \
         \"))))(Tile((id \
         37751a91-d52a-468c-af5b-18714e1705f8)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4853f187-e0af-4f5f-b009-c93a3a5c1d57)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d61839e3-8852-4432-83cd-159b9ce5bb36)(content(Whitespace\"\\n\"))))(Secondary((id \
         23684c0d-36e7-4184-8c72-f29752b278da)(content(Whitespace\" \
         \"))))(Secondary((id \
         9066dc65-2207-40e8-9005-2509a5dec728)(content(Whitespace\" \
         \"))))(Secondary((id \
         51c597d4-edf7-4fb0-8710-5bf38feb6178)(content(Comment\"# Note that \
         `9` and `32` both have hats. `9` has a taller hat \
         #\"))))(Secondary((id \
         a717c0d0-81c8-4350-8444-7950a94511b4)(content(Whitespace\"\\n\"))))(Secondary((id \
         a6c17916-f38e-4b5a-96d6-5820075d8cb7)(content(Whitespace\" \
         \"))))(Secondary((id \
         fa799063-168a-44a3-82cc-d35ac5e17640)(content(Whitespace\" \
         \"))))(Secondary((id \
         a243e83e-7ad2-468f-8262-7aece7761cef)(content(Comment\"# to show it's \
         lower in the call stack. `32` has purple text #\"))))(Secondary((id \
         b176001e-97f0-430e-9c52-2e4c06f8ac88)(content(Whitespace\"\\n\"))))(Secondary((id \
         e04e936c-84b3-476a-88aa-3a2096eba78f)(content(Whitespace\" \
         \"))))(Secondary((id \
         eb8663a1-16dd-4a0a-a7cf-b902430fd5ef)(content(Whitespace\" \
         \"))))(Secondary((id \
         11d09808-6b63-4938-9175-3cf411c6dc83)(content(Comment\"# to further \
         emphasize that is is directly below `10`. #\"))))(Secondary((id \
         626ea327-cd1b-4cbf-9b63-30303d80ea46)(content(Whitespace\"\\n\"))))(Secondary((id \
         803da079-b564-42df-8389-eb48257099be)(content(Whitespace\" \
         \"))))(Secondary((id \
         e6fbf690-bdcb-45e9-8c61-33e86d7e7465)(content(Whitespace\" \
         \"))))(Secondary((id \
         22417862-1d0e-430d-b4df-0ce24dc8c404)(content(Comment\"# Now select \
         12 below, representing a call to `second` #\"))))(Secondary((id \
         236504e0-6c37-4271-ba99-02dc366bad4e)(content(Whitespace\"\\n\"))))(Tile((id \
         7ebc612a-8aed-4f26-8999-2379b2df018d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3c50b54e-fa94-42c1-88e3-9b072f20d98a)(content(Whitespace\" \
         \"))))(Tile((id \
         1a0a5441-1ad9-42af-b945-94caa7caab16)(label(first))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0c88cfc5-4203-40c7-917d-1c9a122e9ea6)(content(Whitespace\" \
         \")))))((Secondary((id \
         acf1cb3c-b74c-43cf-a4fe-3f522237b935)(content(Whitespace\" \
         \"))))(Tile((id 7738d2ad-05ee-435e-9e50-5b6b7ca7ed07)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e73b6ab0-bcaa-4ad4-910a-849c2b5b69cc)(content(Whitespace\" \
         \"))))(Tile((id \
         3e0af08c-1ff3-4019-8503-cd4b1fe9707d)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         13f8ca0f-219b-4c4a-bec4-9492a03ff3db)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1fa58b72-2d36-433a-8d0d-d9e3229f4f27)(content(Whitespace\" \
         \"))))(Projector((id e896ca2a-24bb-4cd7-9fe1-b23e56dd2a40)(kind \
         Probe)(syntax(Tile((id \
         241282e3-f3e3-4a94-b65c-94ae5740358b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         23564024-9865-483d-99ec-fe2f9ac79bcd)(label(second))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fe88043d-71e3-4182-97c8-55396edd1859)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0d0083a2-0a86-49f9-afb8-d653fef6d779)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         61c3ec05-4dd7-4506-aa8b-6a937d57af80)(content(Whitespace\" \
         \"))))(Tile((id \
         37dba9d9-a906-4780-991a-0e9da7c1f8c8)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c9bde02-4269-45d9-853b-3bffbbfcfc58)(content(Whitespace\" \
         \"))))(Tile((id \
         898af5b5-1e12-48f2-bbeb-fdd41f1c3ce4)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         d6503127-f3dd-4a0d-9a19-ec213341ad29)(content(Whitespace\" \
         \"))))(Tile((id \
         6898e69a-45ed-4a2e-a0ef-bb5c60223889)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         15b0b19a-cd75-4a8e-9803-8c638dd26a77)(content(Whitespace\" \
         \"))))(Tile((id \
         247deefd-4015-494b-92d2-15cfb8658102)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         801db48e-cf40-4a96-b63c-20e1f79fd9d8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         19ed830c-e78c-4b53-9492-a7422fc4e108)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3e5a72a-d79b-479d-a967-b12954882a5c)(content(Whitespace\" \
         \"))))(Secondary((id \
         1677e1e8-c45c-4c15-968a-c170197f133f)(content(Whitespace\" \
         \"))))(Secondary((id \
         effec567-910c-43e5-9818-28b0a7ba88ae)(content(Comment\"# Note how the \
         hats have changed. Finally, select `24` below, #\"))))(Secondary((id \
         513a1274-6dac-417d-90d8-736d5ee0844b)(content(Whitespace\"\\n\"))))(Secondary((id \
         9fa18e12-59e1-485b-bc89-37f7ea1a881c)(content(Whitespace\" \
         \"))))(Secondary((id \
         526c489c-93dc-4ccb-93ab-4d6f3da48d35)(content(Whitespace\" \
         \"))))(Secondary((id \
         aeb230ce-1add-4a39-ab17-8fb561a5c539)(content(Comment\"# and then \
         again select 12, 10, 32, and 9 in turn. Notice how \
         #\"))))(Secondary((id \
         2ea853cb-6dac-4f0a-8828-daa21e251762)(content(Whitespace\"\\n\"))))(Secondary((id \
         943f3910-eefb-4731-a5b4-7b8645719de1)(content(Whitespace\" \
         \"))))(Secondary((id \
         2c1eabac-fb4d-44aa-a693-e170d227ba5c)(content(Whitespace\" \
         \"))))(Secondary((id \
         fc1b0b1d-d544-42bc-a59b-f9ff8c2038f1)(content(Comment\"# the solid \
         purple call indicators have internal shadows #\"))))(Secondary((id \
         eb9d34b4-0e63-4424-ac48-5ad1e860372e)(content(Whitespace\"\\n\"))))(Secondary((id \
         3e83e2cb-4b03-4c33-ab86-fa2eb40808e5)(content(Whitespace\" \
         \"))))(Secondary((id \
         0750fbef-fd06-412d-a4b6-a2c8f5b0a6b6)(content(Whitespace\" \
         \"))))(Secondary((id \
         828dfde2-c9a9-4cbb-a75d-6b4a3622a1fa)(content(Comment\"# representing \
         their depth relative to the selected cell #\"))))(Secondary((id \
         f8b76c2a-e02d-4a0f-91ae-7ec9c421f5e8)(content(Whitespace\"\\n\"))))(Projector((id \
         a7d341eb-18b9-412f-983b-59c7596103e8)(kind Probe)(syntax(Tile((id \
         4eea541c-9c9a-438c-bc0f-02ab1512d318)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b0020a12-9731-4c07-a095-93fa3f48f415)(label(first))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a67d5e4e-80b8-412d-886a-d7b67426d9a1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fa4cbf11-ac4c-4ab0-b055-1f13ebe59b42)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         832d12fa-6234-4b7e-ac19-6188b5c558fa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ed8aad0c-8bdb-41e2-bf8d-dd055935f9ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         24087ca1-d3eb-489e-bbce-26500fdea72d)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6def294-2ebe-4dda-9695-6e1ab9717f28)(content(Comment\"# RECURSION \
         #\"))))(Secondary((id \
         77740d89-976a-45d8-becf-ae4fdea59b94)(content(Whitespace\"\\n\"))))(Secondary((id \
         10753836-4e3a-44d1-a024-2b7e1474856b)(content(Comment\"# Note how \
         cells are lowered/raised to indicate their #\"))))(Secondary((id \
         08d17c4c-aa6c-4395-bbc3-be47f6ee2bbb)(content(Whitespace\"\\n\"))))(Secondary((id \
         44fd88c5-12a9-4b67-b19d-46426adf1e46)(content(Comment\"# relative \
         call stack depth to the selected cell #\"))))(Secondary((id \
         cfe53560-96f2-46d6-b5c2-75b459354a2c)(content(Whitespace\"\\n\"))))(Tile((id \
         d67e559a-9061-4614-ad46-9f892d08bb2e)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         bc4fe845-fa8c-45a9-9279-d9c0d4d38ffb)(content(Whitespace\" \
         \"))))(Tile((id \
         6db8a5f2-3dbe-4dfc-82ec-97564eafbce8)(label(fact))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         64008638-30fe-4546-a958-f1f0589526c8)(content(Whitespace\" \
         \")))))((Secondary((id \
         9848ed48-2f51-4e05-90d2-deae280d6dbf)(content(Whitespace\" \
         \"))))(Tile((id 36811263-f1d7-432f-93e8-6a752c0d02ab)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         399f7624-a41d-450b-9cbd-143c9b562d88)(content(Whitespace\" \
         \"))))(Projector((id fbb7653f-3744-46e2-9375-db0e00e850f9)(kind \
         Probe)(syntax(Tile((id \
         aaf7982d-c868-412e-beb0-57bcc7dae0f5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         77e64c06-f45f-4939-9cfe-02f10f352d3e)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         8806c6f6-ba39-42bd-8ba8-a34d4b2a2e03)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9fa5c4ed-eeac-4cb2-b684-97553e00f269)(content(Whitespace\"\\n\"))))(Tile((id \
         27c6c738-e79a-4d74-8f8e-a4aa6b9a464c)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         2d8d463d-e67f-4dd8-bcef-2fd38929e52e)(content(Whitespace\" \
         \"))))(Projector((id 2b4b1a23-5e68-4bb8-a9f8-aeba9f89595b)(kind \
         Probe)(syntax(Tile((id \
         61552fb2-4928-4177-99df-3ff6fa150428)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         616b5a02-5ce3-44d1-b371-50cff0b93dc6)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         670ce8bf-80c2-4f98-bec2-9d515ef892d2)(content(Whitespace\"\\n\"))))(Tile((id \
         69b387f7-eb49-41f9-93d0-af6f6b3b3aad)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b4b58334-eb23-4f8d-a7eb-3730a2efa116)(content(Whitespace\" \
         \"))))(Tile((id \
         4515a8f3-fd21-4eb5-8542-1ed1f18d5fdb)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         566af644-bcb3-444f-af84-bc9d2e4b495d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         09dfb98a-cc65-45b1-916c-977e8aadc27a)(content(Whitespace\" \
         \"))))(Projector((id f46d9112-bac3-4857-bcb5-7fd9ea0c59d9)(kind \
         Probe)(syntax(Tile((id \
         42557a2b-bcc0-42a9-bb1f-b728ec28d3a7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e0a89cf4-22ce-497d-8a6e-8148403fffea)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         b2aff38d-072c-4b3b-ad7f-a9565fc3e5be)(content(Whitespace\"\\n\"))))(Tile((id \
         e3d5ff6e-726c-482b-b8d5-82198dea3a88)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2735f222-8a06-45bb-ae74-0a9dca7dd0f2)(content(Whitespace\" \
         \"))))(Tile((id \
         71d9d521-ad13-4287-bb57-6fc316774ccb)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         be6f2138-8384-481a-88e7-fa651638c5c8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dc386a89-82aa-48e9-8d58-a2813709a883)(content(Whitespace\"\\n\"))))(Tile((id \
         eb560f86-9c02-45d0-a700-6495d4eb0f7a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0dbffdbc-097a-4a64-9a57-be4a293ef77a)(content(Whitespace\" \
         \"))))(Tile((id \
         031a614b-ae61-4102-99f7-fdc9919ba231)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3c29043c-cb77-4409-baef-6de28e300ab7)(content(Whitespace\" \
         \")))))((Secondary((id \
         fe2970f2-c4ab-4bb6-a459-9b2fe1d0cef0)(content(Whitespace\" \
         \"))))(Projector((id 7bc62f96-a472-441b-86d4-b6ebb96d1e26)(kind \
         Probe)(syntax(Tile((id \
         24e61cf5-2caf-4904-ad28-8787744ab18d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         26b61323-c51b-4602-a493-09e9f0ea7c39)(label(fact))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e6a41a31-c805-487b-b3ec-1601c0cf0d69)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ad412450-5bee-4b0e-afcb-ab221b2fa6c0)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c411030f-c1e9-4054-b842-bb7293f06bce)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Tile((id \
         14e82764-ff95-4f30-9fba-250c15cf5ec6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         5b684b62-5448-4fa9-a927-97cff6be32a6)(content(Whitespace\" \
         \"))))(Secondary((id \
         08f3fb7e-fd03-4606-8031-6a39047b75fb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ab6e1b6e-33d6-4a0e-b270-a2f2a97f8811)(content(Whitespace\" \
         \"))))(Projector((id 11c1795a-591b-4e4c-9e19-b36f332493f4)(kind \
         Probe)(syntax(Tile((id \
         25901673-b309-4c57-80ad-22fd792e767a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f6d2a397-5a73-4a5a-9876-1577a5a44737)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5bfc6498-5130-4c42-af46-3de7ba8a89d2)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d6f58599-f7c4-455a-8c1e-1c54d85b0d10)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         956b50a0-b26e-44eb-8e53-f2580d05f25c)(content(Whitespace\" \
         \"))))(Secondary((id \
         86c4851c-d941-4603-a815-c803f1a0775b)(content(Whitespace\" \
         \"))))(Secondary((id \
         ec3146ec-5a59-496e-a549-646250de6ea3)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6c1ec845-5451-4133-b757-ca55cbf2a369)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         213896f9-690f-47c8-9c9b-24c52da90db8)(content(Whitespace\"\\n\"))))(Tile((id \
         faeaf9f0-316e-41e7-9384-1efa09102e23)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         103eba1d-d178-45b6-b621-f0ea2e095de9)(content(Whitespace\" \
         \"))))(Projector((id c72a9aed-3e0f-4ad6-8aa6-dafe61e48f42)(kind \
         Probe)(syntax(Tile((id \
         fb0894c7-8ae9-4682-911d-dfd78e1c73a6)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cc9b8113-9eb1-41f2-ab6f-6033f6e3d8d2)(label(fact))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da22fe5f-3b3b-4eba-89c2-d4a85b406261)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4bc94c00-975b-4339-97b1-632365595a3f)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         3036fd52-d75f-4067-935b-d0bc43d8ce2b)(content(Whitespace\" \
         \"))))(Tile((id \
         c242d2e4-6a55-4306-8c5e-68d429956300)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8c7f82e-55f8-4e29-9386-d7af01990db3)(content(Whitespace\" \
         \"))))(Tile((id \
         fe11699f-8c68-4172-8a6c-1cc6f41b8f04)(label(120))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         968c1406-af58-4a2c-9085-602e005d7249)(content(Whitespace\" \
         \")))))))))(Tile((id \
         bc042822-db33-4975-bdf2-67153aaba6f3)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ac2c4830-b991-40a5-af82-0270fe1475a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         211c8e1e-4f33-451c-b668-0e1c9eeb37f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         53e0642e-820c-4227-9956-a32a45e98334)(content(Comment\"# TAIL \
         RECURSION #\"))))(Secondary((id \
         0aab870f-55c3-487f-a386-0ac2a246a48e)(content(Whitespace\"\\n\"))))(Tile((id \
         1037419f-a866-42a5-b203-730cae2ca409)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d64e6660-9eda-4848-bc74-2a7463caf22d)(content(Whitespace\" \
         \"))))(Tile((id \
         858bdcf7-6f37-4369-8b50-32aebb71d407)(label(fact1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5fcb0c92-a452-435b-8ec7-57f4ddada494)(content(Whitespace\" \
         \")))))((Secondary((id \
         d83e0d34-a7d7-47f0-bd71-b949de6b635c)(content(Whitespace\" \
         \"))))(Tile((id 274c30c0-6eca-46ba-8f54-aec8d57c16ca)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 40))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         ca1da914-ef09-4c8a-81fd-3a8687fbf4b0)(content(Whitespace\" \
         \"))))(Tile((id \
         931723cd-f2c7-4b6b-af13-d8917a7dc165)(label(go))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bff88fa2-ec1c-45c5-9547-9c270ae16322)(content(Whitespace\" \
         \")))))((Secondary((id \
         b72adb55-6c5a-4442-a186-25d6d55e3c45)(content(Whitespace\"\\n\"))))(Tile((id \
         4d501eed-a4fb-4539-b976-0230b4a79ac7)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cdc00ce5-4267-436b-83ac-ef0b9c85c222)(content(Whitespace\" \
         \"))))(Tile((id \
         f29b5c95-e2f9-48d4-afa2-b67e0749f999)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Projector((id \
         d3ae29b5-f648-446e-87d0-21094ad14ad9)(kind Probe)(syntax(Tile((id \
         5e823251-4cec-4d7a-99b7-5ac8689b4c2c)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4a35fd8a-a335-4904-a19d-21f0b9c08d30)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\")))(Tile((id \
         2dae6b4c-34f8-42d0-b59d-a5dfb55d7a0a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         dcca702c-ae7e-49bd-a7bf-5a9a3b0625a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9abf3d0-2b61-4e2b-9859-2188b7376020)(content(Whitespace\" \
         \"))))(Projector((id fd91b1ec-aa91-4405-b212-3c849aab7e56)(kind \
         Probe)(syntax(Tile((id \
         be48bdbb-c641-47f6-b11d-39e41e2ae2ea)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         615525df-2f4a-4abb-a692-9c650293dad9)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\"))))))))(Secondary((id \
         a2294ecc-c1b2-4af4-a6f5-bc726a076d42)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6ef2d102-8acb-4ddd-a902-887f7beb5545)(content(Whitespace\"\\n\"))))(Tile((id \
         2f186bd9-3578-4a4d-8cfd-fbdabe52f29c)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a1c59215-2ca1-4db0-8d90-395122760377)(content(Whitespace\" \
         \"))))(Projector((id 7238f13a-4d18-49d3-93cc-d5c67a7cb7c1)(kind \
         Probe)(syntax(Tile((id \
         7bf59c28-8b0e-4faf-95d7-6372d1880ff7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         98e7546b-e656-4b47-8a35-2a9d5168d629)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         7cfd7481-96a0-4c4e-b356-ab313b966581)(content(Whitespace\"\\n\"))))(Tile((id \
         1ecbc9f7-479f-4ed1-abcd-e50591000014)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a6013378-0d96-4746-bc37-4a11095325cd)(content(Whitespace\" \
         \"))))(Tile((id \
         82ee1230-80d3-4be9-bef1-c916413a61ec)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e6a63dd0-9be3-4281-9cd1-7c46a80d7e35)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6c13db27-151a-4a8b-a617-62ca57bc8580)(content(Whitespace\" \
         \"))))(Projector((id 03773cfd-8f4a-44d2-b2a2-6a3db70a6249)(kind \
         Probe)(syntax(Tile((id \
         d7c72d19-86cd-4a40-998d-be934f88fc80)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8b2e95fc-700c-4c6c-af0b-18a256081433)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         88ff7c60-9b8f-465d-8ca2-6f240dbfe25c)(content(Whitespace\"\\n\"))))(Tile((id \
         0c7d42b0-180d-4a99-91bb-b46db3b63795)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         57b80ab7-e5e3-4eab-b4f4-93132c175ae2)(content(Whitespace\" \
         \"))))(Tile((id \
         d3f994ab-0ae0-4af3-97a8-23419f30bad3)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a5d6c34c-37a1-4a5c-947d-3261c7616289)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a6b6dd4a-4719-4cda-bb81-a914c4cfe1b0)(content(Whitespace\"\\n\"))))(Tile((id \
         f45decc2-b9ab-4460-aac7-7eb1f46dc5b5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         13a7f901-5201-4537-adb7-0141814f6409)(content(Whitespace\" \
         \"))))(Tile((id \
         91cf638e-8c3b-44ea-9d4f-e7ae151ad466)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         42752e64-7801-405b-a1e9-0dd99a95d5d1)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d47505e-9a59-4ba9-94cb-a5230440ed8c)(content(Whitespace\" \
         \"))))(Projector((id 069a1797-0199-4517-b8e7-495cc3ba7f16)(kind \
         Probe)(syntax(Tile((id \
         6cf1c845-dec4-431b-84b4-2f9d6b209454)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3d889595-15c6-43f2-bc0c-5be66555a8b1)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6c9c978c-87ee-41c1-b594-9814f943494f)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ad2c0faa-7c62-433b-996d-dc5a526a24b6)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         0c658c4d-2ffa-4b1e-8391-0c15a9f2facb)(content(Whitespace\" \
         \"))))(Secondary((id \
         e902f4c4-80d4-4a62-a3ff-a7ceaec0df71)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         fed4fcf0-a0a6-4b8d-b7fd-75fdfd22b9be)(content(Whitespace\" \
         \"))))(Projector((id 5f5d0390-fb81-416f-a12c-b7ba8f5a69e2)(kind \
         Probe)(syntax(Tile((id \
         b68d0ce7-6b76-42c0-bb46-535375ffe68a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         b3af13ed-f95f-4a75-ad05-d31eadb58cb8)(label(go))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         fc8ade99-f204-427a-a04c-dbff59303e6f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2827313c-50ab-4bfa-bde3-8eb69d0e89b7)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a8c3ed7a-af1d-47ce-b148-4afbe903f326)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1a353b46-499a-4f96-874f-10936d5412d6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b46c44d7-2a99-42ac-b3f5-a480f5fb2a9e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cfee8131-5b19-42d6-bd3d-d855ffb8e628)(content(Whitespace\" \
         \"))))(Tile((id \
         504a9030-146c-419c-a73b-ca9e7ab53d1d)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         b54b0dd6-48e3-4432-aced-d2cef1a1a06a)(content(Whitespace\" \
         \"))))(Secondary((id \
         e3138c81-9057-4a97-80df-07e674939569)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         710ccdce-67ef-439c-a508-a0f4a6e5088f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dc81ba1f-8766-41b0-9bcc-0cc0df276aed)(content(Whitespace\"\\n\"))))(Tile((id \
         f48f847a-4ab4-4756-9b98-c990761c1bb4)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b3fa14ce-8eb5-4f59-8abd-bb0f55da8239)(content(Whitespace\" \
         \"))))(Tile((id \
         91b27be8-541b-4c55-87eb-a351d70a6e92)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         241d96f1-852c-471c-b0fb-2c93e0598b36)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b1ae9ebb-06f7-433f-8d7b-7d7932b97edf)(content(Whitespace\" \
         \"))))(Projector((id 5dadfe7c-14ba-437a-97d8-b0f192560f91)(kind \
         Probe)(syntax(Tile((id \
         1e656a62-9443-4f39-b8ae-6a81a19516bc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         f5731f06-0eae-40f6-9ba8-10d806b65c31)(label(go))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2a92dd5a-5d32-4f24-bba7-15078ac38f6b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         987ff3dd-da24-41a2-9aab-341e210ba5a8)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2e75dd3-0fe7-4ee0-952d-a2499cab830c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Tile((id \
         ec8f97a5-43c9-48a5-842f-267943b7c711)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         3e5c11f8-cabb-43c3-ad31-d417b2311538)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3da9da9a-bfdb-43ac-a93b-fcbe54489db8)(content(Whitespace\"\\n\"))))(Tile((id \
         36ce329c-b0aa-4831-ad28-5fac6d020ed4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         13cc9259-35e0-4518-83c1-7c5872f38043)(content(Whitespace\" \
         \"))))(Projector((id ae791990-4bc9-4a68-9efb-6772f3da7cc0)(kind \
         Probe)(syntax(Tile((id \
         a4563bd4-6abe-4d76-a6f0-753b06869564)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         10fb8a16-9c8f-454e-aed3-9aa2eb1d9c30)(label(fact1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b8c77cc2-f714-448f-8523-c50b68f0eb23)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         50eed1a6-4378-4cf3-9d93-f4a8fc18bb4b)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         5c7a738b-6edb-4221-b06b-5e7f623cb4ca)(content(Whitespace\" \
         \"))))(Tile((id \
         5c2f42a0-92f5-4f8a-af71-fb4bddc0eae4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45987470-af55-4833-951a-93307bf3820f)(content(Whitespace\" \
         \"))))(Tile((id \
         dace8206-c5de-4748-80f1-77ae618b8f7b)(label(120))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9464d365-2c0b-4fc7-be1f-c90f031b7242)(content(Whitespace\" \
         \")))))))))(Tile((id \
         36014ce4-6375-466e-8842-3ed47d565f41)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5b69515d-2da2-485f-99ff-0c903275d70c)(content(Whitespace\"\\n\"))))(Secondary((id \
         330ec3d2-01f2-4a4c-95f1-317a0ac1ab76)(content(Whitespace\"\\n\"))))(Secondary((id \
         6d42375d-c373-4dd9-86e4-c4a3be5c284e)(content(Comment\"# FUNCTIONS IN \
         FUNCTIONS #\"))))(Secondary((id \
         dea124e4-3682-423e-8d41-758207a2fe28)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8aa4083-2874-4fef-b0d0-1beda8dac975)(content(Comment\"# The frunk \
         factory prethunks your frunk for later clunking #\"))))(Secondary((id \
         9ca052d9-b2a5-4b5e-9448-6461d05c1beb)(content(Whitespace\"\\n\"))))(Tile((id \
         49bac26e-78db-4c23-babe-40df7520b0ea)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f3a9663f-490b-4f59-ac1e-73e1606a60e1)(content(Whitespace\" \
         \"))))(Tile((id \
         d8a2d89f-829d-4434-ac3b-b956f6216b56)(label(frunk_factory))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d4a81841-9a35-4cb7-856a-3881cefc8f7a)(content(Whitespace\" \
         \")))))((Secondary((id \
         f35a9c73-0ef8-4f9c-a76c-2d48411256b0)(content(Whitespace\" \
         \"))))(Tile((id 2bc1a8b1-ca87-41ec-8ab9-ed4a55354392)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f6b7aa18-bcd6-4780-8058-ac91cf70699b)(content(Whitespace\" \
         \"))))(Projector((id 242229d0-1bcc-45aa-8f44-0d0e426d1af6)(kind \
         Probe)(syntax(Tile((id \
         7e979b99-e2ad-4cb2-b858-0cebbcce9550)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         83235ec7-7657-41fd-8595-888cde27f0d5)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         5e8ad5e7-7fd1-403c-a8c5-ce274b683168)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         76cf8e09-844a-4958-8677-071658249553)(content(Whitespace\"\\n\"))))(Secondary((id \
         060ef687-524f-4cc2-ae3c-808964e1b68e)(content(Comment\"# This is a \
         play area to explore nested function definitions \
         #\"))))(Secondary((id \
         84e00f6b-6777-4f2c-81c0-e9ef8f787a86)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc1fed9d-4b8e-4ff5-8847-5e180a957c3b)(content(Comment\"# and \
         functions returning functions #\"))))(Secondary((id \
         6d4cdc8d-def4-48ce-8808-631f31ed975c)(content(Whitespace\"\\n\"))))(Tile((id \
         e84d0746-243d-4990-9e3e-9cd5c21b5249)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         490ff44d-b0b5-4b5c-9db4-1bd07277a3b0)(content(Whitespace\" \
         \"))))(Tile((id \
         959ab076-5899-472c-90dc-61cd658d0f4d)(label(factor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         2040599e-ce7e-44e7-99e0-321b785b29ab)(content(Whitespace\" \
         \")))))((Secondary((id \
         a8b4addb-a067-4537-b461-bea353cd0d06)(content(Whitespace\"\\n\"))))(Tile((id \
         f9714b19-b197-4abb-99f4-e8edcd94677d)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a615738c-9afa-41a2-9002-72a9ea8c29f0)(content(Whitespace\" \
         \"))))(Tile((id \
         2d5f267c-d9ba-4e25-8f44-8713124857cf)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bfc17393-ac11-4461-a50d-6a4c55bbf21c)(content(Whitespace\" \
         \"))))(Projector((id 535c73cc-a66b-4bbb-8a89-dcf01609607c)(kind \
         Probe)(syntax(Tile((id \
         96bb6bb0-20c0-4415-aaef-e1064221c289)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fb83656a-3fb5-4e7f-9765-c9bd09e2a633)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2a93350b-1ce4-4d3a-925b-b362631665fa)(content(Whitespace\" \
         \"))))(Tile((id \
         c3f4d5f2-b557-4069-ba17-61355c5783c6)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f76e7020-237a-4aab-84ce-397a6bf64904)(content(Whitespace\" \
         \"))))(Tile((id \
         6903185c-be8c-4af9-a92f-2f49ff7e9e68)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         f52fc2dc-9494-4c6f-acd9-86b626b03c68)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         403a136c-38a0-42b0-953e-5a6ff7c9faf7)(content(Whitespace\"\\n\"))))(Tile((id \
         f1df1899-4825-4867-8168-399cd8737f67)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a6667f05-ef35-446e-8cf5-f82df949ee2d)(content(Whitespace\" \
         \"))))(Tile((id \
         32e89b5b-d442-4f03-933f-15aee64ab3e7)(label(refactor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6321d5cb-d083-45f3-a5f6-78cb40b0fe46)(content(Whitespace\" \
         \")))))((Secondary((id \
         92a088e9-8806-495b-ad3f-078b78d9b7e8)(content(Whitespace\" \
         \"))))(Tile((id f4822dbc-879f-45af-a09b-f77142dbd52c)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         e6be2051-972a-4db9-918a-7ead4d538ab4)(content(Whitespace\" \
         \"))))(Projector((id 8eb45cea-387c-47b9-ba66-3d786494d27d)(kind \
         Probe)(syntax(Tile((id \
         5e6ca7dd-22c1-42cb-b35a-3b69a5c31926)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         d66fbae9-811a-49e3-bba7-0e42313620c7)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         46ab1a48-9ad9-445e-9962-130ed08696ce)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5a4fe8fe-aec3-4b7d-9e31-1ecd57d4d5b0)(content(Whitespace\"\\n\"))))(Projector((id \
         3d4a52ed-e87b-4ec6-8229-acee885b345d)(kind Probe)(syntax(Tile((id \
         34243513-2ff1-4820-b314-dd27ec98edc3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         77252c36-d6de-478b-bfcf-0ab51e35af9a)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1eb7afc-768c-4a0c-b721-063bfceeaf99)(content(Whitespace\" \
         \"))))(Tile((id \
         799e18b5-721d-4d06-8d97-a6b5e8c4dd49)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         274aa318-ebe2-4976-80ab-39b1d9ef46bc)(content(Whitespace\" \
         \"))))(Tile((id \
         67dec197-13ee-4c61-9c31-464e0ffcffa6)(label(factor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         589b7827-c02a-4880-9c3d-d303a132888f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b5c88f09-a145-4eae-ac56-15098a903f7d)(content(Whitespace\"\\n\"))))(Tile((id \
         7ddf2c6c-c99f-4413-856f-605d11fcd9a1)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8a3c4af6-9108-429f-b12e-d6f6e2d119e9)(content(Whitespace\" \
         \"))))(Tile((id \
         d889ef82-a55c-4f1e-aa5d-a1da6b18fdef)(label(factor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         07a52ab8-2618-4a33-b3c6-084186f66b5c)(content(Whitespace\" \
         \")))))((Secondary((id \
         0724ada3-c123-4b03-b1da-fd1e025149ae)(content(Whitespace\"\\n\"))))(Projector((id \
         3d40609f-09b4-4e27-8d05-3ba04990bd4f)(kind Probe)(syntax(Tile((id \
         6947c6e1-8bde-4a47-8b1b-ba673ebdd7ba)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9be4180e-59a3-42c6-9400-9630362b0fe1)(label(refactor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         826c9248-7b75-4b2c-812e-f04f48119ff0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         701f746d-58c6-4a2a-93c1-bdbb2c5bbce2)(label(factor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         f508a122-ff50-46b5-aa98-6943accd5cea)(content(Whitespace\"\\n\"))))(Tile((id \
         fbb46aed-cf8b-4424-a216-7f3ff2161006)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1574517-15ef-4f9d-93be-17244c1d6006)(content(Whitespace\" \
         \"))))(Projector((id dcb88ad6-154d-4cad-beac-2a75b8318962)(kind \
         Probe)(syntax(Tile((id \
         7d211858-880d-44d2-b189-8f0bcd5959c0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         db8bb542-fa4d-4239-8f1b-d93d2f8ed98a)(label(refactor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b42ae291-831b-41f3-8eef-14ec84956767)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8974f50d-4bc6-488c-bcb1-edcdd0050c23)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         fd14679f-6f01-4881-b18e-cf72fda933fd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9a3c1baf-23d8-436b-ac8f-4d005715f007)(content(Whitespace\" \
         \"))))(Secondary((id \
         29ef74f5-2fd0-42d8-a84e-03479564b1db)(content(Whitespace\"\\n\"))))(Tile((id \
         1dd1c432-a466-4056-9f41-22cfc09869af)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3b8cee1e-4adb-4b7b-9c49-50b405689cc5)(content(Whitespace\" \
         \"))))(Tile((id \
         2667b289-e732-419c-b55e-c01ed6c89d45)(label(perturb))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         98966716-578b-4b11-94f4-7713ea0fe98b)(content(Whitespace\" \
         \")))))((Secondary((id \
         aa9f0a82-4e21-4669-a0df-6c1d38b67b9b)(content(Whitespace\" \
         \"))))(Tile((id cc070ecc-db92-4710-b39a-a9f4e06a23e5)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         0e0fb4ea-040a-420b-a4f9-72f2766f8384)(content(Whitespace\" \
         \"))))(Projector((id e3e78c0f-ee25-46cc-8574-eb5f8dc5677b)(kind \
         Probe)(syntax(Tile((id \
         1d20f589-0487-4d04-adc1-b29b17b6cde2)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         8fc2bc52-ec89-4396-af79-557d4233f7fc)(label(s))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         e913178d-ea9a-483b-bf38-193f863a1d17)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         490e5223-20a7-4ed9-9b7b-75b50fa97a60)(content(Whitespace\"\\n\"))))(Tile((id \
         d531dec6-5010-48be-ba49-86861aed5274)(label(factor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         569ce6f9-9edb-4a02-a793-5c95679ce4b8)(content(Whitespace\" \
         \"))))(Tile((id \
         4d42a467-9d77-478c-8e3e-027fa4fb5dd5)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         920ec730-5043-4540-9f6f-62f9094d1b34)(content(Whitespace\" \
         \"))))(Projector((id 4b4bf939-4d1f-4ca3-a7b7-7636e25f64ea)(kind \
         Probe)(syntax(Tile((id \
         e03aec1a-e3e4-4ea4-a1ca-21e909418c8f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         fd3f5fea-75b7-4087-9913-17741b2f3436)(label(refactor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c9b960c5-f263-40b9-afdd-d81008cc77d9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         953ec0c7-dd3f-4edb-9de0-9285da796683)(label(s))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         28266125-4353-4590-8655-bcb2039c2f08)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ce86b0dc-8698-40ff-9079-36caedc4cbde)(content(Whitespace\"\\n\"))))(Tile((id \
         4092c9c5-ea17-417a-a6dd-dc7447c24310)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         60919ceb-f86b-48c5-9e06-9e7da8c63628)(content(Whitespace\" \
         \"))))(Projector((id 06df9174-eefa-4523-8475-5259d898e226)(kind \
         Probe)(syntax(Tile((id \
         cb07ec3f-59cf-498b-8452-4b73bede2815)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         6a2cf44f-991f-44a0-a6cf-04577750d1bb)(label(z))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))))))))(model\"()\")))(Secondary((id \
         7e892ebe-72fd-40a0-a8e1-2ab686b7902b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         46868042-26e0-4e1b-ab31-d8ffd72a7c6b)(content(Whitespace\"\\n\"))))(Projector((id \
         34babf35-4fa6-45d9-8b66-2b1dd7f9522a)(kind Probe)(syntax(Tile((id \
         7595d7ae-c7e7-4523-9c1c-1a93ce917b19)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         e635c8ad-894e-4531-bc72-55f2396e7c46)(label(perturb))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         67206723-e6e1-443a-87c7-eb058d7ce418)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fc46ff9b-ad55-4532-a0d5-d4013a4cd4ce)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         40095422-39c5-442f-85c8-b48538dcbaa6)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         4fe24ca3-ec9e-4d7f-83a4-00a769223705)(label(z))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         0c8982df-6b07-485d-bc7c-9771fc6081bb)(content(Whitespace\"\\n\"))))(Tile((id \
         65f81315-23cd-48a8-bbae-33965effa396)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         acd30304-eb72-4ad5-8a69-998225078632)(content(Whitespace\" \
         \"))))(Projector((id bf6ed5f2-17f9-4054-9dbc-b692496772ed)(kind \
         Probe)(syntax(Tile((id \
         742ec05b-dd1a-4489-8e67-0e2d6e48fc7a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         75b7eb59-bb93-4086-a6c4-94de2cbc28e6)(label(perturb))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2694b312-577a-425c-aabc-e1a713d48348)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         559e199a-7e29-41ee-8246-31f64e327ac8)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         98071f6b-3de9-4d9f-ba24-8e17570d7264)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         76c6bd10-1fe4-4f00-b764-72cb7b98f49c)(label(z))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         59b2ecd7-0d0d-4409-9b59-1696bbc58a5c)(content(Whitespace\" \
         \"))))(Secondary((id \
         b1f4efc2-dd9d-44f3-b57b-eb47c98d2836)(content(Whitespace\" \
         \"))))(Secondary((id \
         48c8036f-ca04-40c0-bf05-ecbebf0cbcaa)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1d505e3b-e40f-41e6-8a41-571611d5c891)(content(Whitespace\" \
         \"))))(Tile((id d8a13356-385f-4b4b-be52-3f5851b8caff)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 40))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         cd0705a7-8d7d-4d73-8ef9-fc6d8e4b1bf9)(content(Whitespace\" \
         \"))))(Tile((id \
         56ea8411-5b03-4960-b603-d947593c66ae)(label(new_frunk))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5e277d9d-40e4-4dd3-9f90-3052148beb65)(content(Whitespace\" \
         \")))))((Secondary((id \
         976310cc-ab22-457d-83d8-ea1b2b8116e7)(content(Whitespace\" \
         \"))))(Projector((id c3ccf50d-db8e-4325-bb48-60300162d816)(kind \
         Probe)(syntax(Tile((id \
         d42cd938-8a39-4225-b870-c8bed2943d28)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         02913790-5d44-430e-8b9c-8c533910d0b5)(label(frunk_factory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         eb72cd1c-5053-4591-a810-e29ae9dcbf08)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         47d07e1f-83b4-4924-b737-bc5f85436c82)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         ce6999c2-b25b-4e9f-861b-520c7206a231)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ec6b2496-7d93-41e8-af4a-6c3a5c0e7a5f)(content(Whitespace\"\\n\"))))(Tile((id \
         ac429488-232c-4f59-a4a9-afff7b900cd8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b9cee0d6-6a3a-4d37-b21c-bd4f28a11a41)(content(Whitespace\" \
         \"))))(Projector((id 82ef6f1f-4914-415c-9ad8-dcad26229a80)(kind \
         Probe)(syntax(Tile((id \
         6a503b5b-f733-4f50-93d7-f255042607ed)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a719f534-517a-4efa-9b6b-b3ab53010786)(label(new_frunk))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         772fa7d0-1e20-47ed-a4a5-1a3f48fad011)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0011ab75-e214-4365-b86a-8ad034157cb8)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         288d87d8-6aad-49c1-a09c-b610965d6e33)(content(Whitespace\" \
         \"))))(Tile((id \
         08f6239d-952a-444b-8c01-d837eff87c25)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25bbb41d-f5bd-4757-9199-996b45f7b681)(content(Whitespace\" \
         \"))))(Tile((id \
         6cf3e2bb-1cd3-4c49-bb8d-e2a181cb66f4)(label(314))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f282250-4956-4099-a9d5-1153e939bace)(content(Whitespace\" \
         \")))))))))(Tile((id \
         82bbe3c4-7664-4f3b-b738-ec013370f914)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cb186c6a-baef-4e41-aff8-82ba4967be2b)(content(Whitespace\"\\n\"))))(Tile((id \
         f1698b4a-101d-472d-af9b-cf5642688114)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         86abd306-a516-4307-8ac3-542e932a84b7)(content(Whitespace\" \
         \"))))(Projector((id 8612402e-2d66-440a-a493-8a2bdb1b068b)(kind \
         Probe)(syntax(Tile((id \
         94d26244-713e-420b-84a4-365ea7cd6f32)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         18b8c4b2-6cb9-4a43-b5bb-9b180ed87125)(label(new_frunk))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         28a80acd-ee09-4f3f-924a-518f7df03cc3)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f5cc0394-25d6-4fb0-887c-bd65cbbf0fce)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))))(model\"()\")))(Secondary((id \
         37b01a9b-5154-45b8-9f9d-964a54e58147)(content(Whitespace\" \
         \"))))(Tile((id \
         8f05c3fd-73f0-4e95-85a2-8ccfdf6bf35c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b9ab557-31f1-42b4-9d7d-bf2d4e7266bc)(content(Whitespace\" \
         \"))))(Tile((id \
         d0e0afd5-3108-40b5-ae08-0b726c7c6186)(label(330))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8692054f-4534-47c8-8838-ec963589b8f8)(content(Whitespace\" \
         \")))))))))(Tile((id \
         6de9aede-e203-4919-8b7c-419b614053fd)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0e18e706-103e-4782-9dc9-83f1276b4c03)(content(Whitespace\"\\n\"))))(Secondary((id \
         42b4f066-cadf-4c0e-aaea-72b3f97500aa)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb6d7e8a-196a-417f-a45b-da184d4b0565)(content(Whitespace\"\\n\"))))(Tile((id \
         ebf308a5-34b7-4871-bf24-064be6a2731d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f9948103-e4fa-4806-b498-d44c0b6fab12)(content(Whitespace\" \
         \"))))(Tile((id \
         a10912f8-5fbb-49c0-b178-c38b3c999b2c)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         22edc12f-0a39-4e50-b3ef-ebd55e6692a2)(content(Whitespace\" \
         \")))))((Secondary((id \
         ac96dd29-2a36-4889-a9b3-75b5c63ab1fa)(content(Whitespace\" \
         \"))))(Tile((id 259ce7d0-f3ef-40f2-b806-7bc2a1f95724)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 40))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         7545d141-2ccc-4bf3-a125-37f3c6424675)(content(Whitespace\" \
         \"))))(Tile((id \
         82c1e6e7-8462-428a-bff4-53dee48f305c)(label(fib))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         04c1f1c6-3eeb-4539-a257-3e38a1d0adf4)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2308914a-8387-423a-b7b8-47b36be834d9)(content(Whitespace\" \
         \"))))(Tile((id \
         aa764695-cf75-4b50-b159-2e2ba9c44140)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         076f9750-b0cd-4dea-999c-fa3598117f8c)(content(Whitespace\" \
         \"))))(Tile((id \
         5819ab86-1f4a-4a54-a5d4-fa9844a3aaeb)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d6fa6c8d-657a-49f3-8285-df6db17dfbc1)(content(Whitespace\" \
         \"))))(Tile((id \
         fc1c61ad-3d81-4c1b-a3ed-71fc12aaf24f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e7c7b9e5-813e-421b-9f87-fef9649227db)(content(Whitespace\" \
         \")))))((Secondary((id \
         6704fac6-dbfe-48f2-86ac-bcffdae0580a)(content(Whitespace\"\\n\"))))(Secondary((id \
         da06bb44-1350-4bbe-a773-34be86480ed1)(content(Comment\"# Recursive \
         calls can complicate probe display due #\"))))(Secondary((id \
         ed7617f7-9a2b-45cf-b5d2-c404f67c52ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         291e5c0a-e3bc-47a8-a50d-b1d1ce4917fd)(content(Comment\"# due to \
         overlapping information channels.#\"))))(Secondary((id \
         691d2867-a26b-4b38-9977-aa31c872c2e5)(content(Whitespace\"\\n\"))))(Tile((id \
         08e39d56-de1c-46ad-9329-60e5cb4942b4)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         36))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         190258f1-148c-4fa8-856a-a075d53d0e59)(content(Whitespace\" \
         \"))))(Tile((id \
         e8f8db93-f6c8-452d-b179-93ae3dfd7a3c)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7bbab4b5-9755-41f6-9ad2-057c3813dd21)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         82356e38-8f49-4ba2-8fc8-31573e7e8cab)(content(Whitespace\" \
         \"))))(Tile((id 45213a90-f2bd-4c51-b3eb-a896c2a6499b)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c60cb75e-fe5f-4c1b-b17c-0c8c4b450152)(content(Whitespace\" \
         \"))))(Tile((id \
         b447e2bc-58d3-40a8-b280-4195ddf400cd)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f15e9636-93fb-4644-b5b3-e0120b7d6916)(content(Whitespace\"\\n\"))))(Tile((id \
         3d5839ff-bd24-4565-b0a1-79a6c31a6409)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0542a356-fb43-4402-982c-d72be0bd4108)(content(Whitespace\" \
         \"))))(Tile((id \
         2d70e17d-341a-4c42-9dc0-7836bd147649)(label(0))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         eca22ff3-8c8b-4642-940d-030165a80fd6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a50ec24-4622-41ad-a662-430e1023a068)(content(Whitespace\" \
         \"))))(Tile((id \
         4a2c7532-9264-4e6e-a583-70a7fc94ac25)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4e1d94cd-a26c-4948-bc2d-c10cb5934cf0)(content(Whitespace\"\\n\"))))(Tile((id \
         ad0e9836-0f3d-4279-9f11-b9abbc6b0811)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1e83c823-548e-4d2b-8e5c-882e87324935)(content(Whitespace\" \
         \"))))(Tile((id \
         e2430a14-5849-4216-bb8c-431d95fd820b)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9662b9f4-145c-4777-b50a-2d9c9a2f3dc5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f6b1f0c8-c4bb-4714-b16f-8f44d3473dfc)(content(Whitespace\" \
         \"))))(Tile((id \
         b60e85a3-669e-4af5-8acd-23f52908b19a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9addbc22-8fe4-46f6-849a-0c0562e6c6a3)(content(Whitespace\"\\n\"))))(Tile((id \
         e00ade9e-b26c-4b23-9778-e28d25b16dc8)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 43))(sort Exp))((shape(Concave \
         43))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         0feb49cf-247d-4b39-bdf3-0dbe33aac97c)(content(Whitespace\" \
         \"))))(Tile((id \
         3aa76315-a75a-47ab-a623-d9d950fed108)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         90b490d5-61b5-4e2d-baee-079a27290c21)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a2bbc997-75df-43d0-ad8e-c6ea014f8a27)(content(Whitespace\" \
         \"))))(Secondary((id \
         aae853b1-eca3-453c-8f88-a7769a811bbb)(content(Whitespace\" \
         \"))))(Secondary((id \
         660f4502-9819-419b-912f-6e64814c2d0e)(content(Whitespace\" \
         \"))))(Secondary((id \
         cc606b6e-5fe1-4113-824a-cc1db7014656)(content(Whitespace\"\\n\"))))(Secondary((id \
         19fa7109-22cf-4c27-b3c6-05bb1be282a1)(content(Comment\"# Select the \
         first `1` below: #\"))))(Secondary((id \
         9d11b749-5702-4823-9d4e-c63366861bc8)(content(Whitespace\"\\n\"))))(Tile((id \
         bf090a3c-23c9-4801-b4b3-16e54fe21ae6)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         969478fb-f2c6-49bb-9d49-bfefa775775d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fd56b6db-5869-440b-9a0a-e9ebb226d744)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         57ea6961-e5fd-493c-8dfd-3503c3a154ee)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Tile((id \
         a122ee98-7477-4df8-87be-0e46ec0a1015)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4746dca9-53dc-4c00-94a8-7b186a553cba)(content(Whitespace\"\\n\"))))(Secondary((id \
         742a1d41-b6fc-4cf8-ad64-fef37f854604)(content(Comment\"# Note the \
         purple `2` below corresponding the call #\"))))(Secondary((id \
         82d54329-2c5e-42ae-9572-3db2ec6d1af3)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf46ff4e-fe11-4462-ae1e-9edf8d8c6403)(content(Comment\"# fib(4-2) \
         which contains the above `1`. The `1` below #\"))))(Secondary((id \
         fb96b8c3-f4a5-4f5e-8edf-eecf67266c8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         d28ca590-220f-4306-9054-7ba84b834d17)(content(Comment\"# OTOH is \
         highlit because when the above call was made, #\"))))(Secondary((id \
         616e20ce-53e1-4a98-8d76-89b209433580)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa6f63b5-b43f-47c4-b01a-570e76ca4750)(content(Comment\"# the call \
         below had that value. The two `1s` outline in #\"))))(Secondary((id \
         253a36c0-9567-4f9a-9146-4e031c2b5db1)(content(Whitespace\"\\n\"))))(Secondary((id \
         3042b4df-a3cd-4f61-a779-9418e543464d)(content(Comment\"# purple above \
         come /from/ the indicated call, whereas the #\"))))(Secondary((id \
         5e603014-00dd-4705-92a3-6d57f7266d64)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a675591-692a-43f8-8ac3-f1a6822bf23b)(content(Comment\"# highlit `2`s \
         are from the /same/ call the indicated call #\"))))(Secondary((id \
         f2ee88e1-82b0-4b20-a340-e2a7fb192773)(content(Whitespace\"\\n\"))))(Secondary((id \
         83b6638c-afaa-41e3-862c-e729e587117c)(content(Comment\"# was \
         evaluated in. #\"))))(Secondary((id \
         79c25b72-7a36-4bdb-b2b4-f0b4b445674b)(content(Whitespace\"\\n\"))))(Tile((id \
         4fb3a612-8c85-4b64-a54d-9d8945dae3c8)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f68632f3-de64-4384-9ca3-9c67e10849ce)(content(Whitespace\" \
         \"))))(Tile((id \
         0d6af880-6e35-4158-8b17-3e06641be1a5)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         53c37d22-9bc8-45b8-8922-490a01b60591)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f99559d9-d5de-47b4-97b0-dcbe1f9fd583)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e125f2e6-6f9d-43da-ab2c-019904387632)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Tile((id \
         be47f463-ad41-4636-9eac-1098f35dcf19)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5f126523-aa74-470f-87ea-77d17526dab5)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4f45722f-f2e4-46df-8b0b-07aef94e1f04)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ee724f04-2595-4595-819a-e39d8cce5272)(content(Whitespace\"\\n\"))))(Tile((id \
         824b7f83-3a4d-4e76-bb78-9bbd11e658a2)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b6526bcf-8918-4ded-a0f3-cbe55c5bd885)(content(Whitespace\" \
         \"))))(Tile((id \
         6edb49b7-669b-4bf8-96ed-8e2853d30055)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         efcd0d68-8ae7-4917-bd8e-b802c19bb8c4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         390748de-a0fb-41fb-a24b-6078df3e4662)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         41b9cca3-e038-453d-af5e-1f12d0ded715)(content(Whitespace\" \
         \"))))(Tile((id \
         c80a3c34-8a4e-4050-b7f9-4418d18a8ff7)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         457fe0df-60a7-45fb-9dec-664188dfad0f)(content(Whitespace\" \
         \"))))(Tile((id \
         88e04c9c-cd80-4105-aa21-90112ebda8ef)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         97a5e58f-bd1e-482c-80d3-6015308f76b6)(content(Whitespace\" \
         \")))))))))(Tile((id \
         288a95dd-c09b-44f3-9c90-0e7189039ced)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         11e74ec4-6f01-4b59-878e-8a63b0f3643a)(content(Whitespace\"\\n\"))))(Tile((id \
         5fec9883-7602-4cd7-aa6f-76a2b41ce189)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         fb3b7264-89a3-4525-8795-54b982d7a4eb)(content(Whitespace\" \
         \"))))(Tile((id \
         4a31ade5-77f1-4aae-acd1-e9069e931275)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8a4099c8-4740-4912-9cc5-e82a42d562ca)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b5b8a0ce-c3b3-4837-97ca-7ccae11649d1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         63c10ee8-5f1a-4d7a-a718-2846a85c14e0)(content(Whitespace\" \
         \"))))(Tile((id \
         a79f1a6c-ec20-49c5-84e0-7309051bdbe3)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         69a8a870-85f7-49e7-a7e5-964fce2cadde)(content(Whitespace\" \
         \"))))(Tile((id \
         8fbe457e-9950-4e1d-9bf2-d193d05068f7)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         903f143b-deb8-4b3b-bf65-9928e7a2967a)(content(Whitespace\" \
         \")))))))))(Tile((id \
         e66ba948-253a-4efd-990a-af49205cac15)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         670522c2-9af0-4223-aff4-b9956dc65776)(content(Whitespace\"\\n\"))))(Tile((id \
         bba9408c-c93a-463c-b275-7df6f5875b7c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         b7ffca54-0724-46f8-b6f6-c46e8c3ab07c)(content(Whitespace\" \
         \"))))(Tile((id \
         ec085b97-be31-4ba0-8deb-2ab3dec17965)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01b8885d-2bb0-4d1b-89df-d1723fcc919f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         897d17fe-2c5f-4fe3-b9fe-0e7f6cf176e6)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1ee639dc-3623-4e87-8070-aa310582c81e)(content(Whitespace\" \
         \"))))(Tile((id \
         8e5f82b4-11b6-4aec-b0bb-bb7a5ae7d7fb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e15e68b6-4b8c-4172-abf5-d07e2af1bbb6)(content(Whitespace\" \
         \"))))(Tile((id \
         13261013-4bfa-4169-9179-a3e0cc2b3bb9)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a7b9afc0-eec6-47b3-bda2-06d624892036)(content(Whitespace\" \
         \")))))))))(Tile((id \
         bdb682de-1954-4c37-aaaa-3285fa61f92b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         17499cc2-70a7-46e2-8c0b-546ed5c5dab5)(content(Whitespace\"\\n\"))))(Tile((id \
         f80c39ec-ac92-4230-8f45-1138676ecc03)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         05cf9261-a9c2-4ed8-9fc1-28a363083a01)(content(Whitespace\" \
         \"))))(Tile((id \
         5c718870-417a-4bb2-b3a7-98dbd089104e)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4fe7caa5-a1ba-4d21-84b8-0d476f200796)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         35de7f36-35b1-4c5d-a871-52533b5d8a59)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ef292f64-f9cb-4a42-aa98-507106a633ff)(content(Whitespace\" \
         \"))))(Tile((id \
         1c09dcb1-4574-42c6-b6c6-e474b445e3f8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f0884565-27b2-405c-a6d9-73e393293b5e)(content(Whitespace\" \
         \"))))(Tile((id \
         021abc8d-0886-4554-b0f3-011b776b6802)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         965e3071-9bb7-413b-9ae8-fdbc08fff3ba)(content(Whitespace\" \
         \")))))))))(Tile((id \
         c6f971d7-ddb1-46e1-a5a1-3301062cf21e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         edb4489e-4647-44c5-b66c-d83290ff5584)(content(Whitespace\"\\n\"))))(Tile((id \
         861bde97-1a8a-4574-b734-af2e54e13892)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         96a44435-d1f7-4eef-adcc-99739cb74df2)(content(Whitespace\" \
         \"))))(Tile((id \
         5c697604-e8ac-41be-a763-d713df2390f2)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         842bacd1-013f-4385-91aa-651cc90a073b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         14942527-410c-42a3-a301-cb01d824ba14)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2bb0bfaa-cff2-40ff-9206-6ccaa6013d8e)(content(Whitespace\" \
         \"))))(Tile((id \
         11b3b773-c905-4efd-8ac0-c9ca109a2447)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72f17ef7-13a9-4478-82a9-59c4928bd8e7)(content(Whitespace\" \
         \"))))(Tile((id \
         99ee73d9-0e5e-4b6e-8c32-075a12eada41)(label(8))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         355cb7dc-bb23-4547-a4a4-ef15395c4215)(content(Whitespace\" \
         \")))))))))(Tile((id \
         d18a643b-7acb-4128-990c-781f0dc033de)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f43c47ae-0fde-4fe6-8200-24677920bcf0)(content(Whitespace\"\\n\"))))(Tile((id \
         77e7cc2b-2183-48aa-84f1-f5fb91f9e679)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d12d85a6-53fa-4e97-ade3-6fd6d1e0fe57)(content(Whitespace\" \
         \"))))(Tile((id \
         301ff24b-3127-45c1-92c2-570dfd4180bb)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         72a9b1b8-55a7-4cf8-b254-42b8c12105de)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         95c95c86-995a-4512-88a4-eaa821bab9e2)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         205e7ed8-df98-45f4-9756-c03989ae05c8)(content(Whitespace\" \
         \"))))(Tile((id \
         3024c5cf-ca7a-4d84-ac70-f0ad440e9602)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6cdc462d-440e-45f8-b3e5-773731d52c49)(content(Whitespace\" \
         \"))))(Tile((id \
         99ba2f3e-6d0a-4eb9-8e6c-802b0d242d5f)(label(13))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         42f5c324-8210-4695-aecd-f55b74df7970)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         07aef8df-0d39-485d-84d9-d604fbbb4bb5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e5da75f6-3b29-45d6-9f10-f70cdf74e844)(content(Whitespace\" \
         \"))))(Secondary((id \
         e7f5b189-1f7d-4ffa-84dd-13d484d8eba4)(content(Whitespace\"\\n\"))))(Secondary((id \
         5df19ec1-4b86-4544-bfff-200538657925)(content(Whitespace\"\\n\"))))(Secondary((id \
         9aa000ea-b607-4d76-b95e-dfbc7292763a)(content(Comment\"# ADVANCED \
         FEATURES #\"))))(Secondary((id \
         c69f3b3d-afb5-4d73-b72e-e5f60c7f0886)(content(Whitespace\"\\n\"))))(Secondary((id \
         e304eca9-e4ea-41fa-8ded-37bc3b64247a)(content(Comment\"# - You can \
         resize a cell by holding shift and dragging horizontally \
         #\"))))(Grout((id c6d59965-e3c8-4647-b0ed-587226496ea7)(shape \
         Convex))))))(ancestors())))(caret Outer))";
      backup_text =
        "#  _____           _                #\n\
         # |  __ \\         | |               #\n\
         # | |__) | __ ___ | |__   ___  ___  #\n\
         # |  ___/ '__/ _ \\| '_ \\ / _ \\/ __| #\n\
         # | |   | | | (_) | |_) |  __/\\__ \\ #\n\
         # |_|   |_|  \\___/|_.__/ \\___||___/ #\n\
         # INLINE EVAL WITH PROBE PROJECTORS #\n\n\
         # INTRODUCTION #\n\n\
         # Probe projectors are a kind of inline evaluation, #\n\
         # similar to value hints in Emacs or IntelliJ. #\n\n\
         # You can put these on any expression or variable binding to #\n\
         # see a list of all values taken on by that expression/pattern. #\n\
         # By default values are sorted by left-to-right by most-recent. #\n\n\
         # More generally, each cell represents a stack state, #\n\
         # including the top stack frame / closure and hence the #\n\
         # expression's value, the values of environment variables, #\n\
         # as well as the surrounding call stack context. #\n\n\
         # When a cell is selected, you can hover over it to see #\n\
         # relevant environment variables, and all /other/ cells #\n\
         # are decorated according to their relative position in #\n\
         # to the selected cell. in the context #\n\n\
         # Probe are intended mostly as a println replacement #\n\
         # for exposing intermediate values, with the above decorations #\n\
         # as a supporting feature to help maintain context when #\n\
         # navigating between multiple probed expressions, which #\n\
         # may take on many values across nested or recursive functions. #\n\n\n\
         # TUTORIAL #\n\n\
         # The expression (10 * 10) below has a probe.  #\n\
         # Its value, 20, is shown in a cell to the right. #\n\
         let chips = ^^probe((10 + 10)) in\n\n\
         # To probe the below expression, put your caret to #\n\
         # left of the `(` and press option/alt-v (for value), #\n\
         # or select `Probe` from the lower right corner menu: #\n\
         let mult = (1 + 2 * 3) in\n\
         # The expression should be encased in a green block #\n\
         # and a cell reading `7` should appear to the right. #\n\
         # The same shortcut or menu toggle removes it. #\n\n\
         # Click the below cell (with value 21) to select it. #\n\
         let score = ^^probe((chips * mult)) in\n\
         # Notice when you hover over a selected cell, it #\n\
         # shows the values of any contained variables. #\n\n\
         # Probes only have cells if the are evaluated. #\n\
         # Below, only the first case branch is evaluated. #\n\
         let check = case ^^check(true)\n\
         | false => ^^probe(\"checks out\")\n\
         | true => ^^probe(\"you cheated\")  \n\
         end in\n\
         # Note the 2nd branch probe has a zero to the right. #\n\
         # This is the cell's collected closure count, i.e. #\n\
         # the number of times the expression was evaluated #\n\n\
         # Probes can be placed on expressions: #\n\
         let pow = ^^probe(50 ** 2) in\n\
         # And also on patterns (e.g. variables), shown in blue: #\n\
         let ^^probe(pow) = ^^slider(54) ** 2 in\n\
         # Expressions currently CAN'T BE EDITED WHILE PROBED #\n\
         # So probing a name instead makes iteration easier. #\n\n\n\
         # FUNCTIONS #\n\
         let _ =\n\
         # Because functions can run multiple times, they can #\n\
         # have multiple cells. Note the closure counts below #\n\
         # are all 2, indicating each probe was evaluated twice. #\n\
         let celsius = fun ^^probe(farenheit) ->\n\
         # Click to select the cell above reading 72.5 #\n\
         let diff = ^^probe(farenheit -. 32.) in\n\
         # This highlights cells below corresponding to the same #\n\
         # function call: the cells reading 40.5 and 22.5) #\n\
         ^^probe(5./.9. *. diff) in\n\
         let (t1, t2) = 72.5, 103.1 in (\n\
         # It also highlights in purple the cell #\n\
         # of the function's call site#\n\
         ^^probe(celsius(t1)),\n\
         # Now select the cell above reading 22.5 #\n\
         ^^probe(celsius(t2))\n\
         # Note the 72.5, 40.5, and 22.5 are no longer green-highlit #\n\
         # as they are not part of the same call as /the expression/ #\n\
         # `celsius(t1)`. However, they now have purple hats, indicating #\n\
         # they are below that function call in the call stack #\n\
         ) in\n\n\
         # BRANCHING IN FUNCTIONS #\n\
         let cases =\n\
         # Select `6` then `5` then '4' below: #\n\
         fun ^^probe(x) -> case x \n\
         # Note how each activate exactly one branch below: #\n\
         | 4 => ^^probe(true)\n\
         # Select the `5` above and then the `false` below: #\n\
         | 5 => ^^probe(false)\n\
         # Note the same things are highlit as both cells are #\n\
         # from the same call to cases#\n\
         | _ => ^^probe(true) end    \n\
         in let _ = (\n\
         # Select `true` below and then the `4` cell #\n\
         # for the argument x to `cases` above. #\n\
         ^^probe(cases(4)),\n\
         # Note how the same cells stay indicated, but the kind #\n\
         # of indication changes. The `true` below the `4` above #\n\
         # goes from purple outline (created by the cases(4) call) #\n\
         # to green highlighting (part of the same call as `4`). #\n\
         # The formerly selected lower `true` is now highlit in #\n\
         # purple since it indicates the call where `4` lives . #\n\
         ^^probe(cases(5)),\n\
         ^^probe(cases(6))\n\
         ) in\n\n\
         # FUNCTIONS CALLING FUNCTIONS #\n\
         let _ =\n\
        \  # Select `9` below. Note four cells below become purple #\n\
         let fourth = fun f -> 4 * ^^probe(f) - 4 in\n\
        \  # This is because they represent function calls #\n\
        \  # above the `9` cell in the function call stack. #\n\
        \  # For example 32 below represents the call producing `9`.  #\n\
         let third = fun t -> ^^probe(fourth(t - 3)) / 3 in\n\
        \  # Now, select `32` above. Note the 9 now has a purple hat. #\n\
        \  # This represents that it is below the `32` call in the stack. #\n\
        \  # Now select `10` below, which is a call to `third`: #\n\
         let second = fun s -> ^^probe(third(2 * s)) + 2 in\n\
        \  # Note that `9` and `32` both have hats. `9` has a taller hat #\n\
        \  # to show it's lower in the call stack. `32` has purple text #\n\
        \  # to further emphasize that is is directly below `10`. #\n\
        \  # Now select 12 below, representing a call to `second` #\n\
         let first = fun f -> ^^probe(second(f + 1)) * 2 in\n\
        \  # Note how the hats have changed. Finally, select `24` below, #\n\
        \  # and then again select 12, 10, 32, and 9 in turn. Notice how #\n\
        \  # the solid purple call indicators have internal shadows #\n\
        \  # representing their depth relative to the selected cell #\n\
         ^^probe(first(5)) in\n\n\
         # RECURSION #\n\
         # Note how cells are lowered/raised to indicate their #\n\
         # relative call stack depth to the selected cell #\n\
         let fact = fun ^^probe(x) ->\n\
         case ^^probe(x)\n\
         | 1 => ^^probe(1)\n\
         | _ =>\n\
         let r = ^^probe(fact(x-1)) \n\
         in ^^probe(x*r)  \n\
         end in\n\
         test ^^probe(fact(5)) == 120 end;\n\n\
         # TAIL RECURSION #\n\
         let fact1 = let go =\n\
         fun (^^probe(x),\n\
        \ ^^probe(acc)) ->\n\
         case ^^probe(x)\n\
         | 1 => ^^probe(acc)\n\
         | _ =>\n\
         let r = ^^probe(x*acc) \n\
         in ^^probe(go(x-1, r)) \n\
         end in\n\
         fun x -> ^^probe(go(x,1)) in\n\
         test ^^probe(fact1(5)) == 120 end;\n\n\
         # FUNCTIONS IN FUNCTIONS #\n\
         # The frunk factory prethunks your frunk for later clunking #\n\
         let frunk_factory = fun ^^probe(y) ->\n\
         # This is a play area to explore nested function definitions #\n\
         # and functions returning functions #\n\
         let factor =\n\
         4 + ^^probe(10 * y) in\n\
         let refactor = fun ^^probe(x) ->\n\
         ^^probe(x + factor) in\n\
         let factor =\n\
         ^^probe(refactor(factor))\n\
         - ^^probe(refactor(y)) in \n\
         let perturb = fun ^^probe(s) ->\n\
         factor + ^^probe(refactor(s)) in\n\
         fun ^^probe(z) ->\n\
         ^^probe(perturb(3*z))\n\
         + ^^probe(perturb(5*z))  \n\
         in let new_frunk = ^^probe(frunk_factory(7)) in\n\
         test ^^probe(new_frunk(4)) == 314 end;\n\
         test ^^probe(new_frunk(6)) == 330 end;\n\n\n\
         let _ = let fib: Int -> Int =\n\
         # Recursive calls can complicate probe display due #\n\
         # due to overlapping information channels.#\n\
         fun x -> case x\n\
         | 0 => 1\n\
         | 1 => 1\n\
         | n =>   \n\
         # Select the first `1` below: #\n\
         fib(x-1)\n\
         # Note the purple `2` below corresponding the call #\n\
         # fib(4-2) which contains the above `1`. The `1` below #\n\
         # OTOH is highlit because when the above call was made, #\n\
         # the call below had that value. The two `1s` outline in #\n\
         # purple above come /from/ the indicated call, whereas the #\n\
         # highlit `2`s are from the /same/ call the indicated call #\n\
         # was evaluated in. #\n\
         + fib(x-2) end\n\
         in\n\
         test fib(1) == 1 end;\n\
         test fib(2) == 2 end;\n\
         test fib(3) == 3 end;\n\
         test fib(4) == 5 end;\n\
         test fib(5) == 8 end;\n\
         test fib(6) == 13 end\n\
         in \n\n\
         # ADVANCED FEATURES #\n\
         # - You can resize a cell by holding shift and dragging horizontally #";
    } )
