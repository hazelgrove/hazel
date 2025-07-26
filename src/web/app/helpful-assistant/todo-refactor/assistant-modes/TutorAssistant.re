open Util;
open Virtual_dom.Vdom;

module M: AssistantBase.AssistantMode = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    init_chat: AssistantModel.chat,
    curr_chat: Id.t,
    past_chats: Id.Map.t(AssistantModel.chat),
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = () => {
    init_chat: AssistantModel.init_chat(HazelTutor),
    curr_chat: AssistantModel.init_chat(HazelTutor).id,
    past_chats: Id.Map.empty,
  }; // todo: implement
  let update = (_, model) => model; // todo: implement
  let view = (_, _) => Node.text("Tutor Assistant");
};
