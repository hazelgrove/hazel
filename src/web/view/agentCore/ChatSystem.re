open Util_web;
open Haz3lcore;
open AgentResult;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type active_screen =
    | Chat
    | History;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type slash_menu_state = {
    filter: string,
    selected_index: int,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type ui = {
    active_screen,
    current_text_box_content: string,
    [@yojson.default None]
    slash_menu: option(slash_menu_state),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    chat_map: Id.Map.t(Chat.Model.t),
    current: Id.t,
    ui,
  };
};

module Utils = {
  let find_chat = (id: Id.t, model: Model.t): Chat.Model.t => {
    Id.Map.find_opt(id, model.chat_map)
    |> OptUtil.get_or_fail("[ChatSystem.Utils.find_chat] Chat not found");
  };

  let switch_chat = (id: Id.t, model: Model.t): Model.t => {
    {
      ...model,
      current: id,
    };
  };

  let update_chat = (chat: Chat.Model.t, model: Model.t): Model.t => {
    {
      ...model,
      chat_map: Id.Map.add(chat.id, chat, model.chat_map),
    };
  };

  let new_chat =
      (~system_prompt: string, ~dev_notes: string, model: Model.t): Model.t => {
    let new_chat = Chat.Utils.init(~system_prompt, ~dev_notes);
    let model = update_chat(new_chat, model);
    {
      ...model,
      current: new_chat.id,
    };
  };

  let delete_chat = (id: Id.t, model: Model.t): Model.t => {
    {
      ...model,
      chat_map: Id.Map.remove(id, model.chat_map),
      current: Id.Map.choose(model.chat_map) |> fst,
    };
  };

  let chats_to_list = (model: Model.t): list(Chat.Model.t) => {
    // Converts the map of chats to a list of chats
    // ordered by the created_at timestamp
    Id.Map.bindings(model.chat_map)
    |> List.map(((_, chat: Chat.Model.t)) => chat)
    |> List.sort((a: Chat.Model.t, b: Chat.Model.t) =>
         Float.compare(a.created_at, b.created_at)
       );
  };

  let init = (~system_prompt: string, ~dev_notes: string): Model.t => {
    let initial_chat = Chat.Utils.init(~system_prompt, ~dev_notes);
    {
      chat_map: Id.Map.singleton(initial_chat.id, initial_chat),
      current: initial_chat.id,
      ui: {
        active_screen: Chat,
        current_text_box_content: "",
        slash_menu: None,
      },
    };
  };

  let derive_slash_menu_from_content =
      (~prev: option(Model.slash_menu_state), content: string)
      : option(Model.slash_menu_state) =>
    if (String.length(content) < 1 || content.[0] != '/') {
      None;
    } else {
      let after_slash = String.sub(content, 1, String.length(content) - 1);
      if (String.contains(after_slash, ' ')) {
        None;
      } else {
        let prev_filter =
          Option.map((s: Model.slash_menu_state) => s.filter, prev);
        let selected_index =
          switch (prev_filter) {
          | Some(f) when f == after_slash =>
            Option.map((s: Model.slash_menu_state) => s.selected_index, prev)
            |> Option.value(~default=0)
          | _ => 0
          };
        Some({
          filter: after_slash,
          selected_index,
        });
      };
    };
};

module Update = {
  module Action = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | SwitchChat(Id.t)
      | NewChat(string, string)
      | DeleteChat(Id.t)
      | SwitchScreen(Model.active_screen)
      | SaveTextBoxContent(string)
      | SlashMenuAdjustSelection(int)
      | ChatAction(Chat.Update.Action.t, Id.t);
  };

  let get = (result: Result.t(Model.t)): Model.t => {
    switch (result) {
    | Ok(model) => model
    | Error(error) =>
      failwith(
        switch (error) {
        | Failure.Info(msg) => msg
        },
      )
    };
  };

  let update = (action: Action.t, model: Model.t): Result.t(Model.t) => {
    switch (action) {
    | SwitchChat(chat_id) =>
      let m = Utils.switch_chat(chat_id, model);
      Ok({
        ...m,
        ui: {
          ...m.ui,
          slash_menu: None,
        },
      });
    | NewChat(system_prompt, dev_notes) =>
      let m = Utils.new_chat(~system_prompt, ~dev_notes, model);
      Ok({
        ...m,
        ui: {
          ...m.ui,
          slash_menu: None,
        },
      });
    | DeleteChat(chat_id) => Ok(Utils.delete_chat(chat_id, model))
    | SwitchScreen(active_screen) =>
      Ok({
        ...model,
        ui: {
          ...model.ui,
          active_screen,
        },
      })
    | SaveTextBoxContent(content) =>
      Ok({
        ...model,
        ui: {
          ...model.ui,
          current_text_box_content: content,
          slash_menu:
            Utils.derive_slash_menu_from_content(
              ~prev=model.ui.slash_menu,
              content,
            ),
        },
      })
    | SlashMenuAdjustSelection(delta) =>
      switch (model.ui.slash_menu) {
      | None => Ok(model)
      | Some(sm) =>
        let cmds = ChatSlashCommands.filtered(sm.filter);
        let n = List.length(cmds);
        if (n == 0) {
          Ok(model);
        } else {
          let idx = (sm.selected_index + delta + n * 1000) mod n;
          Ok({
            ...model,
            ui: {
              ...model.ui,
              slash_menu:
                Some({
                  ...sm,
                  selected_index: idx,
                }),
            },
          });
        };
      }
    | ChatAction(chat_action, chat_id) =>
      switch (
        Chat.Update.update(chat_action, Utils.find_chat(chat_id, model))
      ) {
      | Ok(updated_chat) => Ok(Utils.update_chat(updated_chat, model))
      | Error(error) => Error(error)
      }
    };
  };
};
