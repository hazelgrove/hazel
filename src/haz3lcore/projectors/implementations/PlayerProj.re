open Util;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Language;

/* Global play state for mutual exclusion - only one Player can be active at a time */
module PlayState = {
  let current: ref(option(Id.t)) = ref(Option.None);

  let play = (id: Id.t, pattern: Strudel.pattern) => {
    Strudel.stopMusic();
    Strudel.playPattern(pattern);
    current := Option.Some(id);
  };

  let stop = () => {
    Strudel.stopMusic();
    current := Option.None;
  };

  let is_playing = (id: Id.t) => current^ == Option.Some(id);
};

/* Get Sound value from dynamics samples */
let get_sound_value = (dynamics: option(Dynamics.Info.t)): option(Exp.t) =>
  switch (dynamics) {
  | Some(di) =>
    switch (di.samples) {
    | [sample, ..._] => Some(sample.value)
    | [] => None
    }
  | None => None
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Play
    | Stop;

  /* Only init on expressions (type check happens at runtime via dynamics) */
  let init = (any: Any.t): option(model) =>
    switch (any) {
    | Exp(_) => Some()
    | _ => None
    };

  let dynamics = true; /* Need dynamics to get evaluated value */
  let focusable = Focusable.non;
  let placeholder = (_, _) => ProjectorCore.Shape.default;

  let update = (model, _info, _action: action) => model; /* State is global, not in model */

  let view = ({info, local, _}: View.args(model, action)) => {
    let sound_value = get_sound_value(info.dynamics);
    let is_playing = PlayState.is_playing(info.id);

    /* Check if we have a valid sound value */
    let has_sound =
      switch (sound_value) {
      | Some(exp) => SoundUtil.is_sound(exp)
      | None => false
      };

    View.{
      inline: div([]),
      overlay: None,
      offside:
        Some(
          div(
            ~attrs=[
              Attr.id(Id.cls(info.id)),
              Attr.classes(["player-offside"]),
            ],
            [
              /* Play/Pause button */
              button(
                ~attrs=[
                  Attr.classes(
                    ["player-btn"]
                    @ (is_playing ? ["playing"] : [])
                    @ (has_sound ? [] : ["disabled"]),
                  ),
                  Attr.on_click(_ =>
                    if (is_playing) {
                      PlayState.stop();
                      local(Stop);
                    } else {
                      switch (sound_value) {
                      | Some(exp) =>
                        switch (SoundUtil.interpret_sound(exp)) {
                        | Some(pattern) =>
                          PlayState.play(info.id, pattern);
                          local(Play);
                        | None => Effect.Ignore
                        }
                      | None => Effect.Ignore
                      };
                    }
                  ),
                ],
                [text(is_playing ? {js|⏸|js} : {js|▶|js})],
              ),
              /* Sound description */
              switch (sound_value) {
              | Some(exp) when SoundUtil.is_sound(exp) =>
                span(
                  ~attrs=[Attr.classes(["player-desc"])],
                  [text(SoundUtil.sound_description(exp))],
                )
              | Some(_) =>
                span(
                  ~attrs=[Attr.classes(["player-desc", "not-sound"])],
                  [text("not Sound")],
                )
              | None =>
                span(
                  ~attrs=[Attr.classes(["player-desc", "no-sound"])],
                  [{js|∅|js} |> text],
                )
              },
            ],
          ),
        ),
    };
  };
};
