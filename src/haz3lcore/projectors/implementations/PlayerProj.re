open Util;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Language;

/* Use the shared PlayState from Strudel module */
module PlayState = Util.Strudel.PlayState;

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
    let samples_ready = Util.Strudel.samplesReady();

    /* Check if we have a valid sound value */
    let has_sound =
      switch (sound_value) {
      | Some(exp) => SoundUtil.is_sound(exp)
      | None => false
      };

    /* Live coding: if we're playing and the sound value exists,
     * auto-update the pattern. This enables seamless editing while playing.
     * The play_or_update function checks if the pattern actually changed. */
    let () =
      if (is_playing && has_sound) {
        switch (sound_value) {
        | Some(exp) =>
          let desc = SoundUtil.sound_description(exp);
          switch (SoundUtil.interpret_sound(exp)) {
          | Some(pattern) => PlayState.play_or_update(info.id, pattern, desc)
          | None => ()
          };
        | None => ()
        };
      };

    /* Can only play if samples are loaded and we have a valid sound */
    let can_play = samples_ready && has_sound;

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
                    @ (can_play ? [] : ["disabled"]),
                  ),
                  Attr.title(
                    !samples_ready
                      ? "Loading samples..."
                      : !has_sound ? "No sound to play" : "Play/Pause",
                  ),
                  Attr.on_click(_ =>
                    if (!can_play) {
                      Effect.Ignore;
                    } else if (is_playing) {
                      PlayState.stop();
                      local(Stop);
                    } else {
                      switch (sound_value) {
                      | Some(exp) =>
                        let desc = SoundUtil.sound_description(exp);
                        switch (SoundUtil.interpret_sound(exp)) {
                        | Some(pattern) =>
                          PlayState.play_or_update(info.id, pattern, desc);
                          local(Play);
                        | None => Effect.Ignore
                        };
                      | None => Effect.Ignore
                      };
                    }
                  ),
                ],
                [
                  text(
                    !samples_ready
                      ? {js|⏳|js} : is_playing ? {js|⏸|js} : {js|▶|js},
                  ),
                ],
              ),
              /* Speaker icon - pulses when playing */
              span(
                ~attrs=[
                  Attr.classes(
                    ["player-speaker"]
                    @ (is_playing ? ["playing"] : [])
                    @ (can_play ? [] : ["no-sound"]),
                  ),
                ],
                [text({js|🔊|js})],
              ),
            ],
          ),
        ),
    };
  };
};
