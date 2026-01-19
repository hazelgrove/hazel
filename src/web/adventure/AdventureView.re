/* Adventure Mode: View Component
 *
 * Renders the floating adventure dialog with speech bubble aesthetic.
 * Uses FloatingElement pattern for position:fixed rendering that
 * escapes overflow clipping.
 */

open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* Render the hazelnut avatar */
let avatar = () =>
  div(
    ~attrs=[clss(["adventure-avatar"])],
    [text("\xF0\x9F\x8C\xB0")] /* Chestnut emoji as placeholder for hazelnut */
  );

/* Render a button */
let button = (~primary=false, ~disabled=false, label, on_click) =>
  Node.button(
    ~attrs=[
      clss(
        ["adventure-button"]
        @ (primary ? ["primary"] : [])
        @ (disabled ? ["disabled"] : []),
      ),
      Attr.on_click(_ => disabled ? Effect.Ignore : on_click),
    ],
    [text(label)],
  );

/* Get upcoming agent actions after the current step.
 * Looks ahead through auto-advancing steps (Checkpoint, AgentAction)
 * until hitting a Message or UserGate. */
let get_upcoming_actions = (model: AdventureModel.t): list(Adventure.step) => {
  let steps = model.script.steps;
  let current = model.current_step;
  let rec collect = (idx, acc) =>
    if (idx >= List.length(steps)) {
      List.rev(acc);
    } else {
      switch (List.nth(steps, idx)) {
      | Adventure.AgentAction(_) as step => collect(idx + 1, [step, ...acc])
      | Adventure.Checkpoint => collect(idx + 1, acc)
      | Adventure.LoadEditor(_) => collect(idx + 1, acc)
      | Adventure.Message(_)
      | Adventure.UserGate(_) => List.rev(acc)
      };
    };
  /* Start looking from the step AFTER current */
  collect(current + 1, []);
};

/* Render action preview */
let action_preview = (upcoming: list(Adventure.step)): Node.t =>
  if (List.length(upcoming) == 0) {
    Node.none;
  } else {
    let action_texts =
      List.filter_map(
        fun
        | Adventure.AgentAction({narration: Some(n), _}) => Some(n)
        | Adventure.AgentAction({actions, _}) =>
          Some(
            "Agent will perform "
            ++ string_of_int(List.length(actions))
            ++ " action(s)",
          )
        | _ => None,
        upcoming,
      );
    if (List.length(action_texts) == 0) {
      Node.none;
    } else {
      div(
        ~attrs=[clss(["adventure-action-preview"])],
        [
          div(
            ~attrs=[clss(["adventure-preview-label"])],
            [text("Next, agent will:")],
          ),
          ...List.map(
               action_text =>
                 div(
                   ~attrs=[clss(["adventure-preview-item"])],
                   [text("• " ++ action_text)],
                 ),
               action_texts,
             ),
        ],
      );
    };
  };

/* Render the message content based on current step */
let message_content =
    (
      ~inject: AdventureUpdate.t => Ui_effect.t(unit),
      model: AdventureModel.t,
    ) => {
  switch (AdventureModel.current_step(model)) {
  | None =>
    /* Adventure complete */
    div(
      ~attrs=[clss(["adventure-message"])],
      [
        div(
          ~attrs=[clss(["adventure-text"])],
          [text("Tutorial complete! Great job.")],
        ),
        div(
          ~attrs=[clss(["adventure-actions"])],
          [button("Close", inject(Stop))],
        ),
      ],
    )

  | Some(Message({text: msg_text, can_advance})) =>
    let upcoming = get_upcoming_actions(model);
    div(
      ~attrs=[clss(["adventure-message"])],
      [
        div(~attrs=[clss(["adventure-text"])], [text(msg_text)]),
        action_preview(upcoming),
        div(
          ~attrs=[clss(["adventure-actions"])],
          can_advance
            ? [button(~primary=true, "Next", inject(Advance))] : [],
        ),
      ],
    );

  | Some(AgentAction({narration, _})) =>
    let display_text = Option.value(narration, ~default="Working...");
    div(
      ~attrs=[clss(["adventure-message", "agent-acting"])],
      [
        div(~attrs=[clss(["adventure-text"])], [text(display_text)]),
        div(
          ~attrs=[clss(["adventure-actions"])],
          [button(~primary=true, "Next", inject(Advance))],
        ),
      ],
    );

  | Some(UserGate({hint, _})) =>
    let hint_text = hint == "" ? "Complete the task to continue." : hint;
    div(
      ~attrs=[clss(["adventure-message", "user-gate"])],
      [
        div(~attrs=[clss(["adventure-text"])], [text(hint_text)]),
        div(~attrs=[clss(["adventure-hint"])], [text("(Your turn!)")]),
      ],
    );

  | Some(Checkpoint | LoadEditor(_)) =>
    /* These auto-advance, shouldn't be visible */
    div(~attrs=[clss(["adventure-message"])], [text("...")])
  };
};

/* Render reset suggestion overlay */
let reset_suggestion = (~inject: AdventureUpdate.t => Ui_effect.t(unit)) =>
  div(
    ~attrs=[clss(["adventure-reset-suggestion"])],
    [
      div(
        ~attrs=[clss(["adventure-reset-text"])],
        [text("Stuck? You can reset to try again.")],
      ),
      div(
        ~attrs=[clss(["adventure-actions"])],
        [
          button("Reset", inject(Reset)),
          button("Keep trying", inject(DismissResetSuggestion)),
        ],
      ),
    ],
  );

/* Render reset button when available */
let reset_button =
    (
      ~inject: AdventureUpdate.t => Ui_effect.t(unit),
      model: AdventureModel.t,
    ) =>
  if (AdventureModel.can_reset(model)) {
    div(
      ~attrs=[clss(["adventure-reset-container"])],
      [button("Reset", inject(Reset))],
    );
  } else {
    Node.none;
  };

/* Main view function */
let view =
    (
      ~inject: AdventureUpdate.t => Ui_effect.t(unit),
      model: AdventureModel.t,
    )
    : Node.t =>
  if (!model.active) {
    Node.none;
  } else {
    let content =
      if (model.show_reset_suggestion) {
        reset_suggestion(~inject);
      } else {
        message_content(~inject, model);
      };

    /* The dialog uses FloatingElement pattern:
     * - position: fixed escapes overflow clipping
     * - Positioned in bottom-right corner of viewport */
    div(
      ~attrs=[
        Attr.id("adventure-dialog"),
        clss(["adventure-dialog"]),
        /* Fixed positioning in viewport */
        Attr.style(
          Css_gen.concat([
            Css_gen.position(`Fixed),
            Css_gen.top(`Px(40)),
            Css_gen.right(`Px(40)),
          ]),
        ),
      ],
      [
        /* Close button */
        div(
          ~attrs=[
            clss(["adventure-close"]),
            Attr.on_click(_ => inject(Stop)),
            Attr.title("Close tutorial"),
          ],
          [text("\xC3\x97")] /* Unicode multiplication sign (looks like X) */
        ),
        /* Title bar */
        div(
          ~attrs=[clss(["adventure-title"])],
          [text(model.script.title)],
        ),
        /* Avatar and speech bubble */
        div(
          ~attrs=[clss(["adventure-content"])],
          [avatar(), div(~attrs=[clss(["adventure-bubble"])], [content])],
        ),
        /* Reset button when at a gate */
        AdventureModel.is_at_gate(model)
          ? reset_button(~inject, model) : Node.none,
      ],
    );
  };
