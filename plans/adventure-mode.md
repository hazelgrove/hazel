# Adventure Mode: Interactive Tutorial System

Adventure Mode is an interactive guided tutorial system for Hazel, designed to introduce users (particularly user study participants) to Hazel's features, with an initial focus on probes and debugging functionality.

## Overview

The system presents an **agent/avatar** (a hazelnut character) in a floating speech bubble that guides users through interactive lessons. The agent can:
- Display instructional messages
- Perform actions in the editor (typing, moving cursor, adding probes)
- Wait for users to complete tasks before advancing
- Reset to checkpoints when users get stuck

## Architecture

### Module Structure

```
src/web/adventure/
├── Adventure.re           # Core types and script definitions
├── AdventureModel.re      # State management
├── AdventureUpdate.re     # Actions (Start, Stop, Advance, Reset, etc.)
├── AdventureView.re       # Floating UI component
├── AdventureGate.re       # Predicate evaluation for user gates
└── AdventureScripts.re    # Tutorial scripts (probes_intro, etc.)
```

### Core Types

```reason
// Adventure script step types
type step =
  | Message(message_config)              // Show message, wait for "Next"
  | AgentAction(agent_action_config)     // Agent performs editor actions
  | UserGate(gate_config)                // Wait for user to complete task
  | Checkpoint                           // Mark restorable state
  | LoadEditor(Zipper.t)                 // Load specific editor state

type message_config = {
  text: string,
  can_advance: bool,  // Show "Next" button
}

type agent_action_config = {
  actions: list(Action.t),
  narration: option(string),  // Optional text shown during action
  animate: bool,              // Character-by-character (future)
}

type gate_config = {
  predicate: gate_predicate,
  hint: string,               // Shown to help user
  action_limit: option(int),  // Actions before suggesting reset
}

type gate_predicate =
  | HasAnyProbe                 // User has added any probe
  | HasProbeOnIndicated         // Probe on currently indicated term
  | TextContains(string)        // Editor text contains substring
  | TextEquals(string)          // Editor text exactly equals
  | TermSatisfies(string)       // Placeholder for AST checks
  | And(list(gate_predicate))
  | Or(list(gate_predicate))

// Runtime state (AdventureModel.t)
type t = {
  active: bool,
  script: Adventure.script,
  current_step: int,
  checkpoint: option(Zipper.t),   // For reset functionality
  actions_since_gate: int,        // Count for reset suggestion
  show_reset_suggestion: bool,
  confirming_exit: bool,          // Showing exit confirmation dialog
}
```

## MVP Features

### Phase 1: Core Infrastructure
- [x] Adventure types and script representation
- [x] FloatingElement-based dialog UI with speech bubble aesthetic
- [x] Message display and "Next" button advancement
- [x] Integration into Page model/update
- [x] Keyboard shortcut to trigger (Ctrl/Cmd + Shift + A)

### Phase 2: Agent Actions
- [x] Instant action execution (paste code, toggle probe)
- [x] Caret visibility before action (user sees where change happens)
- [x] Checkpoint creation and "Reset" button

### Phase 3: User Gating
- [x] Term structure predicate checking
- [x] Probe presence checking
- [x] Gate UI with hint text
- [x] Action counting with reset suggestion after threshold

### Phase 4: First Tutorial
- [x] Simple probes tutorial script
- [x] Test with basic flow

## Future Features (Post-MVP)

### Animated Actions
- Character-by-character typing via scheduled actions
- Interrupt confirmation modal if user acts during animation
- Visual indication that agent is "typing"

### Enhanced Navigation
- Back button to previous steps
- Progress indicator
- Skip to section

### UI Polish
- Hazelnut avatar illustration
- Draggable dialog position
- UI element highlighting/pointing
- Smooth transitions

### Extended Gating
- Action-based gates (monitor specific keypresses)
- Evaluation result checking
- Time-based hints ("Still working on it?")

### Content
- Multiple tutorial scripts
- Hazel syntax basics
- Advanced probe features (pinning, step-into)
- Debugging workflows

## Integration Points

### Page.re
- Add `adventure: AdventureModel.t` to `Model.t`
- Add `Adventure(AdventureUpdate.t)` to `Update.t`
- Forward user actions to adventure for gate checking
- Render adventure overlay in view

### Keyboard Shortcuts
- Ctrl/Cmd + Shift + A: Toggle adventure mode
- Space: Advance to next step (when editor is locked during tutor's turn)

### Action Forwarding
When adventure mode has an active UserGate, user actions are:
1. Applied normally to the editor
2. Also passed to AdventureGate for predicate evaluation
3. If predicate satisfied, adventure advances automatically

## Example Tutorial Script

```reason
let probes_intro: list(step) = [
  // Welcome
  Message({
    text: "Welcome to Hazel! I'll teach you about probes - a powerful way to see your code's values as it runs.",
    can_advance: true,
  }),

  // Load blank editor
  LoadEditor(Zipper.init()),
  Checkpoint,

  // Agent types expression
  Message({
    text: "First, let me type a simple expression...",
    can_advance: true,
  }),
  AgentAction({
    actions: [Paste(String("1 + 2"))],
    narration: Some("Typing: 1 + 2"),
    animate: false,
  }),

  // Explain probes
  Message({
    text: "Now I'll add a probe to see its value. Watch the cursor!",
    can_advance: true,
  }),
  AgentAction({
    actions: [Probe(ToggleManual)],
    narration: Some("Adding probe..."),
    animate: false,
  }),

  // Point out value
  Message({
    text: "See the '3' that appeared? That's the probe showing you the expression's value!",
    can_advance: true,
  }),

  // Remove probe
  AgentAction({
    actions: [Probe(ToggleManual)],
    narration: Some("Removing probe..."),
    animate: false,
  }),

  // User's turn
  Checkpoint,
  Message({
    text: "Your turn! Add ' * 4' after the expression, then add a probe to see the new value.",
    can_advance: false,
  }),
  UserGate({
    predicate: TextContains("* 4"),
    hint: "Move to the end and type ' * 4' (so it becomes '1 + 2 * 4')",
    action_threshold: 25,
  }),

  Message({
    text: "Great! You modified the expression.",
    can_advance: true,
  }),

  UserGate({
    predicate: HasAnyProbe,
    hint: "Now add a probe using Ctrl+E (or Cmd+E on Mac)",
    action_threshold: 25,
  }),

  // Success!
  Message({
    text: "The value is now 12. You've learned the basics of probes!",
    can_advance: true,
  }),
];
```

## Design Decisions

### Why FloatingElement?
The adventure dialog needs to float above all editor content, including areas that might be clipped by overflow:auto containers. FloatingElement solves this with position:fixed + viewport coordinate calculation.

### Why Term-Based Gating?
Checking term structure (not surface syntax) means we don't care about whitespace, comments, or formatting variations. The user can write `1+2*4` or `1 + 2 * 4` and both satisfy the same predicate.

### Why Action Counting?
Rather than trying to detect "wildly off-track" states, we count actions since checkpoint. After N actions without gate satisfaction, we gently suggest resetting. This is simpler and more predictable.

### Why Instant Actions for MVP?
Animated typing requires:
1. Scheduled action mechanism (timers)
2. Interrupt handling (what if user types during animation?)
3. Visual feedback (cursor blinking, "agent is typing" indicator)

These are valuable but not essential for initial testing. Instant actions with caret visibility still shows WHERE changes happen.

---

## Issues Discovered (Post-MVP Testing)

### Issue 1: Gate Checking Never Triggered (BUG)
**Problem**: `UserActed` and `check_gate` are defined in `AdventureUpdate.re` but never called. When user adds a probe, the gate isn't checked.

**Root Cause**: No hook connects editor actions to adventure gate checking.

**Fix**: In `Page.Update.calculate()`, after computing statics, check if adventure is at a gate and if the predicate is satisfied. This happens after `calculate` because we need `info_map` from statics.

```reason
// In Page.Update.calculate, after editor calculation:
let model =
  if (model.adventure.active && AdventureModel.is_at_gate(model.adventure)) {
    let zipper = get_current_zipper(model.editors);
    let info_map = get_current_statics(model.editors);
    let result = AdventureUpdate.check_gate(~zipper, ~info_map, model.adventure);
    // If gate passed, result.model has advanced step
    // Also apply any editor_actions from the result
    {...model, adventure: result.model}
  } else {
    model
  };
```

### Issue 2: Starting on Non-Blank Editor
**Problem**: When adventure starts on an editor with content, agent actions insert at cursor position instead of on a clean slate.

**Fix Options**:
1. **Add ClearEditor step** - New step type that clears editor content
2. **Start script with clear actions** - Begin with `Select(All), Destruct(Left)`
3. **Switch to blank scratch slide** - More complex but cleaner

**Recommended**: Option 2 for simplicity - add clear actions at script start.

### Issue 3: User Can Edit During Agent Sequences
**Problem**: User can modify editor while agent is showing messages or performing actions, breaking expected state.

**Fix**: Add "editor locked" concept to adventure:
- New field: `editor_locked: bool` in `AdventureModel.t`
- Lock editor during `Message` steps (before user's turn) and `AgentAction` steps
- In `Page.Selection.handle_key_event`, check if adventure has locked editor
- If locked, return `None` (ignore edit) or show a toast message

```reason
// In Page.Selection.handle_key_event:
| _ when model.adventure.active && AdventureModel.is_editor_locked(model.adventure) =>
  // Editor is locked during agent actions - ignore input
  None
```

**Alternative**: Instead of blocking, show a modal asking "The tutorial is in progress. Exit tutorial?" with options to exit or wait.

### Issue 4: Wrong Keyboard Shortcut in Hint
**Problem**: Script says "Ctrl+P" but probe shortcut is actually "Ctrl+E" / "Cmd+E".

**Fix**: Update hint text in `AdventureScripts.re`.

### Issue 5: No Back/Forward Navigation
**Problem**: User can't review previous messages or undo accidental advancement.

**Fix**: Add navigation with editor state restoration:

```reason
// Add to AdventureModel.t:
type history_entry = {
  step_index: int,
  zipper: Zipper.t,
};
step_history: list(history_entry),  // Stack of previous states

// Add to AdventureUpdate.t:
| Back   // Go to previous step, restore editor
| Forward  // Go forward (if we've been there)
```

At each `Checkpoint` and before `UserGate`, capture editor state. `Back` pops history and restores editor. `Forward` re-advances if we've already been to that step.

---

## Finalized Design Decisions

### Editor Locking
- **Decision**: Lock editor entirely during non-gate steps (Message, AgentAction, etc.)
- User can only edit during `UserGate` steps
- Provide escape hatch: "Exit Tutorial" button always available
- Rationale: Simpler than trying to validate/recover from user edits during agent turns

### Fresh Scratch Slide (Not Just Clear)
- **Decision**: Create a fresh scratch slide at tutorial start instead of clearing current editor
- Avoids messy state from existing content
- Tutorial runs in isolated environment
- Implementation: Create new slide via Editors module rather than Select(All)+Destruct

### Back/Forward Navigation with Atomic Groupings
- **Decision**: Tutorial writer controls step-back granularity via "atomic groups"
- Steps within a group are treated as one unit for back navigation
- Example: Message + AgentAction + Message could be one atomic group
- Back goes to start of previous group, forward replays the group
- Need to store editor state at group boundaries

### Keyboard Shortcut
- **Confirmed**: Probe shortcut is Ctrl/Cmd+E (not Ctrl+P as incorrectly stated in hint)

---

## Action Items

### Immediate (Priority 1) - Do Now
- [x] **Fix gate checking bug** - Add `check_gate` call in `Page.Update.calculate()` after statics computation
- [x] **Fix keyboard shortcut hint** - Change "Ctrl+P" to "Ctrl+E" (or "Cmd+E" on Mac) in AdventureScripts.re
- [x] **Create fresh scratch slide** - Modify tutorial start to create new slide instead of clearing

### Soon (Priority 2) - After Initial Fixes
- [x] **Block mode/slide navigation during adventure** - Navigation actions (SwitchMode, SwitchSlide, SwitchExercise) are blocked at the data level in Page.Update when adventure is active
- [x] **Disable mode/slide dropdowns during adventure** - Solved via overlay: during locked steps (non-UserGate), an overlay covers entire UI preventing all mouse interaction. During UserGate steps, navigation is still blocked at data level (dropdowns clickable but do nothing).
- [x] **Lock editor during non-gate steps** - `AdventureModel.is_editor_locked` returns true when active but not at UserGate. Blocks keyboard input, undo/redo, paste, and mouse (via overlay). Cmd/Ctrl+Shift+A still works as escape hatch. Clicking overlay exits tutorial.
- [x] **Space to advance** - Space key advances when editor is locked (tutor's turn). Hint shown below Next button.
- [ ] **Wire up action counting** - `UserActed` action exists but is never dispatched. Need to call it after user edits during UserGate so `actions_since_gate` increments and reset suggestion can trigger. Currently reset suggestion never appears.
- [ ] **Implement atomic groupings** - Add `AtomicGroup(list(step))` step type or grouping metadata
- [ ] **Back/forward navigation** - Store editor state at group boundaries, implement navigation

### Later (Priority 3) - Polish
- [x] **Turn indicator UI** - Two avatars (Tutor 🌰 and User 👤) with green ring on active one. Shows whose turn it is visually.
- [x] **Contextual exit message** - Clicking overlay during Tutor's turn shows "It's the Tutor's turn. Click Next to continue." instead of immediately asking to exit.
- [ ] **Floating toast feedback** - Brief tooltip-like message near cursor when clicking overlay (e.g., "Tutor's turn"), fades after 2-3 seconds. Requires cursor position tracking.
- [ ] **Avatar pulse animation** - Pulse the active avatar's ring when user clicks during wrong turn
- [ ] **Forward button** - Re-advance after going back
- [ ] **Progress indicator** - Show current position in tutorial
- [ ] **Animated typing** - Character-by-character agent actions

---

## Implementation Notes

### Page.re Helper Functions

The adventure integration in Page.re uses two helper functions to keep the code clean:

**`create_adventure_slide`**: Creates a new blank scratch slide for the adventure
- Generates unique name ("Adventure", "Adventure (1)", etc.)
- Appends to existing scratchpads
- Switches to Scratch mode with new slide as current

**`apply_adventure_result`**: Handles side effects from `AdventureUpdate.update_result`
- Schedules `editor_actions` via `ActiveEditor`
- Captures checkpoint when `set_checkpoint` is true
- Schedules reset actions (Select All, Destruct, Paste) when `reset_to_checkpoint` is true

Both the `Adventure(action)` case in `update` and the gate checking in `calculate` use `apply_adventure_result` to avoid duplication.

### Gate Checking
Gate checking happens in `Page.Update.calculate()` after editors calculation (so statics are available):
```reason
let adventure =
  if (model.adventure.active && AdventureModel.is_at_gate(model.adventure)) {
    let editor = get_editor({...model, editors});
    let zipper = editor.editor.state.zipper;
    let info_map = editor.statics.info_map;
    let result = AdventureUpdate.check_gate(~zipper, ~info_map, model.adventure);
    apply_adventure_result(~schedule_action, ~zipper, result);
  } else {
    model.adventure;
  };
```

### Navigation Blocking
In `Page.Update.update`, navigation actions are blocked when adventure is active:
```reason
| Editors(action) =>
  let is_navigation_action = switch (action) {
    | SwitchMode(_) | Scratch(SwitchSlide(_))
    | Tutorial(SwitchExercise(_)) | Exercises(SwitchExercise(_)) => true
    | _ => false
  };
  if (model.adventure.active && is_navigation_action) {
    model |> return_quiet;  // Block navigation
  } else { ... }
```

### Editor Locking
When adventure is active but NOT at a `UserGate` step, the editor is locked to prevent users from modifying state during agent demonstrations:

**`AdventureModel.is_editor_locked`**: Returns `true` when `active && !is_at_gate(model)`

**Keyboard blocking** in `Page.Selection.handle_key_event`:
- Undo/Redo are blocked when locked (return `None`)
- All other keys fall through to editors, but are blocked when locked
- Adventure toggle (Cmd/Ctrl+Shift+A) always works as escape hatch

**Paste blocking** in `Page.View.handlers`:
- Paste events are blocked when locked (check before processing clipboard)

**Mouse blocking** via overlay in `Page.View.main_view`:
- When locked, renders `.adventure-overlay` div covering entire viewport
- z-index 999 (below adventure dialog at 1000)
- Slight tint (5% black) for subtle visual feedback
- `cursor: not-allowed` indicates non-interactivity
- Clicking the overlay triggers `Adventure(RequestExit)` which shows contextual message
- Overlay disappears during `UserGate` steps when user should be editing

**Turn indicator** in `AdventureView`:
- Two rows stacked vertically: Tutor (🌰) and User (👤)
- Each row has a caret icon (capsule-shaped SVG matching editor caret) + avatar emoji
- Caret icon uses `.caret-path` class for consistent styling with actual editor caret
- Active row is full opacity, inactive is dimmed (40% opacity)
- Tutor caret is green (--G0), User caret is red (--R1)
- `is_user_turn` determined by `AdventureModel.is_at_gate(model)`
- Editor caret turns green during tutor's turn (via `.tutor-turn` class on `#page`)
- Caret keeps blinking during tutor's turn to maintain visual continuity

**Exit confirmation** is contextual:
- During Tutor's turn: "It's the Tutor's turn. Click Next to continue." with [Got it] [Exit Tutorial]
- During User's turn: "Exit the tutorial?" with [Continue] [Exit Tutorial]

**Space to advance** in `Page.Selection.handle_key_event`:
- When editor is locked and adventure can advance, space triggers `Adventure(Advance)`
- Keyboard hint "(Space)" shown below the Next button in `AdventureView`

---

## Known Issues / Technical Debt

### UserActed Never Dispatched
The `UserActed` action in `AdventureUpdate.t` is designed to increment `actions_since_gate` so that after N actions without satisfying the gate, a reset suggestion appears. However, this action is never dispatched from Page.re. The `check_gate` function is called in `calculate()` but only checks if the predicate is satisfied - it doesn't increment the action counter.

**Impact**: Users never see the "Stuck? You can reset" prompt even after many failed attempts.

**Fix**: After editor actions during a UserGate step, dispatch `Adventure(UserActed)`. This could be done in `calculate()` alongside `check_gate`, or in the update handler for editor actions.
