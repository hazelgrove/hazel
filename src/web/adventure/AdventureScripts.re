/* Adventure Mode: Tutorial Scripts
 *
 * Predefined tutorial scripts for teaching Hazel features.
 * Each script is a list of Adventure.step values that guide
 * the user through an interactive lesson.
 */

open Haz3lcore;

/* Simple probes introduction tutorial */
let probes_intro: Adventure.script = {
  id: "probes-intro",
  title: "Introduction to Probes",
  steps: [
    /* Welcome message */
    Adventure.message(
      "Welcome! I'll teach you about probes - a way to see values in your code as it runs.",
    ),
    /* Clear editor and set checkpoint */
    Adventure.Checkpoint,
    /* Explain what we'll do */
    Adventure.message(
      "First, I'll type a simple expression. Watch the editor!",
    ),
    /* Agent types expression */
    Adventure.agent_action(
      ~narration="Typing: 1 + 2",
      [Action.Paste(String("1 + 2"))],
    ),
    /* Explain probes */
    Adventure.message(
      "Now I'll add a probe to see the value. A probe shows you what an expression evaluates to.",
    ),
    /* Agent adds probe */
    Adventure.agent_action(
      ~narration="Adding probe...",
      [Action.Probe(ToggleManual)],
    ),
    /* Point out the value */
    Adventure.message(
      "See the value that appeared? The probe shows that 1 + 2 equals 3!",
    ),
    /* Remove the probe */
    Adventure.message(
      "I'll remove the probe now. You can toggle probes on and off.",
    ),
    Adventure.agent_action(
      ~narration="Removing probe...",
      [Action.Probe(ToggleManual)],
    ),
    /* User's turn - set checkpoint first */
    Adventure.Checkpoint,
    /* UserGate directly - no blocking Message needed.
     * The hint text tells the user what to do. */
    Adventure.user_gate(
      ~hint=
        "Your turn! Move to the end and type ' * 4' (so it becomes '1 + 2 * 4')",
      ~action_threshold=25,
      Adventure.TextContains("* 4"),
    ),
    /* Confirmation after first gate */
    Adventure.message("Great! You modified the expression."),
    /* Second gate for probe */
    Adventure.user_gate(
      ~hint="Now add a probe using Ctrl+E (or Cmd+E on Mac)",
      ~action_threshold=25,
      Adventure.HasAnyProbe,
    ),
    /* Celebrate success */
    Adventure.message(
      "Excellent! You've learned the basics of probes. Use them to understand how your code evaluates!",
    ),
    /* Final message */
    Adventure.message(
      "That's the end of this tutorial. Press Cmd/Ctrl+Shift+A to close, or click the X button.",
    ),
  ],
};
