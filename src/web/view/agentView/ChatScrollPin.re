/* ChatScrollPin — a one-shot request to pin the chat to its bottom on the
   next frame (set when the user sends a prompt; consumed by the messages
   list's scroll hook). Lives apart so the bottom bar and the messages view
   need not depend on each other. */
let request: ref(bool) = ref(false);
