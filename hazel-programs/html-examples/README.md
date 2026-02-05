# HazelHtml Example Programs

These example programs demonstrate the HazelHtml web app library features.

## How to Use

1. Copy the code from any `.hz` file
2. Paste it into a Hazel scratchpad
3. Right-click on the expression that produces HTML
4. Select "Add HTML" from the context menu
5. The HTML projector will render your app

## Examples

### counter.hz
A simple click counter demonstrating:
- Basic HTML structure (Div, Button, Text)
- Event handlers (OnClick)
- Self-modifying pattern (click updates the view)

### todo-list.hz
A todo list application demonstrating:
- Form input handling (OnInput)
- List manipulation (adding/removing items)
- More complex state management

### timer.hz
A timer using subscriptions demonstrating:
- The App type: `((HTML, Cmd), HTML -> Sub)`
- Every subscription for periodic updates
- Start/pause/reset functionality

### keyboard-game.hz
A keyboard-controlled game demonstrating:
- OnDocumentKeyDown subscription for global key events
- KeyEvent handling (arrow keys)
- Position-based styling

### animation.hz
A bouncing ball animation demonstrating:
- AnimationFrame subscription for smooth animation
- Physics simulation (gravity, bounce)
- Continuous visual updates

### full-app.hz
A comprehensive example demonstrating:
- Multiple features together (tabs, forms)
- Commands (Focus, Log)
- Event handlers returning (HTML, Cmd) tuples
- Subscriptions (OnResize)

## Types Reference

See `docs/hazel-html-implementation.md` for complete type documentation.

### Quick Reference

```
HTML = Div([attrs], [children]) | Button([attrs], [children]) | Text(str) | ...
Attr = Class(str) | Style([(key, value)]) | OnClick(HTML -> HTML) | ...
Cmd = CmdNone | Focus(id) | Log(msg) | Delay(ms, HTML -> HTML) | ...
Sub = SubNone | Every(ms, (HTML, Float) -> HTML) | OnDocumentKeyDown(...) | ...
App = ((HTML, Cmd), HTML -> Sub)
```
