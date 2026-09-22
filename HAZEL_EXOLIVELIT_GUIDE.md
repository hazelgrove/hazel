# Hazel Exo Livelit: External App Integration Guide

Use this guide to make your app embeddable in Hazel as an ExoLivelit. Your app runs in an iframe and communicates with Hazel using `postMessage`. This is a minimal proof of concept implementation; we will be implementing more message types to expose more Hazel functionality, as well as support for incremental syntax updates when it becomes necessary.

This guide has been claude hardened; a claude was able to oneshot adapt an unrelated ts app to an exolivelit and make the relevant hazelside changes by being pointed at this file in an alongside clone of the hazel repo, given additionally a description of how the relevant part of that app's data model should be represented in the JSON format hazel can consume. You can check out the value builder exolivelit to familiarize yourself with the data schema supported.

## Requirements

- **Iframe-compatible**: Your app must work inside an iframe
- **PostMessage communication**: Follow Hazel's protocol for bidirectional messaging
- **URL parameters**: Read `id` and `parentOrigin` from query string
- **Auto-resize**: Measure and report your content dimensions
- **Value serialization**: Accept initial state and send updated state as stringified JSON

## URL Contract

Hazel loads your app with: `https://your-app/?id=<uuid>&parentOrigin=<hazel-origin>`

- **`id`**: Unique identifier for this projector instance
- **`parentOrigin`**: Hazel's origin for `postMessage` target (security)

## Message Protocol

### App → Hazel (ToHazelMessage)

To start, it suffices to send `ready` and `setSyntax`, and to handle `init`. More of the internal livelits API will be exposed here in the future.

```typescript
type ToHazelMessage =
  | { type: "ready"; id: string } // Sent on mount
  | { type: "setSyntax"; id: string; codec: string; value: string } // Send edits
  | { type: "resize"; id: string; width: number; height: number }; // Report size
```

### Hazel → App (FromHazelMessage)

```typescript
type FromHazelMessage =
  | { type: "init"; id: string; value: string } // Initial value
  | {
      type: "constraints";
      id: string;
      maxWidth: number;
      maxHeight: number;
      minWidth?: number;
      minHeight?: number;
    }; // Size limits
```

## Communication Lifecycle

1. **App mounts** → sends `{type: 'ready', id}`
2. **Hazel responds** → `{type: 'init', value: "..."}` + `{type: 'constraints', ...}`
3. **App applies constraints** → sets `maxWidth` CSS, starts auto-resize
4. **User edits** → app sends `{type: 'setSyntax', codec: 'json', value: JSON.stringify(newValue)}`
5. **Content grows** → app sends `{type: 'resize', width, height}` (debounced)

## Integration Starter Library

Copy these **3 files** into your e.g. `src/hooks/` directory:

### 1. Core Hook (`hazel-integration-base.ts`)

### 2. Resize Strategies (`resize-strategies.ts`)

### 3. App-Specific Hook (`useHazelIntegration.ts`)

```
src/
├── components/
│   └── MyEditor.tsx            # Core app logic (Hazel-agnostic)
├── hooks/
│   ├── hazel-integration-base.ts   # Shared base hook
│   ├── resize-strategies.ts        # Resize implementations
│   └── useHazelIntegration.ts      # App-specific wrapper
└── App.tsx                     # Entry point, uses Hazel integration
```

## App Integration Example

```typescript
// App.tsx
import { useState } from "react";
import { useHazelIntegration } from "./useHazelIntegration";

export default function App() {
  const urlParams = new URLSearchParams(window.location.search);
  const id = urlParams.get("id") || "local-demo";
  const [value, setValue] = useState<any>(0);

  const { setSyntax } = useHazelIntegration({
    id,
    codec: "json" /* The only codec supported for now  */,
    onInit: (valueStr) => {
      setValue(JSON.parse(valueStr));
    },
    onConstraints: (c) => {
      document.body.style.maxWidth = `${c.maxWidth}px`;
    },
  });

  const handleChange = (newValue: any) => {
    setSyntax(JSON.stringify(newValue));
  };

  return (
    <div style={{}}>
      <h3>My Hazel-Embedded App</h3>
      {/* Your editor UI here */}
      <button onClick={() => handleChange(value + 1)}>
        Increment: {value}
      </button>
    </div>
  );
}
```

## CSS Guidelines

**DO:**

- Use flexible, responsive layouts (`flex`, `grid`)
- Allow content-driven height (`min-height` instead of fixed `height`)
- Apply `maxWidth` from constraints for responsive behavior

**DON'T:**

- Set fixed `height: 100vh` on root containers
- Use viewport units that ignore iframe constraints
- Create horizontal scrolling (respect `maxWidth`)

## Hazel Integration (Hazel-side)

We don't currently support dynamic registration for new kinds of exolivelit, although there is no particular blocker to doing so. Right now to add a new one, you'll need to clone the hazel repo and change two definitions in the `Exo.re` file.

### 1. Create a static identifier for your exolivelit

Add YourApp to the `Exo.kind` type. This will determine the Hazel UI name of your exolivelit.

```ocaml
  type kind =
  | ...
  | YourApp;
```

### 2. Specify static and default properties for your exolivelit

Add a corresponding case to the `Exo.module_of_kind` function. This requires a `prod` and option `dev` URL for your app. The `shape` property determines how the text flow resumes to the right of your livelit; pick `Block` if in doubt. The rest of the properties (`guard` and `size`) are work-in-progress and will likely be set dynamically via content negoiation in the future; copying the values below should suffice for a prototype.

```ocaml
  | YourApp => {
      kind,
      prod: "https://yourdomain.com", (* Your public URL for the app *)
      dev: "http://localhost:port", (* Your internal dev path, if applicable *)
      shape: Block, (* Block: After livelit, text flow continues from bottom line. Tab: Continues from top *)
      guard: _ => true, (* Determines what Hazel syntax your app can be applied to; okay to leave as this for now *)
      size: {
        width: 680, (* init width in px; *)
        height: 490, (* init height in px *)
      },
    }
```

## Example Apps

- **`external-apps/simple-slider/`**: Integer slider
- **`external-apps/value-builder/`**: Compositional Hazel value editor
- **`https://github.com/disconcision/nool/pull/4`**: A PR updating a math toy to support livelit embedding
