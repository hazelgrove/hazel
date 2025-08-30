# Hazel Exo Livelit: External App Integration Guide

Use this guide to make your app embeddable in Hazel as an ExoLivelit. Your app runs in an iframe and communicates with Hazel using `postMessage`. This is a minimal proof of concept implementation; we will be implementing more message types to expose more Hazel functionality, as well as support for incremental syntax updates when it becomes necessary.

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

## Shared Integration Library (Recommended)

Copy these **3 files** into your `src/hooks/` directory for maximum convenience:

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
    codec: "json",
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
    <div style={{ padding: 8 }}>
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

- Use flexible layouts (`flex`, `grid`)
- Allow content-driven height (`min-height` instead of fixed `height`)
- Apply `maxWidth` from constraints for responsive behavior

**DON'T:**

- Set fixed `height: 100vh` on root containers
- Use viewport units that ignore iframe constraints
- Create horizontal scrolling (respect `maxWidth`)

## Hazel Integration (Hazel-side)

We're still working on the Hazel-side integration story; it should become more minimal but for now there's a few steps. Morally what you're doing is providing the URL of the external app, a friendly name for use in the Hazel UI, and a Hazel type to restrict the kinds of data your exolivelit can apply to. However, things are a little bit more manual at the moment.

Depending on who's reading this, either clone the Hazel repo and make the below modifications, or just ask Andrew to do it.

### 1. Create a static identifier for your exolivelit in ProjectorCore.re

This involves adding a case to the `exo_kind` type, and adding cases to related functions. It will suffice to just imitate all occurrences of `ExoValueBuilder` in that file.

### 2. Create an adapter in `ExoAdapters.re`

Add a new entry like the below one AND ALSO hook that in to the `module_of_kind` function. Similar to above, imitate the ValueBuilder case, except actually specifying a manual `prod` URL instead of using the auto-generated hazel repo one. The same init function can be used, or the completely permissive one below, but if you want to restrict what kind of hazel values you want to be able to consume/produce, you'll need a manual predicate. For now it's fine to just be permissive and fail if you recieve data you don't know how to deal with.

```ocaml
module MyAppAdapter: Exo.Info = {
  let kind = ProjectorCore.Kind.(* Your new kind from step 1 *);
  let prod = (* Your public URL for the app *);
  let dev = (* Your internal dev path/port for your app, if applicable *);
  let init_test = (any) => Some({
    width: (* init width in px; *),
    height: (* init height in px *)
    });
};
```

## Example Apps

- **`external-apps/simple-slider/`**: Integer slider
- **`external-apps/value-builder/`**: Compositional Hazel value editor
