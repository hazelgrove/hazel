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
      // Apply size constraints
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

## Build & Deploy

### Development

```bash
# Your app on localhost
npm run dev -- --port 5175

# Hazel automatically points to localhost in dev mode
```

### Production

```bash
# Build with relative paths
npm run build -- --base=./

# Hazel CI copies dist/ to /external/<your-app>/
# Apps served at https://hazel.org/build/<branch>/external/<your-app>/
```

## Hazel Integration (Hazel-side)

To add your app to Hazel, create an adapter in `ExoAdapters.re`:

```ocaml
module MyAppAdapter: Exo.Info = {
  let exo_kind = ProjectorCore.Kind.ExoMyApp;
  let codec_name = "json";
  let target_origin =
    WebEnv.choose_origin(
      ~name="myapp",
      ~dev="http://localhost:5175",
      ~prod=WebEnv.base_url() ++ "/external/myapp"
    );

  let url = (id: Id.t) =>
    Printf.sprintf("%s/?id=%s&parentOrigin=%s",
      target_origin, Id.to_string(id), WebEnv.window_origin());

  let term_to_string = (term) => (* convert Hazel term to JSON string *);
  let string_to_term = (str, _) => (* convert JSON string to Hazel term *);
  let init_test = (any) => Some({exo_kind, width: 400, height: 200});
};
```

## Wrapper Decoupling

Keep Hazel integration separate from your core app:

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

**Benefits of the shared approach:**

- **90% code reuse** between apps
- **Pluggable resize strategies** for different content types
- **Consistent protocol handling** across all external apps
- **Easy to maintain** - bug fixes benefit all apps

```typescript
// App.tsx - Hazel integration at the top level
export default function App() {
  const [value, setValue] = useState();
  const { setSyntax } = useHazelIntegration({
    codec: "json",
    onInit: (v) => setValue(JSON.parse(v)),
  });

  return (
    <MyEditor
      value={value}
      onChange={(v) => {
        setValue(v);
        setSyntax(JSON.stringify(v));
      }}
    />
  );
}
```

This keeps your core component reusable outside Hazel while maximizing shared infrastructure.

## Troubleshooting

**App not loading**: Check browser console for CORS/sandbox errors  
**No resize**: Verify `ResizeObserver` is firing, check CSS constraints  
**Messages not sent**: Confirm `parentOrigin` is set correctly  
**Parse errors**: Check JSON stringify/parse, match Hazel's codec expectations  
**Focus issues**: Remove focus-related code, Hazel handles iframe focus

## Example Apps

- **`external-apps/simple-slider/`**: Integer slider with `int` codec
- **`external-apps/value-builder/`**: Complex JSON editor with `json` codec

Both demonstrate the complete integration pattern and can serve as templates for your own apps.
