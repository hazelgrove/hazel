# Simple Slider - External Hazel GUI

A standalone React TypeScript slider component designed to integrate with the Hazel editor as an external projector.

## Features

- **Integer Slider**: Interactive range input for integer values
- **Hazel Integration**: Implements postMessage protocol for seamless communication with Hazel
- **Multiple Variants**: Different slider configurations (basic, small range, large range)
- **Responsive Design**: Clean, modern UI that works well in iframes
- **Type Safety**: Full TypeScript support

## Quick Start

```bash
# Install dependencies
npm install

# Start development server
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview
```

## Development

The app will run at `http://localhost:5173` by default. You can view it standalone in your browser or embed it in an iframe for testing the Hazel integration.

### Hazel Integration Protocol

The slider implements a postMessage-based protocol for communication with Hazel:

#### Messages sent to Hazel (parent):

- `ready` - Component is loaded and ready
- `setSyntax` - User changed the slider value (includes codec and new value)
- `resize` - Component wants to change its size (future feature)
- `requestFocus` - Component wants focus (future feature)

#### Messages received from Hazel (parent):

- `init` - Initial value when projector is created
- `update` - Value changed externally (e.g., user edited the underlying syntax)

#### Codec

The slider uses the `int` codec, which converts between:

- **Hazel side**: `Atom(Int(Bigint.t))` (integer literal in the AST)
- **JSON**: String representation of the integer (e.g., `"42"`)

## Integration with Hazel

When integrated with Hazel, this component will:

1. **Replace integer literals** in the editor with an interactive slider
2. **Receive initial value** from the underlying Hazel syntax
3. **Send updates** back to Hazel when the user moves the slider
4. **Stay in sync** when the underlying syntax changes externally

## Project Structure

```
src/
├── components/
│   ├── IntegerSlider.tsx    # Main slider component
│   └── IntegerSlider.css    # Slider styles
├── hooks/
│   └── useHazelIntegration.ts  # Hazel communication hook
├── types/
│   └── hazel-protocol.ts    # TypeScript types for the protocol
├── App.tsx                  # Main app with demo
├── App.css                  # App-level styles
└── main.tsx                 # Entry point
```

## Building for Production

```bash
npm run build
```

The built files will be in the `dist/` directory. For Hazel integration, these files can be:

- Served from a static web server
- Copied into Hazel's `src/web/www/external/simple-slider/` directory
- Served via Hazel's Vite development server

## Next Steps

1. **Test standalone** - Verify the slider works in your browser
2. **Hazel iframe wrapper** - Create a projector that embeds this in an iframe
3. **Message bridge** - Add global postMessage handler in Hazel
4. **Codec implementation** - Add int literal ↔ JSON conversion in Hazel
5. **Integration testing** - Test the full round-trip communication
