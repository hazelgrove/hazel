# @hazelgrove/hazel-embed

React component for embedding Hazel in other applications.

## Installation

### From Git

You can install the package directly from GitHub:

For a specific folder within the repository:

```bash
pnpm add https://gitpkg.vercel.app/hazelgrove/hazel/embed?patchwork
```

## Usage

```tsx
import React, { useCallback } from 'react';
import { HazelEmbed, type HazelToParent, type ParentToHazel } from '@hazelgrove/hazel-embed';

function App() {
  // Reference to store the sendMessage function
  let sendMessageToHazel: ((message: ParentToHazel) => void) | null = null;

  // Handler for messages from Hazel
  const handleMessage = useCallback((message: HazelToParent, sourceId: string) => {
    console.log(`Message from Hazel instance ${sourceId}:`, message);
    
    // Example: respond to a ping with a pong
    if (message.t === 'ping' && sendMessageToHazel) {
      sendMessageToHazel({
        t: 'pong',
        message: 'Hello from parent!'
      });
    }
  }, []);

  // Register the sendMessage function
  const registerSendMessage = useCallback((sendFn: (message: ParentToHazel) => void) => {
    sendMessageToHazel = sendFn;
  }, []);

  return (
    <div style={{ width: '800px', height: '600px' }}>
      <HazelEmbed
        instanceId="hazel-1"
        onMessage={handleMessage}
        registerSendMessage={registerSendMessage}
        // Optional: custom URL
        // url="https://your-hazel-instance.com"
      />
    </div>
  );
}

export default App;
```

## API

### Components

#### `HazelEmbed`

Main component that embeds a Hazel instance in an iframe.

Props:

| Prop | Type | Required | Description |
|------|------|----------|-------------|
| `instanceId` | `string` | Yes | Unique identifier for this Hazel instance |
| `onMessage` | `(message: HazelToParent, sourceInstanceId: string) => void` | Yes | Callback function that receives messages from the Hazel iframe |
| `registerSendMessage` | `(sendMessageFn: (message: ParentToHazel) => void) => void` | Yes | Function to register the sendMessage function for communicating with Hazel |
| `url` | `string` | No (defaults to https://hazel.org/build/patchwork/) | URL of the Hazel instance to embed. Defaults to localhost:8000 in development and hazel.org/build/patchwork/ in production |

### Types

The package exports the following TypeScript types:

- `HazelToParent` - Messages sent from Hazel to the parent application
- `ParentToHazel` - Messages sent from the parent application to Hazel
- Message types: `Init`, `Ping`, `Pong`, `EditorDelta`
- Delta types: `EditScript`, `EditOp`, `DeleteOp`, `InsertOp`, etc.
