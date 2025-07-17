import React, { useState, useCallback, useRef } from "react";
import HazelEmbed from "./components/HazelEmbed";
import MessageDisplay from "./components/MessageDisplay";
import DeltaTree from "./components/DeltaTree";
import TilesViewer from "./components/TilesViewer";
import type { TilesViewerRef } from "./components/TilesViewer";
import type {
  HazelToParent,
  ParentToHazel,
  Pong,
  EditorDelta,
  Ping,
} from "./types/messages";

interface MessageWithMetadata {
  message: HazelToParent;
  instanceId: string;
  timestamp: string;
}

function App() {
  const [messages, setMessages] = useState<MessageWithMetadata[]>([]);
  const tilesViewerRef = useRef<TilesViewerRef>(null);

  // References to Hazel instances for sending messages back
  const hazelRefs = {
    "hazel-1": React.useRef<{
      sendMessage: (message: ParentToHazel) => void;
    }>(null),
    // "hazel-2": React.useRef<{ sendMessage: (message: ParentToHazel) => void }>(null),
  };

  const registerSendMessage = (
    sendMessageFn: (message: ParentToHazel) => void,
  ) => {
    hazelRefs["hazel-1"].current = { sendMessage: sendMessageFn };
  };

  const handleMessage = (message: HazelToParent, sourceInstanceId: string) => {
    const newMessage: MessageWithMetadata = {
      message,
      instanceId: sourceInstanceId,
      timestamp: new Date().toLocaleTimeString(),
    };
    setMessages((prevMessages) => [...prevMessages, newMessage]);

    switch (message.t) {
        case "delta": {
            console.log(`Received delta from instance ${sourceInstanceId}:`, message);
            
            // Process each operation in the delta through the TilesViewer component
            message.delta.forEach((op) => {
                tilesViewerRef.current?.processOp(op);
            });
            
            break;
        }
        case "ping": {
            const pongMessage: Pong = {
                t: "pong",
                message: `Pong response to ${sourceInstanceId}!`,
            };

            // Send the pong message back to the same instance that sent the ping
            const hazelRef = hazelRefs[sourceInstanceId as keyof typeof hazelRefs];
            if (hazelRef?.current) {
              hazelRef.current.sendMessage(pongMessage);
            } else {
                console.error(`Cannot send pong: instance ${sourceInstanceId} not found`);
            }
            break;
        }
        case "pong": {
          break;
        }
        case "init": {
            console.log(`Received init from instance ${sourceInstanceId}:`, message);
            break;
        }
        default: {
          const exhaustiveCheck: never = message;
          console.warn(`Unknown message type: ${(message as any).t}`);
          return;
        }
    }

  };

  // Function to send a ping message to a Hazel instance
  const sendPing = useCallback((instanceId: string) => {
    const pingMessage: Ping = {
      t: "ping",
      message: `Ping from parent to instance ${instanceId}!`,
    };

    const hazelRef = hazelRefs[instanceId as keyof typeof hazelRefs];
    if (hazelRef?.current) {
      hazelRef.current.sendMessage(pingMessage);
    } else {
      console.error(`Cannot send ping: instance ${instanceId} not found`);
    }
  }, []);

  return (
    <div className="app-container">
      <div className="hazel-column">
        <div className="hazel-instance-header">
          <button className="ping-button" onClick={() => sendPing("hazel-1")}>
            Ping hazel-1
          </button>
        </div>
        <div className="hazel-instance">
          <HazelEmbed
            instanceId="hazel-1"
            onMessage={handleMessage}
            registerSendMessage={registerSendMessage}
          />
        </div>
      </div>
      <div className="message-column">
        <div className="message-section">
          <MessageDisplay messages={messages} />
        </div>
        <div className="delta-section">
            <TilesViewer ref={tilesViewerRef} />
        </div>
      </div>
    </div>
  );
}

export default App;
