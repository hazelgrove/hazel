import React, { useState, useCallback } from "react";
import HazelEmbed from "./components/HazelEmbed";
import MessageDisplay from "./components/MessageDisplay";
import DeltaTree from "./components/DeltaTree";
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
    console.log(`Received message from instance ${sourceInstanceId}:`, message);

    // If we receive a ping, automatically respond with a pong
    if (message.t === "ping") {
      console.log(
        `Received ping from instance ${sourceInstanceId}, responding with pong`,
      );

      // Create a pong response
      const pongMessage: Pong = {
        t: "pong",
        message: `Pong response to ${sourceInstanceId}!`,
      };

      // Send the pong message back to the same instance that sent the ping
      const hazelRef = hazelRefs[sourceInstanceId as keyof typeof hazelRefs];
      if (hazelRef?.current) {
        hazelRef.current.sendMessage(pongMessage);
      }
    }
  };

  // Get the latest delta message if there is one
  const latestDelta =
    messages.length > 0
      ? (messages.filter((item) => item.message.t === "delta").pop() as
          | {
              message: EditorDelta;
              instanceId: string;
              timestamp: string;
            }
          | undefined)
      : undefined;

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
        {latestDelta && (
          <div className="delta-section">
            <div className="delta-tree-container">
              <h3>Syntax Tree</h3>
              <DeltaTree delta={latestDelta.message.delta} />
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

export default App;
