import React, { useState, useCallback, useRef } from "react";
import HazelEmbed from "./components/HazelEmbed";
import MessageDisplay from "./components/MessageDisplay";
import DocGraph from "./components/DocGraph";
import DocStateManager from "./components/DocStateManager";
import type {
  HazelToParent,
  ParentToHazel,
  Pong,
  Ping,
  EditorState,
} from "./types/messages";
import "./components/DocComponents.css";

import { exportHazelDoc, generateHazelDoc, type HazelDoc } from "./types/interface";

interface MessageWithMetadata {
  message: HazelToParent;
  instanceId: string;
  timestamp: string;
}

function App() {
  const [messages, setMessages] = useState<MessageWithMetadata[]>([]);
  const [hazelState, setHazelState] = useState<HazelDoc | null>(null);

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
        case "state": {
          console.log(`Received state from instance ${sourceInstanceId}:`, message);
          const doc = generateHazelDoc(message.state);
          // Process the state and update hazelState
          setHazelState(doc);
          break;
        }
        default: {
          const _exhaustiveCheck: never = message;
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

  // Function to load a saved state and send it back to Hazel
  const handleLoadState = useCallback((state: HazelDoc) => {
    const instanceId = "hazel-1"; // We're using a single instance
    const editorStateMessage: EditorState = {
      t: "state",
      state: exportHazelDoc(state),
    };
    
    const hazelRef = hazelRefs[instanceId as keyof typeof hazelRefs];
    if (hazelRef?.current) {
      hazelRef.current.sendMessage(editorStateMessage);
      console.log("Sent state back to Hazel:", state);
    } else {
      console.error(`Cannot send state: instance ${instanceId} not found`);
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
      <div className="right-column">
        <div className="graph-section">
          <h3>Document Structure Graph</h3>
          <DocGraph docState={hazelState} />
        </div>
        <div className="state-manager-section">
          <DocStateManager 
            currentState={hazelState} 
            onLoadState={handleLoadState} 
          />
        </div>
        <div className="message-section">
          <h3>Message Log</h3>
          <MessageDisplay messages={messages} />
        </div>
      </div>
    </div>
  );
}

export default App;
