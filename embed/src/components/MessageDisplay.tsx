import React from "react";
import type { HazelToParent, Ping, Pong } from "../types/messages";
import type { HazelDoc } from "../types/delta";

import "./DocComponents.css";

interface MessageDisplayProps {
  messages: Array<{
    message: HazelToParent;
    instanceId: string;
    timestamp: string;
  }>;
}

const MessageDisplay: React.FC<MessageDisplayProps> = ({ messages }) => {
  // Helper function to format message content for display
  const formatMessageContent = (message: HazelToParent): string => {
    switch (message.t) {
      case "init":
        return `Init: ${message.message}`;
      case "ping":
        return `Ping: ${message.message}`;
      case "pong":
        return `Pong: ${message.message}`;
      case "state":
        return `State update: ${message.state.title || "Untitled"} (${message.state.tiles.length} pieces)`;
      default:
        return `Unknown message type: ${(message as any).t}`;
    }
  };

  return (
    <div className="message-display">
      {messages.length === 0 ? (
        <p>No messages yet</p>
      ) : (
        <div className="message-list">
          {messages.map((msg, index) => (
            <div key={index} className="message-item">
              <div className="message-meta">
                <span className="message-time">{msg.timestamp}</span>
                <span className="message-source">{msg.instanceId}</span>
              </div>
              <div className="message-content">
                {formatMessageContent(msg.message)}
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
};

export default MessageDisplay;
