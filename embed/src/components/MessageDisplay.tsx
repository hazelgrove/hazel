import React, { useRef, useState, useEffect } from "react";
import type { HazelToParent, EditorDelta } from "../types/messages";
import { FaArrowDown } from "react-icons/fa";

interface MessageWithMetadata {
  message: HazelToParent;
  instanceId: string; // Store instance ID separately
  timestamp: string;
}

interface MessageDisplayProps {
  messages: MessageWithMetadata[];
}

const MessageDisplay: React.FC<MessageDisplayProps> = ({ messages }) => {
  const messageListRef = useRef<HTMLDivElement>(null);
  const [isAtBottom, setIsAtBottom] = useState(true);
  const [showScrollButton, setShowScrollButton] = useState(false);

  // Helper function to determine message type
  const getMessageType = (message: HazelToParent): string => {
    // Using the t field to determine message type
    return message.t.charAt(0).toUpperCase() + message.t.slice(1);
  };

  // Helper function to get message content
  const getMessageContent = (message: HazelToParent): string => {
    if (message.t === "delta") {
      return `Delta with ${(message as EditorDelta).delta.length} operations`;
    }
    return message.message;
  };

  // Check if user is at bottom of the message list
  const checkIfAtBottom = () => {
    if (messageListRef.current) {
      const { scrollTop, scrollHeight, clientHeight } = messageListRef.current;
      const atBottom = Math.abs(scrollHeight - scrollTop - clientHeight) < 10;
      setIsAtBottom(atBottom);
      setShowScrollButton(!atBottom);
    }
  };

  // Scroll to bottom function with animation
  const scrollToBottom = () => {
    if (messageListRef.current) {
      const messageList = messageListRef.current;
      const targetScroll = messageList.scrollHeight - messageList.clientHeight;

      // Animate the scroll
      const startPosition = messageList.scrollTop;
      const distance = targetScroll - startPosition;
      const duration = 300; // ms
      let startTime: number;

      // Easing function for smooth animation
      const easeOutCubic = (t: number): number => {
        return 1 - Math.pow(1 - t, 3);
      };

      const animateScroll = (currentTime: number) => {
        if (!startTime) startTime = currentTime;
        const elapsedTime = currentTime - startTime;

        const progress = Math.min(elapsedTime / duration, 1);
        const eased = easeOutCubic(progress);

        messageList.scrollTop = startPosition + distance * eased;

        if (elapsedTime < duration) {
          requestAnimationFrame(animateScroll);
        } else {
          // Animation complete
          messageList.scrollTop = targetScroll;
          setIsAtBottom(true);
          setShowScrollButton(false);
        }
      };

      requestAnimationFrame(animateScroll);
    }
  };

  // Handle scroll events
  useEffect(() => {
    const messageList = messageListRef.current;
    if (messageList) {
      messageList.addEventListener("scroll", checkIfAtBottom);
      return () => {
        messageList.removeEventListener("scroll", checkIfAtBottom);
      };
    }
  }, []);

  // Auto-scroll to bottom when new messages arrive if already at bottom
  useEffect(() => {
    if (isAtBottom && messages.length > 0) {
      scrollToBottom();
    }
  }, [messages, isAtBottom]);

  return (
    <div className="message-display">
      <h2>Messages</h2>
      <div className="message-list" ref={messageListRef}>
        {messages.length === 0 ? (
          <p className="no-messages">No messages yet.</p>
        ) : (
          messages.map((item, index) => {
            const messageType = getMessageType(item.message);
            const content = getMessageContent(item.message);

            return (
              <div key={index} className="message-item">
                <div className="message-header">
                  <span className="instance-id">
                    From Instance: {item.instanceId}
                  </span>
                  <span className="message-type">Type: {messageType}</span>
                  <span className="timestamp">{item.timestamp}</span>
                </div>
                <div className="message-content">
                  <div className="message-body">
                    <strong>Content:</strong> {content}
                  </div>
                  <details>
                    <summary>Raw Data</summary>
                    <pre>{JSON.stringify(item.message, null, 2)}</pre>
                  </details>
                </div>
              </div>
            );
          })
        )}
      </div>
      {showScrollButton && (
        <div className="scroll-button-container">
          <button
            className="scroll-to-bottom-button"
            onClick={scrollToBottom}
            aria-label="Scroll to bottom"
          >
            <FaArrowDown />
          </button>
        </div>
      )}
    </div>
  );
};

export default MessageDisplay;
