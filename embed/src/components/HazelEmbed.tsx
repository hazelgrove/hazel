import React, { useEffect, useRef, useCallback } from "react";
import type { HazelToParent, ParentToHazel } from "../types/patchworkmessages";

/**
 * Retrieves a query parameter from the URL
 * @param name - The name of the query parameter to retrieve
 * @param defaultValue - The default value to return if the parameter is not found
 * @returns The value of the query parameter or the default value
 */
export function getQueryParam(name: string, defaultValue: string = ""): string {
  if (typeof window === "undefined") {
    return defaultValue;
  }
  const params = new URLSearchParams(window.location.search);
  return params.get(name) ?? defaultValue;
}

/**
 * Props for the HazelEmbed component
 */
interface HazelEmbedProps {
  /**
   * Unique identifier for this Hazel instance
   */
  instanceId: string;
  
  /**
   * Callback function that receives messages from the Hazel iframe
   * @param message - The message sent from Hazel (with state converted to HazelDoc format)
   * @param sourceInstanceId - The instance ID that sent the message
   */
  onMessage: (message: HazelToParent, sourceInstanceId: string) => void;
  
  /**
   * Function to register the sendMessage function for communicating with Hazel
   * @param sendMessageFn - Function that allows sending messages to Hazel (accepts HazelDoc format for state)
   */
  registerSendMessage: (
    sendMessageFn: (message: ParentToHazel) => void,
  ) => void;
  
  /**
   * URL of the Hazel instance to embed
   * Defaults to localhost:8000 in development and hazel.org/build/patchwork/ in production
   */
  url?: string;
}

/**
 * Helper function to send a message to the Hazel iframe
 * @param hazel - Reference to the iframe element
 * @param message - Message to send to Hazel (internal iframe format)
 */
const sendToHazel = (hazel: HTMLIFrameElement, message: ParentToHazel) => {
  if (hazel.contentWindow) {
    hazel.contentWindow.postMessage(message, "*");
  }
};

/**
 * React component that embeds a Hazel instance in an iframe and provides
 * two-way communication between the parent application and Hazel
 */
const HazelEmbed: React.FC<HazelEmbedProps> = ({
  instanceId,
  onMessage,
  registerSendMessage,
  url = import.meta.env.DEV ? "http://localhost:8000" : "https://hazel.org/build/patchwork/",
}) => {
  const hazelRef = useRef<HTMLIFrameElement>(null);

  // Listen for messages from the iframe
  useEffect(() => {
    const handleMessage = (event: MessageEvent) => {
      // Only accept messages from our iframe
      if (!hazelRef.current || event.source !== hazelRef.current.contentWindow) {
        return;
      }

      // Filter out React DevTools messages
      if (event.data?.source?.includes("react")) return;

      const hazelMessage = event.data as HazelToParent;
      onMessage(hazelMessage, instanceId);
    };

    window.addEventListener("message", handleMessage);
    return () => {
      window.removeEventListener("message", handleMessage);
    };
  }, [instanceId, onMessage]);

  // Function to send a message to the Hazel instance
  const sendMessage = useCallback(
    (message: ParentToHazel) => {
      if (hazelRef.current) {
        sendToHazel(hazelRef.current, message);
      }
    },
    [],
  );

  // Register the sendMessage function with the parent component
  useEffect(() => {
    registerSendMessage(sendMessage);
  }, [registerSendMessage, sendMessage]);

  return (
    <div className="hazel-embed-container">
      <iframe
        src={url}
        style={{ width: "100%", height: "100%", border: "none" }}
        ref={hazelRef}
      />
    </div>
  );
};

export default HazelEmbed;
