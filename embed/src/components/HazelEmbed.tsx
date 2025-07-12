import React, { useEffect, useRef, useCallback } from "react";
import type {
  Init,
  Ping,
  HazelToParent,
  ParentToHazel,
} from "../types/messages";

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
   * @param message - The message sent from Hazel
   * @param sourceInstanceId - The instance ID that sent the message
   */
  onMessage: (message: HazelToParent, sourceInstanceId: string) => void;
  
  /**
   * Function to register the sendMessage function for communicating with Hazel
   * @param sendMessageFn - Function that allows sending messages to Hazel
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
 * @param message - Message to send to Hazel
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

  useEffect(() => {
    if (hazelRef.current) {
      hazelRef.current.onload = () => {
        const initMessage: Init = {
          t: "init",
          message: `Hello, you are instance ${instanceId}!`,
        };

        sendToHazel(hazelRef.current!, initMessage);
      };
    }
  }, [instanceId]);

  // Listen for messages from the iframe
  useEffect(() => {
    const handleMessage = (event: MessageEvent) => {
      if (event.data?.source?.includes("react")) return;

      // Extract the message from the event data
      const hazelMessage = event.data as HazelToParent;

      if (hazelMessage && hazelMessage.t) {
        // Forward the message to the parent component along with the instance ID
        onMessage(hazelMessage, instanceId);
      } else {
        console.error("Invalid message format from Hazel:", event.data);
      }
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
      } else {
        console.error("Hazel iframe is not available.");
      }
    },
    [instanceId],
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
