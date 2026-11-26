import { useEffect, useCallback, useRef } from "react";

/**
 * Shared Hazel integration types and utilities
 */

// Message protocol types
export type ToHazelMessage =
  | { type: "ready"; id: string }
  | { type: "setSyntax"; id: string; codec: string; value: string }
  | { type: "resize"; id: string; width: number; height: number };

export type FromHazelMessage =
  | { type: "init"; id: string; value: string }
  | {
      type: "constraints";
      id: string;
      maxWidth: number;
      maxHeight: number;
      minWidth?: number;
      minHeight?: number;
    };

export function isFromHazelMessage(data: unknown): data is FromHazelMessage {
  return (
    data !== null &&
    typeof data === "object" &&
    "type" in data &&
    "id" in data &&
    ["init", "constraints"].includes(
      (data as Record<string, unknown>).type as string
    ) &&
    typeof (data as Record<string, unknown>).id === "string"
  );
}

// Utility: Simple trailing throttle
function throttle<T extends (...args: any[]) => void>(fn: T, ms: number): T {
  let timeoutId: number | null = null;
  let lastArgs: any[] | null = null;

  const run = () => {
    timeoutId = null;
    if (lastArgs) {
      fn(...lastArgs);
      lastArgs = null;
    }
  };

  return ((...args: any[]) => {
    lastArgs = args;
    if (timeoutId === null) {
      timeoutId = window.setTimeout(run, ms);
    }
  }) as T;
}

// Resize strategy interface
export interface ResizeStrategy {
  setup(params: {
    id: string;
    sendToHazel: (message: ToHazelMessage) => void;
  }): () => void; // Returns cleanup function
}

// Configuration for the base hook
export interface HazelIntegrationConfig {
  id: string;
  codec: string;
  onInit?: (value: string) => void;
  onConstraints?: (constraints: {
    maxWidth: number;
    maxHeight: number;
    minWidth?: number;
    minHeight?: number;
  }) => void;
  resizeStrategy?: ResizeStrategy;
}

/**
 * Core Hazel integration hook - handles protocol, messaging, and setup
 */
export function useHazelIntegrationBase(config: HazelIntegrationConfig) {
  const { id, codec, onInit, onConstraints, resizeStrategy } = config;
  const hasInit = useRef(false);

  // Get target origin from URL params or fallback
  const targetOrigin =
    new URLSearchParams(window.location.search).get("parentOrigin") || "*";

  // Core message sender
  const sendToHazel = useCallback(
    (message: ToHazelMessage) => {
      if (window.parent && window.parent !== window) {
        window.parent.postMessage(message, targetOrigin);
      }
    },
    [targetOrigin]
  );

  // Throttled setSyntax function
  const setSyntax = useCallback(
    throttle((value: string) => {
      sendToHazel({ type: "setSyntax", id, codec, value });
    }, 50),
    [sendToHazel, id, codec]
  );

  // Manual resize function (for apps that want explicit control)
  const resize = useCallback(
    (width: number, height: number) => {
      sendToHazel({ type: "resize", id, width, height });
    },
    [sendToHazel, id]
  );

  // Message listener and ready handshake
  useEffect(() => {
    const handleMessage = (event: MessageEvent) => {
      const data = event.data;

      if (!isFromHazelMessage(data) || data.id !== id) {
        return;
      }

      switch (data.type) {
        case "init":
          if (onInit) {
            onInit(data.value);
          }
          break;
        case "constraints":
          if (onConstraints) {
            onConstraints({
              maxWidth: data.maxWidth,
              maxHeight: data.maxHeight,
              minWidth: data.minWidth,
              minHeight: data.minHeight,
            });
          }
          break;
      }
    };

    window.addEventListener("message", handleMessage);

    // Send ready message when component mounts
    if (!hasInit.current) {
      sendToHazel({ type: "ready", id });
      hasInit.current = true;
    }

    return () => {
      window.removeEventListener("message", handleMessage);
    };
  }, [id, onInit, onConstraints, sendToHazel]);

  // Setup resize strategy if provided
  useEffect(() => {
    if (!resizeStrategy) return;

    const cleanup = resizeStrategy.setup({ id, sendToHazel });
    return cleanup;
  }, [id, sendToHazel, resizeStrategy]);

  return {
    setSyntax,
    resize, // Manual resize for apps that need explicit control
  };
}
