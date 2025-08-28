import { useEffect, useCallback, useRef } from 'react';
import type { ToHazelMessage } from '../types/hazel-protocol';
import { isFromHazelMessage } from '../types/hazel-protocol';

interface HazelIntegrationConfig {
  id: string;
  codec: string;
  onInit?: (value: any) => void;
  onUpdate?: (value: any) => void;
}

// Simple trailing throttle utility
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

export function useHazelIntegration(config: HazelIntegrationConfig) {
  const { id, codec, onInit, onUpdate } = config;
  const hasInitialized = useRef(false);

  // Get target origin from URL params or env, fallback to '*'
  const targetOrigin = 
    new URLSearchParams(window.location.search).get('parentOrigin') ||
    import.meta.env.VITE_HAZEL_ORIGIN ||
    '*';

  // Send message to parent (Hazel)
  const sendToHazel = useCallback((message: ToHazelMessage) => {
    if (window.parent && window.parent !== window) {
      window.parent.postMessage(message, targetOrigin);
    }
  }, [targetOrigin]);

  // Send setSyntax message with current value (throttled to reduce message rate)
  const setSyntax = useCallback(
    throttle((value: any) => {
      sendToHazel({ type: 'setSyntax', id, codec, value });
    }, 50),
    [sendToHazel, id, codec]
  );

  // Send resize message
  const resize = useCallback((width: number, height: number) => {
    sendToHazel({ type: 'resize', id, width, height });
  }, [sendToHazel, id]);

  // Send ready message
  const sendReady = useCallback(() => {
    sendToHazel({ type: 'ready', id });
  }, [sendToHazel, id]);

  // Request focus from parent
  const requestFocus = useCallback(() => {
    sendToHazel({ type: 'requestFocus', id });
  }, [sendToHazel, id]);

  // Listen for messages from parent (Hazel)
  useEffect(() => {
    const handleMessage = (event: MessageEvent) => {
      const data = event.data;
      
      if (!isFromHazelMessage(data) || data.id !== id) {
        return;
      }

      switch (data.type) {
        case 'init':
          if (onInit) {
            onInit(data.value);
          }
          break;
        case 'update':
          if (onUpdate) {
            onUpdate(data.value);
          }
          break;
      }
    };

    window.addEventListener('message', handleMessage);
    
    // Send ready message when component mounts
    if (!hasInitialized.current) {
      sendReady();
      hasInitialized.current = true;
    }

    return () => {
      window.removeEventListener('message', handleMessage);
    };
  }, [id, onInit, onUpdate, sendReady]);

  // Optional: Report size changes to parent
  useEffect(() => {
    const element = document.documentElement;
    const resizeObserver = new ResizeObserver(entries => {
      const rect = entries[0].contentRect;
      sendToHazel({ 
        type: 'resize', 
        id, 
        width: Math.round(rect.width), 
        height: Math.round(rect.height) 
      });
    });
    
    resizeObserver.observe(element);
    
    return () => {
      resizeObserver.disconnect();
    };
  }, [id, sendToHazel]);

  return {
    setSyntax,
    resize,
    requestFocus,
  };
}
