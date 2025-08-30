import { useEffect, useCallback, useRef } from 'react';
import type { ToHazelMessage } from '../types/hazel-protocol';
import { isFromHazelMessage } from '../types/hazel-protocol';

interface HazelIntegrationConfig {
  id: string;
  codec: string;
  onInit?: (value: any) => void;
  onUpdate?: (value: any) => void;
  onConstraints?: (constraints: { maxWidth: number; maxHeight: number; minWidth?: number; minHeight?: number }) => void;
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
  const { id, codec, onInit, onUpdate, onConstraints } = config;
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
    sendToHazel({ type: 'ready', id, });
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
        case 'constraints':
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

    window.addEventListener('message', handleMessage);
    
    // Send ready message when component mounts
    if (!hasInitialized.current) {
      sendReady();
      hasInitialized.current = true;
    }

    return () => {
      window.removeEventListener('message', handleMessage);
    };
  }, [id, onInit, onUpdate, onConstraints, sendReady]);

  // Enhanced ResizeObserver with debouncing and better content measurement
  useEffect(() => {
    const appRoot = document.getElementById('root') || document.body;
    let lastWidth = 0;
    let lastHeight = 0;

    // Throttled resize reporter (trailing edge)
    const reportResize = throttle((width: number, height: number) => {
      // Only send if there's a meaningful change (avoid spam)
      if (Math.abs(width - lastWidth) >= 2 || Math.abs(height - lastHeight) >= 2) {
        lastWidth = width;
        lastHeight = height;
        sendToHazel({ 
          type: 'resize', 
          id, 
          width: Math.round(width), 
          height: Math.round(height) 
        });
      }
    }, 100); // 100ms debounce

    // Use ResizeObserver on the app root to measure actual content
    const resizeObserver = new ResizeObserver(entries => {
      if (entries.length === 0) return;
      
      const entry = entries[0];
      // Measure the actual content dimensions
      // Use scrollHeight if larger than clientHeight, otherwise use clientHeight
      const contentHeight = Math.max(appRoot.scrollHeight, appRoot.clientHeight);
      const contentWidth = entry.contentRect.width;
      
      console.log(`ResizeObserver: scrollHeight=${appRoot.scrollHeight}, clientHeight=${appRoot.clientHeight}, using=${contentHeight}`);
      reportResize(contentWidth, contentHeight);
    });
    
    resizeObserver.observe(appRoot);
    
    // Also use MutationObserver to catch DOM changes that ResizeObserver might miss
    const mutationObserver = new MutationObserver(() => {
      // Small delay to let DOM settle
      setTimeout(() => {
        const contentHeight = Math.max(appRoot.scrollHeight, appRoot.clientHeight);
        const contentWidth = appRoot.getBoundingClientRect().width;
        console.log(`MutationObserver: DOM changed, measuring ${contentWidth}x${contentHeight}`);
        reportResize(contentWidth, contentHeight);
      }, 50);
    });
    
    mutationObserver.observe(appRoot, {
      childList: true,
      subtree: true,
      attributes: true,
    });
    
    // Also observe window resize to reflow content
    const handleWindowResize = () => {
      // Trigger a measurement after window resize
      setTimeout(() => {
        const contentHeight = Math.max(appRoot.scrollHeight, appRoot.clientHeight);
        const contentWidth = appRoot.getBoundingClientRect().width;
        reportResize(contentWidth, contentHeight);
      }, 50);
    };
    
    window.addEventListener('resize', handleWindowResize);
    
    return () => {
      resizeObserver.disconnect();
      mutationObserver.disconnect();
      window.removeEventListener('resize', handleWindowResize);
    };
  }, [id, sendToHazel]);

  return {
    setSyntax,
    resize,
    requestFocus,
  };
}
