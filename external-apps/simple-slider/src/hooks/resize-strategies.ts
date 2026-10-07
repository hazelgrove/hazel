import type { ResizeStrategy, ToHazelMessage } from "./hazel-integration-base";

/**
 * Basic resize strategy - reports document size
 * Good for: Static/fixed-size apps
 */
export class BasicResizeStrategy implements ResizeStrategy {
  setup(params: {
    id: string;
    sendToHazel: (message: ToHazelMessage) => void;
  }) {
    const { id, sendToHazel } = params;

    const resizeObserver = new ResizeObserver((entries) => {
      const rect = entries[0]?.contentRect;
      if (!rect) return;

      sendToHazel({
        type: "resize",
        id,
        width: Math.round(rect.width),
        height: Math.round(rect.height),
      });
    });

    resizeObserver.observe(document.documentElement);

    return () => {
      resizeObserver.disconnect();
    };
  }
}

/**
 * Content-aware resize strategy - measures actual content with debouncing
 * Good for: Dynamic/growing content apps
 */
export class ContentAwareResizeStrategy implements ResizeStrategy {
  private debounceMs: number;
  private enableLogging: boolean;

  constructor(options: { debounceMs?: number; enableLogging?: boolean } = {}) {
    this.debounceMs = options.debounceMs ?? 100;
    this.enableLogging = options.enableLogging ?? false;
  }

  setup(params: {
    id: string;
    sendToHazel: (message: ToHazelMessage) => void;
  }) {
    const { id, sendToHazel } = params;
    const appRoot = document.getElementById("root") || document.body;
    let lastWidth = 0;
    let lastHeight = 0;

    // Throttled resize reporter
    const reportResize = this.throttle((width: number, height: number) => {
      // Only send if there's a meaningful change (avoid spam)
      if (
        Math.abs(width - lastWidth) >= 2 ||
        Math.abs(height - lastHeight) >= 2
      ) {
        lastWidth = width;
        lastHeight = height;

        if (this.enableLogging) {
          console.log(`[ContentAware] Reporting resize: ${width}x${height}`);
        }

        sendToHazel({
          type: "resize",
          id,
          width: Math.round(width),
          height: Math.round(height),
        });
      }
    }, this.debounceMs);

    // Use ResizeObserver on the app root to measure actual content
    const resizeObserver = new ResizeObserver((entries) => {
      if (entries.length === 0) return;

      const entry = entries[0];
      // Measure the actual content dimensions
      // Use scrollHeight if larger than clientHeight, otherwise use clientHeight
      const contentHeight = Math.max(
        appRoot.scrollHeight,
        appRoot.clientHeight
      );
      const contentWidth = entry.contentRect.width;

      if (this.enableLogging) {
        console.log(
          `[ContentAware] ResizeObserver: scrollHeight=${appRoot.scrollHeight}, clientHeight=${appRoot.clientHeight}, using=${contentHeight}`
        );
      }

      reportResize(contentWidth, contentHeight);
    });

    resizeObserver.observe(appRoot);

    // Also use MutationObserver to catch DOM changes that ResizeObserver might miss
    const mutationObserver = new MutationObserver(() => {
      // Small delay to let DOM settle
      setTimeout(() => {
        const contentHeight = Math.max(
          appRoot.scrollHeight,
          appRoot.clientHeight
        );
        const contentWidth = appRoot.getBoundingClientRect().width;

        if (this.enableLogging) {
          console.log(
            `[ContentAware] MutationObserver: DOM changed, measuring ${contentWidth}x${contentHeight}`
          );
        }

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
        const contentHeight = Math.max(
          appRoot.scrollHeight,
          appRoot.clientHeight
        );
        const contentWidth = appRoot.getBoundingClientRect().width;

        if (this.enableLogging) {
          console.log(
            `[ContentAware] Window resize, measuring ${contentWidth}x${contentHeight}`
          );
        }

        reportResize(contentWidth, contentHeight);
      }, 50);
    };

    window.addEventListener("resize", handleWindowResize);

    return () => {
      resizeObserver.disconnect();
      mutationObserver.disconnect();
      window.removeEventListener("resize", handleWindowResize);
    };
  }

  // Simple trailing throttle utility
  private throttle<T extends (...args: any[]) => void>(
    fn: T,
    ms: number
  ): T {
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
}

/**
 * No-op resize strategy - for apps that handle resize manually
 */
export class NoResizeStrategy implements ResizeStrategy {
  setup() {
    // Do nothing, app will call resize() manually
    return () => {};
  }
}

// Convenience factory functions
export const createBasicResize = () => new BasicResizeStrategy();
export const createContentAwareResize = (options?: {
  debounceMs?: number;
  enableLogging?: boolean;
}) => new ContentAwareResizeStrategy(options);
export const createNoResize = () => new NoResizeStrategy();
