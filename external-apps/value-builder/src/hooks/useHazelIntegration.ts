import { useHazelIntegrationBase } from "./hazel-integration-base";
import { createContentAwareResize } from "./resize-strategies";

/**
 * Value-builder specific Hazel integration
 * Uses content-aware resize strategy since content grows dynamically
 */

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
}

export function useHazelIntegration(config: HazelIntegrationConfig) {
  return useHazelIntegrationBase({
    ...config,
    resizeStrategy: createContentAwareResize({
      debounceMs: 100,
      enableLogging: false, // Set to true for debugging
    }),
  });
}