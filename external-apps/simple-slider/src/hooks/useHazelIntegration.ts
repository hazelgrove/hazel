import { useHazelIntegrationBase } from "./hazel-integration-base";
import { createBasicResize } from "./resize-strategies";

/**
 * Simple-slider specific Hazel integration
 * Uses basic resize strategy since slider has fixed dimensions
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
    resizeStrategy: createBasicResize(),
  });
}