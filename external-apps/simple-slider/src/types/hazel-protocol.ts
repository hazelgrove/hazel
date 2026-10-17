/**
 * Types for communication protocol between external apps and Hazel
 * Re-exported from shared base for compatibility
 */

export {
  type ToHazelMessage,
  type FromHazelMessage,
  isFromHazelMessage,
} from "../hooks/hazel-integration-base";

export function createToHazelMessage(msg: ToHazelMessage): ToHazelMessage {
  return msg;
}

// Import the types for convenience
import type { ToHazelMessage } from "../hooks/hazel-integration-base";
export type { ToHazelMessage as ToHazel, ToHazelMessage as FromHazel };