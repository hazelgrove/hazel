/**
 * Types for communication protocol between external apps and Hazel
 * Message types matching HazelProtocol.re
 */

// Value types for different codecs
export type CodecValue = string | number | boolean;

// Messages sent from child (js app) to parent (Hazel)
export type ToHazelMessage = 
  | { type: 'ready'; id: string }
  | { type: 'setSyntax'; id: string; codec: string; value: CodecValue }
  | { type: 'resize'; id: string; width: number; height: number }
  | { type: 'requestFocus'; id: string };

// Messages sent from parent (Hazel) to child (js app)
export type FromHazelMessage =
  | { type: 'init'; id: string; value: CodecValue }
  | { type: 'update'; id: string; value: CodecValue };

export function isFromHazelMessage(data: unknown): data is FromHazelMessage {
  return data !== null && typeof data === 'object' && 
         'type' in data && 'id' in data &&
         ['init', 'update'].includes((data as Record<string, unknown>).type as string) &&
         typeof (data as Record<string, unknown>).id === 'string';
}

export function createToHazelMessage(msg: ToHazelMessage): ToHazelMessage {
  return msg;
}
