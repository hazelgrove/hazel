/**
 * Types for communication protocol between external React apps and Hazel
 */

// Messages sent from child (React app) to parent (Hazel)
export type ToHazelMessage = 
  | { type: 'ready'; id: string }
  | { type: 'setSyntax'; id: string; codec: string; value: any }
  | { type: 'resize'; id: string; width: number; height: number }
  | { type: 'requestFocus'; id: string };

// Messages sent from parent (Hazel) to child (React app)
export type FromHazelMessage =
  | { type: 'init'; id: string; value: any }
  | { type: 'update'; id: string; value: any };

// Type guard functions
export function isFromHazelMessage(data: any): data is FromHazelMessage {
  return data && typeof data === 'object' && 
         ['init', 'update'].includes(data.type) &&
         typeof data.id === 'string';
}

export function createToHazelMessage(msg: ToHazelMessage): ToHazelMessage {
  return msg;
}
