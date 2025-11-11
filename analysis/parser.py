#!/usr/bin/env python3
"""
Hazel Log Parser

Parses Hazel student submission JSON files and extracts structured log data
for analysis of student interaction patterns.
"""

import json
import re
from datetime import datetime
from typing import List, Dict, Any, Tuple, Union
import pandas as pd


class SExpressionParser:
    """Parser for s-expression format used in Hazel logs."""
    
    def __init__(self):
        self.tokens = []
        self.pos = 0
    
    def tokenize(self, s: str) -> List[str]:
        """Tokenize s-expression string into tokens."""
        # Split on whitespace and parentheses, keeping parentheses as separate tokens
        tokens = re.findall(r'\(|\)|[^\s()]+', s)
        return tokens
    
    def parse(self, s: str) -> Any:
        """Parse s-expression string into nested Python data structures."""
        self.tokens = self.tokenize(s)
        self.pos = 0
        return self._parse_expression()
    
    def _parse_expression(self) -> Any:
        """Parse a single expression."""
        if self.pos >= len(self.tokens):
            return None
        
        token = self.tokens[self.pos]
        
        if token == '(':
            # Start of a list
            self.pos += 1
            result = []
            while self.pos < len(self.tokens) and self.tokens[self.pos] != ')':
                result.append(self._parse_expression())
            if self.pos < len(self.tokens) and self.tokens[self.pos] == ')':
                self.pos += 1
            return result
        elif token == ')':
            # End of list (shouldn't happen in well-formed expressions)
            return None
        else:
            # Atom
            self.pos += 1
            # Try to convert to number if possible
            try:
                if '.' in token:
                    return float(token)
                else:
                    return int(token)
            except ValueError:
                return token


class HazelLogParser:
    """Main parser for Hazel submission files."""
    
    def __init__(self):
        self.sexpr_parser = SExpressionParser()
    
    def load_json_file(self, filepath: str) -> Dict[str, Any]:
        """Load and parse Hazel submission JSON file."""
        with open(filepath, 'r') as f:
            return json.load(f)
    
    def extract_log_data(self, json_data: Dict[str, Any]) -> str:
        """Extract the log field from JSON data."""
        if 'log' not in json_data:
            raise ValueError("No 'log' field found in JSON data")
        return json_data['log']
    
    def parse_log_entries(self, log_string: str) -> List[Tuple[float, Any]]:
        """Parse log string into list of (timestamp, action) tuples."""
        import re
        
        # Handle Hazel's actual log format: ((timestamp action) (timestamp action) ...)
        # Use regex to extract timestamp-action pairs
        action_pattern = r'(\d{13})\s*\(([^)]+)\)'
        matches = re.findall(action_pattern, log_string)
        
        entries = []
        for timestamp_str, action in matches:
            try:
                timestamp = float(timestamp_str)
                entries.append((timestamp, action))
            except ValueError:
                continue
        
        return entries
    
    def flatten_action(self, action: Any, prefix: str = "") -> Dict[str, Any]:
        """Flatten nested action structure for easier analysis."""
        result = {}
        
        if isinstance(action, list):
            if len(action) == 0:
                result[f"{prefix}type"] = "Empty"
            elif len(action) == 1:
                result[f"{prefix}type"] = str(action[0])
            else:
                result[f"{prefix}type"] = str(action[0])
                # Recursively flatten the rest
                for i, item in enumerate(action[1:]):
                    if isinstance(item, list):
                        sub_result = self.flatten_action(item, f"{prefix}arg{i}_")
                        result.update(sub_result)
                    else:
                        result[f"{prefix}arg{i}"] = item
        else:
            result[f"{prefix}type"] = str(action)
        
        return result
    
    def parse_file(self, filepath: str) -> pd.DataFrame:
        """Parse a Hazel submission file and return structured data."""
        # Load JSON
        json_data = self.load_json_file(filepath)
        
        # Extract log
        log_string = self.extract_log_data(json_data)
        
        # Parse log entries
        entries = self.parse_log_entries(log_string)
        
        # Convert to structured records
        records = []
        for timestamp, action in entries:
            # Convert timestamp to datetime
            dt = datetime.fromtimestamp(timestamp / 1000.0)
            
            # Flatten action
            action_flat = self.flatten_action(action)
            
            # Create record
            record = {
                'timestamp': timestamp,
                'datetime': dt,
                'action_raw': action,
                **action_flat
            }
            records.append(record)
        
        return pd.DataFrame(records)
    
    def get_action_summary(self, df: pd.DataFrame) -> Dict[str, Any]:
        """Get summary statistics about actions in the log."""
        if len(df) == 0:
            return {}
        
        # Time range
        time_range = {
            'start': df['datetime'].min(),
            'end': df['datetime'].max(),
            'duration_seconds': (df['datetime'].max() - df['datetime'].min()).total_seconds()
        }
        
        # Action type distribution
        action_types = df['type'].value_counts().to_dict()
        
        # Time between actions
        df_sorted = df.sort_values('timestamp')
        time_diffs = df_sorted['timestamp'].diff().dropna() / 1000.0  # Convert to seconds
        
        return {
            'total_actions': len(df),
            'time_range': time_range,
            'action_types': action_types,
            'time_between_actions': {
                'mean_seconds': time_diffs.mean(),
                'median_seconds': time_diffs.median(),
                'std_seconds': time_diffs.std(),
                'min_seconds': time_diffs.min(),
                'max_seconds': time_diffs.max()
            }
        }


def main():
    """Example usage of the parser."""
    import sys
    
    if len(sys.argv) != 2:
        print("Usage: python parser.py <hazel_submission.json>")
        sys.exit(1)
    
    filepath = sys.argv[1]
    parser = HazelLogParser()
    
    try:
        # Parse the file
        df = parser.parse_file(filepath)
        
        # Print basic info
        print(f"Parsed {len(df)} log entries")
        print(f"Time range: {df['datetime'].min()} to {df['datetime'].max()}")
        print(f"Duration: {(df['datetime'].max() - df['datetime'].min()).total_seconds():.1f} seconds")
        
        # Print action type summary
        print("\nAction types:")
        for action_type, count in df['type'].value_counts().head(10).items():
            print(f"  {action_type}: {count}")
        
        # Get detailed summary
        summary = parser.get_action_summary(df)
        print(f"\nTime between actions:")
        print(f"  Mean: {summary['time_between_actions']['mean_seconds']:.2f}s")
        print(f"  Median: {summary['time_between_actions']['median_seconds']:.2f}s")
        
    except Exception as e:
        print(f"Error parsing file: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
