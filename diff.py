import subprocess
import difflib

# Run `node _build/default/src/hazelcli/cli.bc.js generate-test` to generate the test data
# Use the data and then call `node _build/default/src/hazelcli/cli.bc.js run -` with the test data in stdin
# Run the same data with `node /Users/alexanderbandukwala/Projects/hazel-tdos/_build/default/src/hazelcli/cli.bc.js run -` to get the output
# Diff the output with the expected output

def generate_test_data():
    """Generates test data using the hazelcli tool."""
    command = ["node", "_build/default/src/hazelcli/cli.bc.js", "generate-test"]
    result = subprocess.run(command, capture_output=True, text=True, check=True)
    return result.stdout

def run_test_with_data(test_data, cli_path):
    """Runs the test using the hazelcli tool with the provided test data."""
    command = ["node", cli_path, "run", "-"]
    result = subprocess.run(command, input=test_data, text=True, capture_output=True, check=True)
    return result.stdout

if __name__ == "__main__":
    while True:
        try:
            # Generate test data
            test_data = generate_test_data()
            print("Generated Test Data:")
            print(test_data)

            # Run the test with the first version
            output_v1 = run_test_with_data(test_data, "_build/default/src/hazelcli/cli.bc.js")
            print("Test Output (Version 1):")
            print(output_v1)

            # Run the test with the second version
            output_v2 = run_test_with_data(test_data, "/Users/alexanderbandukwala/Projects/hazel-tdos/_build/default/src/hazelcli/cli.bc.js")
            print("Test Output (Version 2):")
            print(output_v2)

            # Diff the outputs
            diff = list(difflib.unified_diff(
                output_v1.splitlines(), 
                output_v2.splitlines(), 
                lineterm="", 
                fromfile="Version 1", 
                tofile="Version 2"
            ))
            
            if diff:
                print("Diff between Version 1 and Version 2:")
                print("\n".join(diff))
                break
            else:
                print("No differences found between Version 1 and Version 2.")
        except subprocess.CalledProcessError as e:
            print(f"An error occurred: {e.stderr}")