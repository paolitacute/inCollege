#!/bin/bash

# --- Configuration ---
# This script assumes it is run from the root of your project directory.
#
# Directory containing your test inputs (e.g., test1_input.txt)
INPUT_DIR="test_input"
#
# Directory where final outputs will be stored (e.g., test1_output.txt)
OUTPUT_DIR="test_output"
#
# The file your 'incollege' program reads from
TARGET_INPUT_FILE="inCollege-Input.txt"
#
# The file your 'incollege' program writes its output to
PROGRAM_OUTPUT_FILE="inCollege-Output.txt"
#
# The path to your executable
EXECUTABLE="./bin/incollege"

# --- Script ---

# Exit immediately if any command fails
set -e

# 1. Check if any test names were provided as arguments
if [ $# -eq 0 ]; then
    echo "Usage: $0 test1 test2 test3 ..."
    echo "Example: ./run_tests.sh test1 test2"
    exit 1
fi

# 2. Check if the 'incollege' executable exists and is executable
if [ ! -x "$EXECUTABLE" ]; then
    echo "Error: Executable not found or is not executable: $EXECUTABLE"
    exit 1
fi

# 3. Create the output directory if it doesn't already exist
mkdir -p "$OUTPUT_DIR"
echo "Ensured output directory exists: $OUTPUT_DIR"

# 4. Loop through each test name provided as an argument
for test_name in "$@"; do
    echo "----------------------------------------"
    echo "Processing: $test_name"

    # Define the specific input and output files for this test
    source_input_file="${INPUT_DIR}/${test_name}_input.txt"
    final_output_file="${OUTPUT_DIR}/${test_name}_output.txt"

    # 5. Check if the source input file exists
    if [ ! -f "$source_input_file" ]; then
        echo "Warning: Input file not found: $source_input_file. Skipping $test_name."
        continue # Skip to the next test_name
    fi

    # 6. Copy contents of testx_input.txt to inCollege-Input.txt
    echo "Copying $source_input_file to $TARGET_INPUT_FILE..."
    cp "$source_input_file" "$TARGET_INPUT_FILE"

    # 7. Run the command
    echo "Running $EXECUTABLE..."
    "$EXECUTABLE"

    # 8. Check if the program actually created its output file
    if [ ! -f "$PROGRAM_OUTPUT_FILE" ]; then
        echo "Warning: Program did not create $PROGRAM_OUTPUT_FILE. Cannot copy output for $test_name."
        continue # Skip to the next test_name
    fi

    # 9. Copy the output from inCollege-Output.txt to testx_output.txt
    echo "Copying $PROGRAM_OUTPUT_FILE to $final_output_file..."
    cp "$PROGRAM_OUTPUT_FILE" "$final_output_file"

    echo "Successfully processed $test_name."
done

echo "----------------------------------------"
echo "All tests complete."
