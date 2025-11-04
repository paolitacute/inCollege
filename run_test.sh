#!/bin/bash

# --- Configuration ---
INPUT_DIR="test-input"
OUTPUT_DIR="test-output"
TARGET_INPUT_FILE="inCollege-Input.txt"
PROGRAM_OUTPUT_FILE="inCollege-Output.txt"
EXECUTABLE="./bin/incollege"

# --- Script ---

# Exit immediately if any command fails
set -e

# 1. Check if the 'incollege' executable exists and is executable
if [ ! -x "$EXECUTABLE" ]; then
    echo "Error: Executable not found or is not executable: $EXECUTABLE"
    exit 1
fi

# 2. Create the output directory if it doesn't already exist
mkdir -p "$OUTPUT_DIR"
echo "Ensured output directory exists: $OUTPUT_DIR"

# 3. Initialize a counter for the output files
counter=1

# 4. Loop through each .txt file found in the INPUT_DIR
#    We sort the files to ensure a consistent processing order.
for source_input_file in $(find "$INPUT_DIR" -maxdepth 1 -name "*.txt" -type f | sort); do
    echo "----------------------------------------"
    echo "Processing: $source_input_file"

    # Define the specific output file for this test using the counter
    final_output_file="${OUTPUT_DIR}/test-output${counter}.txt"

    # 5. Copy contents of the source file to inCollege-Input.txt
    echo "Copying $source_input_file to $TARGET_INPUT_FILE..."
    cp "$source_input_file" "$TARGET_INPUT_FILE"

    # 6. Run the command
    echo "Running $EXECUTABLE..."
    "$EXECUTABLE"

    # 7. Check if the program actually created its output file
    if [ ! -f "$PROGRAM_OUTPUT_FILE" ]; then
        echo "Warning: Program did not create $PROGRAM_OUTPUT_FILE. Cannot copy output for $source_input_file."
        # We still increment the counter so the next test output number is correct
        counter=$((counter + 1))
        continue # Skip to the next file
    fi

    # 8. Copy the output from inCollege-Output.txt to test-outputX.txt
    echo "Copying $PROGRAM_OUTPUT_FILE to $final_output_file..."
    cp "$PROGRAM_OUTPUT_FILE" "$final_output_file"

    echo "Successfully processed $source_input_file -> $final_output_file"

    # 9. Increment the counter for the next file
    counter=$((counter + 1))
done

echo "----------------------------------------"
if [ $counter -eq 1 ]; then
    echo "Warning: No input files (.txt) were found in $INPUT_DIR."
else
    echo "All tests complete."
fi