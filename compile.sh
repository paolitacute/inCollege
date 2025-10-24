#!/bin/bash

# --- Configuration ---
SRC_DIR="src"
BIN_DIR="bin"
EXECUTABLE_NAME="inCollege"

# Define the output file path
OUTPUT_FILE="${BIN_DIR}/${EXECUTABLE_NAME}"

# Define the source files, ensuring incollege.cob is first
MAIN_FILE="${SRC_DIR}/incollege.cob"
# Find all other .cob files in the src directory
# We use find to safely get all other .cob files, excluding the main one
OTHER_FILES=$(find "${SRC_DIR}" -name "*.cob" -not -name "incollege.cob")

# Combine them, with the main file first
SOURCE_FILES="$MAIN_FILE $OTHER_FILES"

# --- Script ---
echo "Starting COBOL compilation..."
set -e # Exit immediately if any command fails

# 1. Create the binary directory if it doesn't exist
mkdir -p "$BIN_DIR"
echo "Ensured binary directory exists: $BIN_DIR"

# 2. Run the COBOL compiler command
#    -x : Creates an executable
#    $SOURCE_FILES : Expands to "src/incollege.cob" followed by all other .cob files
#    -o "$OUTPUT_FILE" : Specifies the output file path
echo "Compiling main file $MAIN_FILE and other files..."
echo "Files to compile: $SOURCE_FILES"
cobc -x $SOURCE_FILES -o "$OUTPUT_FILE"

# 3. Confirm success
echo "----------------------------------------"
echo "Compilation successful!"
echo "Executable created at: $OUTPUT_FILE"

