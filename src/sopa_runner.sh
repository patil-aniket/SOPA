#!/bin/bash

# Paths to Prolog files
# change the directory name for Tokeniser, Grammer and Evaluator in the script, as per the location in your system
TOKENISER="/Users/satyamshekhar/Desktop/sopa/tokeniser.pl"
GRAMMAR="/Users/satyamshekhar/Desktop/sopa/grammar.pl"
EVALUATOR="/Users/satyamshekhar/Desktop/sopa/Evaluator.pl"

# Ensure Prolog files exist
for file in "$TOKENISER" "$GRAMMAR" "$EVALUATOR"; do
    if [ ! -f "$file" ]; then
        echo "Error: File $file not found!"
        exit 1
    fi
done

# Check if input program is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <path_to_program>"
    exit 1
fi

INPUT_PROGRAM="$1"

# Ensure input program exists
if [ ! -f "$INPUT_PROGRAM" ]; then
    echo "Error: Input program file '$INPUT_PROGRAM' not found!"
    exit 1
fi

# Read the content of the input program and normalize it
INPUT_CONTENT=$(cat "$INPUT_PROGRAM" | tr '\n' ' ')

# Run the Prolog pipeline
# Run the Prolog pipeline
swipl -g "
    consult('${TOKENISER}'),
    consult('${GRAMMAR}'),
    consult('${EVALUATOR}'),
    tokenize('${INPUT_CONTENT}', Tokens),
    ( program(ParsedGrammar, Tokens, _)
    -> eval_program(ParsedGrammar, Result),
       halt  % Removes display of intermediate results like variables
    ;  write('Pipeline failed.'), nl, halt).
" -t halt


if [ $? -ne 0 ]; then
    echo "Error running the Sopa compiler."
    exit 1
fi
