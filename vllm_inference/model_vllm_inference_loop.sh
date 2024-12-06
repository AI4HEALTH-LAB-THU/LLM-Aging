#!/bin/bash

# Description:
# This script processes JSONL files in the source_data_split directory.
# It checks if the corresponding file exists in the cache directory.
# If the file exists, it skips processing; if it doesn't, it creates an empty file and processes it.
# The script uses multiple GPUs (CUDA_VISIBLE_DEVICES) to run a Python script (aging_generate.py)
# and generates output files in the result_split directory. If the output file is empty, it will be removed,
# and the script will stop further processing.

# Define model path, input data path, cache path, and prompt template file
model_path="meta-llama/Meta-Llama-3-70B-Instruct"
data_file_path="./data/source_data/source_data_split"
cache_file="./data/result/result_split"
prompt_template=main_analysis.txt

# Iterate over each JSONL file in the source_data_split directory
for data_file in $data_file_path/*.jsonl; do
    # Get the filename from the full path
    filename=$(basename "$data_file")
    
    # Check if the file already exists in the cache directory
    if [ -f "$cache_file/$filename" ]; then
        echo "File $filename already exists in the cache, skipping processing."
        continue
    else
        echo "File $filename does not exist in the cache, creating an empty file and continuing processing."
        # Create an empty file in the cache directory
        touch "$cache_file/$filename"
    fi

    # Execute the command to process the file using the model
    python3 aging_generate.py \
        --model $model_path \
        --template $prompt_template \
        --data_file $data_file \
        --cache_file $cache_file/$filename

    # Check if the generated file is empty and remove it if necessary
    if [ ! -s "$cache_file/$filename" ]; then
        echo "File $filename is empty after processing, exiting the loop."
        rm "$cache_file/$filename"
        break
    fi
done
