#!/bin/bash

# Description:
# This script processes a single JSONL file and generates output using a specified model.
# If the output file is empty, it will be removed, and the script will exit.

# Define model path, input data path, cache path, and prompt template file
infer_mode=main_analysis  #["main_analysis", "sensitivity_analysis", "chinese_prompt", "additional_analysis"]
model_path="meta-llama/Meta-Llama-3-8B-Instruct"
data_file_path="./data/source_data/health_info_example.jsonl"
cache_file="./data/result/health_info-${infer_mode}-result-example.jsonl"
prompt_template="./prompts/${infer_mode}.txt"

# Check if the file already exists in the cache directory
if [ -f "$cache_file" ]; then
    echo "File already exists in the cache, skipping processing."
    exit 0
else
    echo "File does not exist in the cache, creating an empty file and continuing processing."
    # Create an empty file in the cache directory
    touch "$cache_file"
fi

# Execute the command to process the file using the model
python3 aging_generate.py \
    --model $model_path \
    --prompt $prompt_template \
    --data_file $data_file_path \
    --cache_file $cache_file

# Check if the generated file is empty and remove it if necessary
if [ ! -s "$cache_file" ]; then
    echo "File is empty after processing, removing it and exiting."
    rm "$cache_file"
    exit 1
fi