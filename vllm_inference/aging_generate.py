import os
import re
import sys
import json
import argparse
from model_processor import ModelProcessor
from tqdm import tqdm

def parse_args():
    parser = argparse.ArgumentParser(description="Test Sample Builder")
    parser.add_argument('--model', required=True, help='Path to the model directory')
    parser.add_argument('--data_file', required=True, help='Path to the input data file')
    parser.add_argument('--cache_file', required=True, help='Path to the cache file directory')
    parser.add_argument('--prompt', default=None, help='Path to the prompt file')
    parser.add_argument('--do_sample', action='store_true', help='Whether to use sampling')
    parser.add_argument('--num_return_sequences', type=int, default=1, help='Number of sequences to return')
    parser.add_argument('--temperature', type=float, default=0, help='Sampling temperature')
    parser.add_argument('--max_tokens', type=int, default=1024, help='Maximum number of tokens')
    return parser.parse_args()

def load_data(file_path):
    """Load JSONL data from the given file path."""
    with open(file_path, 'r', encoding='utf-8') as file:
        data = [json.loads(line) for line in file]
    return data

def process_prompt(case, prompt):
    """Apply the prompt to the given case."""
    input_text = prompt
    for key in re.findall(r"\{(.+?)\}", input_text):
        if key in case:
            input_text = input_text.replace(f"{{{key}}}", case[key])

    return input_text

def process_file(data_file, cache_file, model_processor, prompt, args):
    """Process the input data file and generate outputs using the model processor."""
    data = load_data(data_file)
    
    # Set generation parameters
    model_processor.set_generation_params(
        do_sample=args.do_sample,
        num_return_sequences=args.num_return_sequences,
        temperature=args.temperature,
        max_tokens=args.max_tokens,
    )
    
    # Apply the prompt to all cases at once
    input_texts = [process_prompt(case, prompt) for case in data]
    
    # Generate output for all input texts in a batch
    generated_outputs = model_processor.generate_aging(input_texts)
    
    # Write the results to the cache file
    with open(cache_file, 'w', encoding='utf-8') as output_file:
        for case, generated_output in zip(data, generated_outputs):
            case["model_generated_aging_prediction"] = generated_output
            json.dump(case, output_file, ensure_ascii=False)
            output_file.write('\n')

def main():
    args = parse_args()
    # Load the prompt prompt if provided
    prompt = None
    if args.prompt:
        # 直接读取 args.prompt 作为路径
        with open(args.prompt, 'r', encoding='utf8') as f:
            prompt = f.read()
    else:
        raise ValueError("Error: The prompt file is empty or could not be read. Please check the file path and contents.")

    # Initialize ModelProcessor
    model_processor = ModelProcessor(model_dir=args.model)
    
    # Process the input file and generate output
    process_file(args.data_file, args.cache_file, model_processor, prompt, args)
    
    print(f"Processing completed for {args.data_file}. Output saved to {args.cache_file}")

if __name__ == "__main__":
    main()
