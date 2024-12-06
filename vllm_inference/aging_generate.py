import os
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
    parser.add_argument('--template', default=None, help='Path to the template prompt file (optional)')
    parser.add_argument('--do_sample', action='store_true', help='Whether to use sampling')
    parser.add_argument('--num_return_sequences', type=int, default=1, help='Number of sequences to return')
    parser.add_argument('--temperature', type=float, default=0, help='Sampling temperature')
    parser.add_argument('--max_tokens', type=int, default=1024, help='Maximum number of tokens')
    parser.add_argument('--stop_tokens', type=str, nargs='*', default=[], help='List of stop tokens')
    parser.add_argument('--stop_token_ids', type=int, nargs='*', default=[], help='List of stop token IDs')
    parser.add_argument('--logprobs', type=int, default=None, help='Log probabilities for each token')
    return parser.parse_args()

def load_data(file_path):
    """Load JSONL data from the given file path."""
    with open(file_path, 'r', encoding='utf-8') as file:
        data = [json.loads(line) for line in file]
    return data

def process_prompt(case, prompt):
    """Apply the prompt template to the given case."""
    if prompt:
        input_text = prompt
        for key in re.findall(r"\{(.+?)\}", input_text):
            if key in case:
                input_text = input_text.replace(f"{{{key}}}", case[key])
    else:
        input_text = case.get('input', '')
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
        stop_tokens=args.stop_tokens,
        stop_token_ids=args.stop_token_ids,
        logprobs=args.logprobs
    )
    
    with open(cache_file, 'w', encoding='utf-8') as output_file:
        for case in tqdm(data, desc="Processing cases"):
            # Apply the prompt to each case
            input_text = process_prompt(case, prompt)
            
            # Generate output using the model processor
            generated_output = model_processor.generate_text([input_text])[0]
            
            # Write the results to the cache file
            case["model_generated_output"] = generated_output
            json.dump(case, output_file, ensure_ascii=False)
            output_file.write('\n')

def main():
    args = parse_args()
    
    # Load the prompt template if provided
    prompt = None
    if args.template:
        with open(f'prompt/{args.template}', 'r', encoding='utf8') as f:
            prompt = f.read()

    # Initialize ModelProcessor
    model_processor = ModelProcessor(
        model_dir=args.model,
        sc=1  # scaling factor, can be customized
    )
    
    # Process the input file and generate output
    process_file(args.data_file, args.cache_file, model_processor, prompt, args)
    print(f"Processing completed for {args.data_file}. Output saved to {args.cache_file}")

if __name__ == "__main__":
    main()
