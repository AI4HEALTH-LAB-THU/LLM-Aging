import torch
from vllm import LLM, SamplingParams
from transformers import AutoTokenizer

class ModelProcessor:
    def __init__(self, model_dir: str):
        """
        Initializes the ModelProcessor instance.
        Args:
            model_dir (str): The directory containing the model.
            tensor_parallel_size (int): The number of devices for tensor parallelism.
            trust_remote_code (bool): Flag for trusting remote code.
            gpu_memory_utilization (float): GPU memory utilization percentage.
            dtype (str): Data type used for model computation.
        """
        self.model = LLM(model=model_dir, trust_remote_code=True,
            tensor_parallel_size=torch.cuda.device_count(), 
            gpu_memory_utilization=0.9,
            enable_prefix_caching=False,
            max_num_seqs=128,
        )
        self.tokenizer = AutoTokenizer.from_pretrained(model_dir, trust_remote_code=True)

        # Default generation parameters
        self.sampling_params = None

    def set_generation_params(self, do_sample=False, num_return_sequences=1, temperature=0, max_tokens=1024):
        """
        Sets the generation parameters for the model.
        Args:
            do_sample (bool): Whether to use sampling.
            num_return_sequences (int): The number of sequences to return.
            temperature (float): The sampling temperature.
            max_tokens (int): The maximum number of tokens to generate.
            stop_tokens (list): A list of stop tokens.
            stop_token_ids (list): A list of stop token IDs.
            prompt_logprobs (int): Log probabilities for each token.
            logprobs (int): Number of top log probabilities to return.
        """
        self.sampling_params = SamplingParams(
            n=num_return_sequences,
            temperature=temperature,
            max_tokens=max_tokens,
        )

    def generate_aging(self, prompts):
        """Generates text based on the provided prompts."""
        if self.sampling_params:
            # Ensure sampling parameters are set
            batch_generate_text = self.model.generate(prompts, self.sampling_params, use_tqdm=True)
            return [[completion.text for completion in generate_text.outputs] for generate_text in batch_generate_text]
        else:
            raise ValueError("Generation parameters not set. Please call set_generation_params first.")
