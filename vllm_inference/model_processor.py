from vllm import LLM, SamplingParams
from transformers import AutoTokenizer

class ModelProcessor:
    def __init__(self, model_dir: str, sc: int = 1, tensor_parallel_size: int = 1,
                 trust_remote_code: bool = True, gpu_memory_utilization: float = 0.9, dtype: str = "bfloat16"):
        """
        Initializes the ModelProcessor instance.
        Args:
            model_dir (str): The directory containing the model.
            sc (int): Scaling factor.
            tensor_parallel_size (int): The number of devices for tensor parallelism.
            trust_remote_code (bool): Flag for trusting remote code.
            gpu_memory_utilization (float): GPU memory utilization percentage.
            dtype (str): Data type used for model computation.
        """
        self.model = LLM(
            model=model_dir,
            tokenizer=model_dir,
            tensor_parallel_size=tensor_parallel_size,
            trust_remote_code=trust_remote_code,
            gpu_memory_utilization=gpu_memory_utilization,
            dtype=dtype
        )
        self.tokenizer = AutoTokenizer.from_pretrained(model_dir, trust_remote_code=True)
        self.sc = sc

        # Default generation parameters
        self.sampling_params = None

    def set_generation_params(self, do_sample=False, num_return_sequences=1, temperature=0, max_tokens=1024, 
                              stop_tokens=None, stop_token_ids=None, prompt_logprobs=None, logprobs=None):
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
            stop=stop_tokens,
            stop_token_ids=stop_token_ids,
            prompt_logprobs=prompt_logprobs,
            logprobs=logprobs
        )

    def generate_text(self, prompts):
        """Generates text based on the provided prompts."""
        if self.sampling_params:
            # Ensure sampling parameters are set
            batch_generate_text = self.model.generate(prompts, self.sampling_params, use_tqdm=True)
            return [[completion.text for completion in generate_text.outputs] for generate_text in batch_generate_text]
        else:
            raise ValueError("Generation parameters not set. Please call set_generation_params first.")
