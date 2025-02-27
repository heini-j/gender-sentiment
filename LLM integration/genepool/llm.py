import os
from openai import OpenAI

LLM_PARAMS = {
    "model": "meta/llama-3.3-70b-instruct",
    "temperature": 0.1,
    "max_tokens": 1024,
    "stream": False,
}

BATCH_SIZE = 20


def get_client():
    return OpenAI(
        base_url="https://integrate.api.nvidia.com/v1",
        api_key=os.environ["NVIDIA_API_KEY"],
    )


def get_system_message(num_paragraphs, politician):
    return [
        {
            "role": "system",
            "content": (
                f"For the following {num_paragraphs} paragraphs of text that discuss Swiss politics in German (meaning each paragraph runs between two newline characters)"
                f"provide a number for the sentiment conveyed in the text about a politician called {politician} from 0 (very negative) to 10 (very positive) and 5 is neutral."
                "The paragraphs can include sarcasm, irony, or other forms of indirect sentiment."
                "Please ONLY provide the sentiment number separated by a comma character."
            ),
        },
        {
            "role": "system",
            "content": (
                "Input: Frau Müller ist die beste Vertreterin für aussenpolitische Fragen.\n Bundesrat Hansen ist der schlechste Politikern der wir je hatten.\n"
                "Output: 10,0"
            ),
        },
    ]


def analyse_batch_with_llm(text, politician, client):
    num_paragraphs = len(text.split("\n"))

    completion = client.chat.completions.create(
        messages=get_system_message(num_paragraphs, politician)
        + [
            {"role": "user", "content": text},
        ],
        **LLM_PARAMS,
    )

    raw_response = completion.choices[0].message.content.split(",")
    response = [rating.strip() for rating in raw_response]
    ratings = [int(rating) for rating in response if rating.isdigit()]

    if num_paragraphs != len(ratings):
        raise ValueError(
            f"Error: Number of ratings {len(ratings)} does not match {num_paragraphs=}."
        )

    return ratings


def analyse_text_with_llm(text, politician):
    client = get_client()

    paragraphs = text.split("\n")
    num_batches = len(paragraphs) // BATCH_SIZE

    # Generate batches, analyse them and flatten the results.
    ratings = []
    for batch_index in range(0, len(paragraphs), BATCH_SIZE):
        print(f"Processing text {batch_index + 1}/{len(paragraphs)}")

        text_batch = "\n".join(paragraphs[batch_index : batch_index + BATCH_SIZE])
        ratings.extend(analyse_batch_with_llm(text_batch, politician, client))

    return ratings
