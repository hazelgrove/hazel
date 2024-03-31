import json
import sys
import openai
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity

api_key_path = "/Users/andrewblinn/personal-openai-api-key.txt"
embeddings_path = "embeddings.json"

with open(api_key_path, "r") as file:
    openai.api_key = file.read().strip()

def find_similar_chunks(header, embeddings, top_n=6):
    # Convert the header to an embedding
    header_embedding = openai.embeddings.create(input=header, model="text-embedding-ada-002").data[0].embedding

    # Calculate the cosine similarity between the header embedding and chunk embeddings
    chunk_embeddings = [embedding["embedding"] for embedding in embeddings]
    similarities = cosine_similarity([header_embedding], chunk_embeddings)[0]

    # Get the indices of the top-n most similar chunks
    top_indices = similarities.argsort()[-top_n:][::-1]

    # Return the top-n most similar chunks
    return [embeddings[i]["chunk"] for i in top_indices]

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python find_similar_chunks.py <header>")
        sys.exit(1)

    header = sys.argv[1]

    # Load the pre-generated embeddings from the JSON file
    with open(embeddings_path, "r") as file:
        embeddings = json.load(file)

    # Find the top-n most similar chunks
    similar_chunks = find_similar_chunks(header, embeddings)

    # Print the similar chunks
    print(f"Top {len(similar_chunks)} most similar chunks to '{header}':")
    for i, chunk in enumerate(similar_chunks, start=1):
        print(f"{i}. {chunk}")