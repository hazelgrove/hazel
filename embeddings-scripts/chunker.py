file_path = "/Users/andrewblinn/Dropbox/projects/hazel3/all.haze"
api_key_path = "/Users/personal-openai-api-key.txt" 
chunk_length = 150

with open(file_path, "r") as file:
    text = file.read()

chunks = [text[i:i+chunk_length] for i in range(0, len(text), chunk_length)]

for chunk in chunks:
    print(chunk)
    print("$$$")
