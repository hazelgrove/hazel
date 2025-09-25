# Automated tests for LLM task completion
Example run command:
```
cd src/llm-tests
```
`headless`, `attemptTimeoutMs`, and `retries` are optional arguments, with their default values shown in the example run command below:
```
ts-node run-task.ts \
  --task tasks/test.yaml \
  --apiKey <Your OpenRouter API Key> \
  --model deepseek/deepseek-chat-v3.1:free
  --url https://hazel.org/build/assistant-actions-v2
  --headless true
  --outputDir test-results
  --attemptTimeoutMs 120000
  --retries 2
```