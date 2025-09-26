
# Automated tests for LLM task completion


## Example Usage
```

$ cd src/llm-tests

```

```

$ ts-node run-task.ts \

--task tasks/test.yaml \

--apiKey <Your OpenRouter API Key> \

--model deepseek/deepseek-chat-v3.1:free

--url https://hazel.org/build/assistant-actions-v2

--headless true

--outputDir test-results

--attemptTimeoutMs 120000

--retries 2

```
## Arguments

`headless`, `attemptTimeoutMs`, and `retries` are optional arguments, with their default values shown in the example run command below:
| Argument | Default | Description |
|--|--|--|
| `task` | None | The path to the YAML file containing the task description |
| `apiKey` | None | Your OpenRouter API key |
| `model` | None | OpenRouter model ID as defined by their [API](https://openrouter.ai/api/v1/models)
| `url` | `http://0.0.0.0:8000/` | The URL of the Hazel instance to be tested |
| `headless` | `true` | Whether the testing should be done in a headless browser |
| `outputDir` | None | The directory name where the result file (with name matching the task file) should be saved |
| `retries` | `2` | The number of times the test should be retried on timeout or browser crash |
| `attemptTimeoutMs` | `120000` | The maximum number of milliseconds each attempt can take |

## YAML File Format

Your YAML file should have three sections:
| Section | Description |
| -- | -- |
| `initialProgram` | The initial program sketch to paste into the editor |
| `prompt` | The prompt to give to the model |
| `tests` | Tests to paste into the editor after the task is completed |

Make sure to use the `|` character in your YAML files to preserve whitespace in the program sketch/tests.