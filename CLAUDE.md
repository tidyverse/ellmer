# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ellmer is an R package that makes it easy to use large language models (LLMs) from R. It supports a wide variety of LLM providers and implements features including streaming outputs, tool/function calling, structured data extraction, and asynchronous calls.

## Development Commands

### Building and Checking

In an R session:

```r
# Install development version from the current directory
devtools::install()

# Load the package
library(ellmer)

# Run checks (equivalent to R CMD check)
devtools::check()

# Run tests
devtools::test()

# Run a specific test file
devtools::test(filter = "provider-openai")

# Generate documentation
devtools::document()
```

### Styling code

Run `air format .` in console

### Testing

The package uses testthat for testing. Tests are organized by functionality in files named `test-*.R` in the `tests/testthat/` directory.

```r
# Run all tests
devtools::test()

# Run a specific test file
devtools::test(filter = "chat")

# Run tests with a specific pattern
devtools::test(filter = "provider")
```

Note: Some tests require API keys for different LLM providers. These are stored as environment variables (e.g., `OPENAI_API_KEY`).

## Architecture

The package is built around several key S7 classes:

1. `Provider` - Base class for all LLM providers (OpenAI, Claude, etc.)
2. `Chat` - Main R6 class for chat interactions
3. `Content` - Class for managing different content types (text, images, etc.)
4. `Turn` - Represents a single exchange in a conversation
5. `Type` - For structured data extraction

The typical workflow:
1. Create a chat object with a specific provider (`chat_openai()`, `chat_anthropic()`, etc.)
2. Add system prompts and context
3. Interact through methods like `$chat()` or `live_console()`

### Provider System

Each LLM provider has its own file (`provider-*.R`) that implements its specific API requirements. They all inherit from the base `Provider` class in `provider.R`.

### Important Components

- `chat.R` - Core chat functionality
- `provider.R` - Base provider class
- `content.R` and related files - Content handling (text, images, PDFs)
- `tools-def.R` - Tool/function calling functionality
- `chat-structured.R` - Structured data extraction

### Parallel and Async

The package uses the coro package for handling asynchronous operations, which enables streaming and parallel API calls.
