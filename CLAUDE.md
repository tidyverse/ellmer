# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ellmer is an R package that provides a unified interface to multiple Large Language Model (LLM) providers. It supports features like streaming outputs, tool/function calling, structured data extraction, and asynchronous processing.

## Key development commands

General advice:
* When running R from the console, always run it with `--quiet --vanilla`
* Always run `air format .` after generating code

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- Use `devtools::test(reporter = "check")` to run all tests
- Use `devtools::test(filter = "name", reporter = "check")` to run tests for `R/{name}.R`
- DO NOT USE `devtools::test_active_file()`
- All testing functions automatically load code; you don't need to.

- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.

### Documentation

- Run `devtools::document()` after changing any roxygen2 docs.
- Every user facing function should be exported and have roxygen2 documentation.
- Whenever you add a new documentation file, make sure to also add the topic name to `_pkgdown.yml`.
- Run `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.
- Use sentence case for all headings
- User facing changes should be briefly described in NEWS.md, following the tidyverse style guide (https://style.tidyverse.org/news.html).

### Code style

- Use newspaper style/high-level first function organisation. Main logic at the top and helper functions should come below.
- Don't define functions inside of functions unless they are very brief.
- Error messages should use `cli::cli_abort()` and follow the tidyverse style guide (https://style.tidyverse.org/errors.html)

## Architecture

### Core Components

**Chat Objects**: Central abstraction using R6 classes that maintain conversation state
- `Chat` - Main chat interface with provider-agnostic methods
- `Provider` - Abstract base class for different LLM providers
- `Turn` - Represents conversation turns with user/assistant messages
- `Content` - Handles different content types (text, images, PDFs, tool calls)

**Provider System**: Modular architecture supporting 15+ LLM providers
- Each provider in separate R file (`provider-*.R`)
- Common interface abstracts provider differences
- Authentication handled per-provider (API keys, OAuth, IAM)

**Tool System**: Function calling capabilities
- `tools-def.R` - Tool definition framework
- `tools-def-auto.R` - Automatic tool definition generation
- `chat-tools.R` - Tool execution and management

**Content Types**: Rich content support
- `content-image.R` - Image handling (files, URLs, plots)
- `content-pdf.R` - PDF document processing
- `content-replay.R` - Conversation replay functionality

**Parallel Processing**: Asynchronous and batch operations
- `parallel-chat.R` - Parallel chat execution
- `batch-chat.R` - Batch processing capabilities
- Uses `coro` package for async operations

### Key Design Patterns

**S7 Type System**: Uses S7 for structured data types in `types.R`
- Type definitions for tool parameters and structured outputs
- Runtime type checking and validation

**Standalone Imports**: Self-contained utility functions
- `import-standalone-*.R` files reduce dependencies
- Imported from other tidyverse packages

**Provider Plugin Architecture**: Each provider implements common interface
- `provider.R` defines base Provider class
- Provider-specific files extend base functionality
- Authentication and request handling per provider

## Key Files

### Core Implementation
- `R/chat.R` - Main Chat class implementation
- `R/provider.R` - Base Provider class and interface
- `R/types.R` - S7 type definitions for structured data
- `R/content.R` - Content handling framework

### Provider Implementations
- `R/provider-openai.R` - OpenAI/GPT integration
- `R/provider-anthropic.R` - Anthropic/Claude integration
- `R/provider-google.R` - Google Gemini integration
- Additional providers for AWS, Azure, Ollama, etc.

### Features
- `R/chat-structured.R` - Structured data extraction
- `R/chat-tools.R` - Tool/function calling
- `R/live.R` - Interactive chat interfaces
- `R/interpolate.R` - Template/prompt interpolation

### Testing and Quality
- `tests/testthat/` - Test suite with VCR cassettes
- `vignettes/` - Documentation and examples
- `.github/workflows/` - CI/CD with R CMD check

## S7

ellmer uses the S7 OOP system.

**Key concepts:**

- **Classes**: Define classes with `new_class()`, specifying a name and properties (typed data fields). Properties are accessed using `@` syntax
- **Generics and methods**: Create generic functions with `new_generic()` and register class-specific implementations using `method(generic, class) <- implementation`
- **Inheritance**: Classes can inherit from parent classes using the `parent` argument, enabling code reuse through method dispatch up the class hierarchy
- **Validation**: Properties are automatically type-checked based on their definitions

**Basic example:**

```r
# Define a class
Dog <- new_class("Dog", properties = list(
  name = class_character,
  age = class_numeric
))

# Create an instance
lola <- Dog(name = "Lola", age = 11)

# Access properties
lola@age  # 11

# Define generic and method
speak <- new_generic("speak", "x")
method(speak, Dog) <- function(x) "Woof"
speak(lola)  # "Woof"
```

## Development Notes

### Testing Strategy
- Uses VCR for HTTP request mocking to avoid live API calls
- Parallel test execution configured in DESCRIPTION
- Snapshot testing for output validation
- Separate test files for each major component

### Code Organization
- Collate field in DESCRIPTION defines file loading order
- Provider files follow consistent naming pattern
- Utility functions grouped by purpose (`utils-*.R`)
- Standalone imports minimize external dependencies

### Documentation
- Roxygen2 comments for all exported functions
- Vignettes demonstrate key use cases
- pkgdown site provides comprehensive documentation
- Examples use realistic but safe API interactions
