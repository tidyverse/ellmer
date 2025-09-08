# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ellmer is an R package that provides a unified interface to multiple Large Language Model (LLM) providers. It supports features like streaming outputs, tool/function calling, structured data extraction, and asynchronous processing.

## Development Commands

### Testing
- `R CMD check` - Full package check (used in CI)
- `testthat::test_check("ellmer")` - Run all tests via testthat
- `devtools::test()` - Run tests interactively during development
- Tests use VCR cassettes for HTTP mocking (located in `tests/testthat/_vcr/`)
- Test configuration includes parallel execution and specific test ordering

### Building and Documentation
- `devtools::document()` - Generate documentation from roxygen2 comments
- `pkgdown::build_site()` - Build package website
- `devtools::build()` - Build package tarball
- `devtools::install()` - Install package locally for development

### Package Structure
- Uses standard R package structure with DESCRIPTION, NAMESPACE, and man/ directories
- Source code organized in R/ directory with provider-specific files
- Vignettes in vignettes/ directory demonstrate key features
- Tests in tests/testthat/ with snapshot testing enabled

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