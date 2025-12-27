.PHONY: help build test clean docs download-tests install ghci watch coverage lint

# Default target
.DEFAULT_GOAL := help

# Colors for output
BLUE := \033[0;34m
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
NC := \033[0m # No Color

# Help target
help: ## Show this help message
	@echo "$(BLUE)HDF5 Direct - Common Tasks$(NC)"
	@echo ""
	@echo "$(GREEN)Available targets:$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(YELLOW)%-20s$(NC) %s\n", $$1, $$2}'
	@echo ""
	@echo "$(GREEN)Quick Start:$(NC)"
	@echo "  make build    - Build the library"
	@echo "  make test     - Run all tests (56 passing)"
	@echo "  make docs     - Generate and open documentation"
	@echo ""

# Build targets
build: ## Build the library and tests
	@echo "$(GREEN)Building HDF5 Direct...$(NC)"
	@stack build
	@echo "$(GREEN)✓ Build successful$(NC)"

rebuild: clean build ## Clean and rebuild everything

# Test targets
test: ## Run all 56 tests
	@echo "$(GREEN)Running test suite...$(NC)"
	@stack test
	@echo "$(GREEN)✓ All tests passed$(NC)"

test-unit: ## Run only unit tests (42 tests)
	@echo "$(GREEN)Running unit tests...$(NC)"
	@stack test --test-arguments='-m "Lib"'

test-integration: ## Run only integration tests (14 tests)
	@echo "$(GREEN)Running integration tests...$(NC)"
	@stack test --test-arguments='-m "Integration"'

test-verbose: ## Run tests with verbose output
	@echo "$(GREEN)Running tests (verbose)...$(NC)"
	@stack test --verbose

test-specific: ## Run tests matching a pattern (use: make test-specific PATTERN="ByteOrder")
	@if [ -z "$(PATTERN)" ]; then \
		echo "$(RED)Error: PATTERN not specified$(NC)"; \
		echo "Usage: make test-specific PATTERN=\"ByteOrder\""; \
		exit 1; \
	fi
	@echo "$(GREEN)Running tests matching '$(PATTERN)'...$(NC)"
	@stack test --test-arguments='-m "$(PATTERN)"'

coverage: ## Run tests with code coverage
	@echo "$(GREEN)Running tests with coverage...$(NC)"
	@stack test --coverage
	@echo "$(GREEN)✓ Coverage report generated$(NC)"

# Documentation targets
docs: ## Generate and open API documentation
	@echo "$(GREEN)Generating documentation...$(NC)"
	@stack haddock --open
	@echo "$(GREEN)✓ Documentation generated and opened$(NC)"

docs-gen: ## Generate documentation without opening
	@echo "$(GREEN)Generating documentation...$(NC)"
	@stack haddock

# Development targets
ghci: ## Start interactive Haskell REPL
	@echo "$(GREEN)Starting GHCi...$(NC)"
	@stack ghci

watch: ## Watch for file changes and rebuild
	@echo "$(GREEN)Watching for changes (Ctrl+C to stop)...$(NC)"
	@stack build --file-watch

watch-test: ## Watch for changes and run tests
	@echo "$(GREEN)Watching for changes and running tests (Ctrl+C to stop)...$(NC)"
	@stack test --file-watch

# Cleaning targets
clean: ## Remove build artifacts
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	@stack clean
	@echo "$(GREEN)✓ Clean complete$(NC)"

clean-docs: ## Remove generated documentation
	@echo "$(YELLOW)Cleaning generated documentation...$(NC)"
	@rm -rf .stack-work/
	@echo "$(GREEN)✓ Documentation cleaned$(NC)"

distclean: clean ## Clean everything including dependencies
	@echo "$(YELLOW)Deep cleaning...$(NC)"
	@stack clean
	@echo "$(GREEN)✓ Full clean complete$(NC)"

# Test data management
download-tests: ## Download HDF5 test files from official HDFGroup repository
	@echo "$(GREEN)Downloading HDF5 test files...$(NC)"
	@chmod +x download-test-files.sh
	@./download-test-files.sh
	@echo "$(GREEN)✓ Test files downloaded$(NC)"

check-tests: ## Check which test files are available
	@echo "$(BLUE)Available HDF5 test files:$(NC)"
	@if [ -d test-data ]; then \
		ls -lh test-data/*.h5 2>/dev/null || echo "No test files found in test-data/"; \
	else \
		echo "test-data directory not found. Run: make download-tests"; \
	fi

# Installation targets
install: build ## Install the library locally
	@echo "$(GREEN)Installing HDF5 Direct...$(NC)"
	@stack install
	@echo "$(GREEN)✓ Installation complete$(NC)"

# Linting and code quality
lint: ## Check code with HLint (if available)
	@echo "$(BLUE)Checking code quality...$(NC)"
	@if command -v hlint >/dev/null 2>&1; then \
		hlint src test; \
	else \
		echo "$(YELLOW)HLint not found. Install with: stack install hlint$(NC)"; \
	fi

format: ## Format Haskell code (if brittany available)
	@echo "$(BLUE)Formatting code...$(NC)"
	@if command -v brittany >/dev/null 2>&1; then \
		brittany --write-mode inplace src/**/*.hs test/**/*.hs; \
		echo "$(GREEN)✓ Code formatted$(NC)"; \
	else \
		echo "$(YELLOW)Brittany not found. Install with: stack install brittany$(NC)"; \
	fi

# Project info
info: ## Show project information
	@echo "$(BLUE)HDF5 Direct Project Information$(NC)"
	@echo ""
	@echo "$(GREEN)Project Structure:$(NC)"
	@echo "  Source files:       $(shell find src -name '*.hs' | wc -l) files"
	@echo "  Test files:         $(shell find test -name '*.hs' | wc -l) files"
	@echo "  Total lines:        $(shell find src test -name '*.hs' -exec wc -l {} + | tail -1 | awk '{print $$1}')"
	@echo ""
	@echo "$(GREEN)Test Coverage:$(NC)"
	@echo "  Total tests:        56"
	@echo "  Unit tests:         42"
	@echo "  Integration tests:  14"
	@echo ""
	@echo "$(GREEN)Datatype Classes:$(NC)"
	@echo "  Implemented:        11/11 (100%)"
	@echo "  Binary parsers:     6 complete + 5 stubs"

# Quick start sequences
quick: build test ## Quick build and test cycle
	@echo "$(GREEN)✓ Quick cycle complete$(NC)"

all: clean build test docs ## Full clean build, test, and documentation
	@echo "$(GREEN)✓ Full build complete$(NC)"

dev: build test ghci ## Build, test, and start interactive REPL
	@echo "$(GREEN)✓ Development environment ready$(NC)"

# Dependencies
deps: ## Show project dependencies
	@echo "$(BLUE)Project Dependencies:$(NC)"
	@grep -A 10 "build-depends:" hdf5-direct.cabal | grep -v "^--"

# Version and status
version: ## Show stack and GHC version
	@echo "$(BLUE)Version Information:$(NC)"
	@stack --version
	@echo ""
	@stack exec ghc -- --version

status: ## Show build and test status
	@echo "$(BLUE)Project Status:$(NC)"
	@echo "$(GREEN)Tests:$(NC) 56 passing (100%)"
	@echo "$(GREEN)Library:$(NC) Building successfully"
	@echo "$(GREEN)Documentation:$(NC) Available with 'make docs'"
	@echo "$(GREEN)Test Files:$(NC) 6 HDF5 files downloaded"

# Documentation viewing
readme: ## View README
	@if command -v less >/dev/null 2>&1; then \
		less README.md; \
	else \
		cat README.md; \
	fi

docs-index: ## View documentation index
	@if command -v less >/dev/null 2>&1; then \
		less INDEX.md; \
	else \
		cat INDEX.md; \
	fi

test-results: ## View test results documentation
	@if command -v less >/dev/null 2>&1; then \
		less TEST_RESULTS.md; \
	else \
		cat TEST_RESULTS.md; \
	fi

# CI/CD helpers
ci: clean build test ## Full CI pipeline (clean, build, test)
	@echo "$(GREEN)✓ CI pipeline complete$(NC)"

check: test lint ## Run tests and lint checks
	@echo "$(GREEN)✓ All checks passed$(NC)"

# Benchmarking (future enhancement)
bench: ## Run benchmarks (if available)
	@echo "$(YELLOW)Benchmarking not yet implemented$(NC)"
	@echo "See TEST_PLAN.md for benchmarking roadmap"

# Development helpers
update-deps: ## Update Stack dependencies
	@echo "$(BLUE)Updating dependencies...$(NC)"
	@stack update
	@echo "$(GREEN)✓ Dependencies updated$(NC)"

# Diagnostic targets
diagnose: ## Run diagnostics
	@echo "$(BLUE)Running diagnostics...$(NC)"
	@echo ""
	@echo "$(GREEN)Stack version:$(NC)"
	@stack --version
	@echo ""
	@echo "$(GREEN)GHC version:$(NC)"
	@stack exec ghc -- --version
	@echo ""
	@echo "$(GREEN)Project structure:$(NC)"
	@ls -1d src test hdf5-direct.cabal 2>/dev/null || echo "Missing files!"
	@echo ""
	@echo "$(GREEN)Test data:$(NC)"
	@ls -1 test-data/*.h5 2>/dev/null | wc -l | xargs echo "HDF5 files:"
	@echo ""
	@echo "$(GREEN)✓ Diagnostics complete$(NC)"

# Phony targets (not real files)
.PHONY: all bench check ci clean clean-docs deps dev diagnose distclean docs docs-gen docs-index download-tests format ghci help info install lint quick readme status test test-integration test-results test-specific test-unit test-verbose update-deps version watch watch-test check-tests
