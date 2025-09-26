# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

RefiLlama is an R package that provides functions to fetch TVL (Total Value Locked) data from the DeFiLlama API. The package is designed to interact with various DeFiLlama endpoints including protocols, chains, pools, and coin data.

## Core Architecture

### Main API Function
- `CallDefillamaApi()` in `R/utils.R` is the core function that handles all API calls to DeFiLlama
- Supports multiple endpoint types: "api", "yields", "stablecoins", "coins", "abi-decoder", "bridges"
- Includes error handling and URL construction

### Functional Organization
- `R/TVL.R` - TVL-related functions for protocols and chains
- `R/coins.R` - Cryptocurrency price and data functions
- `R/get_pool_info.R` - Yield farming pool data
- `R/utils.R` - Core API utilities and JSON conversion helpers

### Key Functions by Category
**TVL Functions:**
- `GetListProtocol()` - Get protocol list with optional filtering
- `GetTvlHistoricalProtocol()` - Historical TVL per chain for a protocol
- `GetTvlHistoricalChain()` - Historical TVL for a specific chain
- `GetTvlCurrentAll()` - Current TVL across all chains

**Coin Functions:**
- `GetCurrentCoinPrice()` - Current price for specified coin
- `GetEarliestCoinDate()` - Earliest available date for coin data
- `GetCoinPercentage()` - Percentage change over time periods

**Pool Functions:**
- `GetPoolInfo()` - Get yield farming pool data

## Development Commands

### Package Development
```r
# Build package
devtools::build()

# Install package
devtools::install()

# Check package
devtools::check()

# Load for development
devtools::load_all()

# Generate documentation
roxygen2::roxygenise()
```

### Testing
```r
# Run all tests
devtools::test()

# Run tests with coverage
covr::package_coverage()

# Run specific test file
testthat::test_file("tests/testthat/test-api.R")

# Test interactively
testthat::test_dir("tests/testthat")
```

### Testing Functions
Since there are no formal tests, test functions interactively:
```r
# Example tests
GetCurrentCoinPrice("bitcoin")
GetListProtocol(tvl_limit = 1E9)
GetTvlHistoricalChain("Polygon")
```

## Code Conventions

**Google R Style Compliance:**
- Function names use BigCamelCase (e.g., `GetCurrentCoinPrice()`)
- Explicit `return()` statements in all functions
- Explicit namespace qualification for base R functions (e.g., `base::as.Date()`)
- Proper spacing around operators and after commas
- Uses tidyverse conventions (dplyr, tibble, purrr)
- All functions return tibbles
- Date handling converts Unix timestamps to Date objects
- Error handling in API calls with informative messages
- Roxygen2 documentation for all exported functions
- Two-space indentation (R standard)

## Data Processing Patterns

- JSON responses converted to tibbles using `JsonToTibble()` helper function
- Unix timestamps converted to R Date objects consistently using `base::as.Date()`
- Wide format data for multi-chain protocols
- Dynamic column naming using rlang's `:=` operator
- Null checks and optional parameters for filtering

## API Endpoint Structure

The package constructs URLs to different DeFiLlama services:
- Main API: `https://api.llama.fi`
- Yields: `https://yields.llama.fi`
- Coins: `https://coins.llama.fi`
- Others: stablecoins, abi-decoder, bridges

Each function builds appropriate endpoint paths and query parameters for the specific data needed.