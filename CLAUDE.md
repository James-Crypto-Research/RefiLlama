





# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

RefiLlama is an R package that provides functions to fetch TVL (Total Value Locked) data from the DeFiLlama API. The package is designed to interact with various DeFiLlama endpoints including protocols, chains, pools, and coin data.

## Core Architecture

### Main API Function
- `CallDefillamaApi()` in `R/utils.R` is the core function that handles all API calls to DeFiLlama
- Supports multiple endpoint types: "api", "yields", "stablecoins", "coins", "abi-decoder", "bridges", "pro-api"
- Includes error handling and URL construction
- Now supports both legacy API endpoints and new Pro API endpoints

### Functional Organization
- `R/TVL.R` - TVL-related functions for protocols and chains
- `R/coins.R` - Cryptocurrency price and data functions
- `R/get_pool_info.R` - Yield farming pool data
- `R/volume.R` - DEX volume and options trading data
- `R/fees.R` - Protocol fees and revenue data
- `R/utils.R` - Core API utilities and JSON conversion helpers

### Key Functions by Category
**TVL Functions:**
- `GetListProtocol()` - Get protocol list with optional filtering
- `GetTvlHistoricalProtocol()` - Historical TVL per chain for a protocol
- `GetTvlHistoricalChain()` - Historical TVL for a specific chain
- `GetTvlCurrentAll()` - Current TVL across all chains
- `GetCurrentTvl()` - Current TVL for specific chain
- `GetTvlCurrentProtocol()` - Current TVL for specific protocol

**Coin Functions:**
- `GetCurrentCoinPrice()` - Current price for specified coin
- `GetEarliestCoinDate()` - Earliest available date for coin data
- `GetCoinPercentage()` - Percentage change over time periods
- `GetHistoricalCoinPrices()` - Historical prices at specific timestamp
- `GetCoinCharts()` - Price chart data with configurable intervals
- `GetBatchHistoricalCoinPrices()` - Batch historical prices via POST
- `GetBlockAtTimestamp()` - Block number at specific timestamp

**Volume Functions:**
- `GetDexVolumeOverview()` - Aggregated DEX volume across all protocols
- `GetDexVolumeByChain()` - DEX volume for specific blockchain
- `GetDexProtocolSummary()` - Volume data for specific DEX protocol
- `GetOptionsVolumeOverview()` - Aggregated options trading volumes
- `GetOptionsVolumeByChain()` - Options volume for specific chain
- `GetOptionsProtocolSummary()` - Options data for specific protocol

**Fees Functions:**
- `GetFeesOverview()` - Aggregated protocol fees and revenue
- `GetFeesByChain()` - Fees and revenue for specific blockchain
- `GetProtocolFeesSummary()` - Fees and revenue for specific protocol

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
# TVL functions
GetCurrentCoinPrice("bitcoin")
GetListProtocol(tvl_limit = 1E9)
GetTvlHistoricalChain("Polygon")

# New coin functions
GetHistoricalCoinPrices(c("bitcoin", "ethereum"), as.POSIXct("2024-01-01"))
GetCoinCharts(c("bitcoin"), period = "7d")
GetBlockAtTimestamp("ethereum", as.POSIXct("2024-01-01"))

# Volume functions
GetDexVolumeOverview()
GetDexProtocolSummary("uniswap")
GetOptionsVolumeOverview()

# Fees functions
GetFeesOverview()
GetProtocolFeesSummary("uniswap")
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
- Legacy Main API: `https://api.llama.fi`
- Legacy Yields: `https://yields.llama.fi`
- Legacy Coins: `https://coins.llama.fi`
- Legacy Others: stablecoins, abi-decoder, bridges
- **New Pro API**: `https://pro-api.llama.fi` (supports advanced features and higher rate limits)

Each function builds appropriate endpoint paths and query parameters for the specific data needed. The newer functions use the Pro API endpoints for better performance and additional features.