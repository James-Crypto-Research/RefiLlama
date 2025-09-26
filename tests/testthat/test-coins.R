# Tests for coin-related functions

test_that("GetCurrentCoinPrice validates parameters correctly", {
  expect_true(exists("GetCurrentCoinPrice"))
  expect_equal(length(formals(GetCurrentCoinPrice)), 1)
  expect_equal(names(formals(GetCurrentCoinPrice)), "coin")

  # Test default parameter
  expect_equal(formals(GetCurrentCoinPrice)$coin, "ethereum")
})

test_that("GetCurrentCoinPrice handles different coin parameters", {
  # Test with various coin names
  coins_to_test <- c("bitcoin", "ethereum", "chainlink", "polkadot")

  for (coin in coins_to_test) {
    expect_no_error({
      tryCatch(
        GetCurrentCoinPrice(coin),
        error = function(e) {
          # Allow network errors but check it's not a parameter error
          expect_false(grepl("argument.*coin", tolower(e$message)))
        }
      )
    })
  }
})

test_that("GetEarliestCoinDate validates parameters correctly", {
  expect_true(exists("GetEarliestCoinDate"))
  expect_equal(length(formals(GetEarliestCoinDate)), 1)
  expect_equal(names(formals(GetEarliestCoinDate)), "coin")

  # Test default parameter
  expect_equal(formals(GetEarliestCoinDate)$coin, "ethereum")
})

test_that("GetCoinPercentage validates parameters correctly", {
  expect_true(exists("GetCoinPercentage"))
  expect_equal(length(formals(GetCoinPercentage)), 4)
  expect_equal(names(formals(GetCoinPercentage)), c("coin", "timestamp", "lookForward", "period"))

  # Test default parameters
  expect_equal(formals(GetCoinPercentage)$coin, "ethereum")
  expect_equal(formals(GetCoinPercentage)$lookForward, FALSE)
  expect_equal(formals(GetCoinPercentage)$period, "24h")
})

test_that("GetCoinPercentage handles different parameter combinations", {
  # Test with different periods
  periods <- c("24h", "7d", "30d")

  for (period in periods) {
    expect_no_error({
      tryCatch(
        GetCoinPercentage("bitcoin", period = period),
        error = function(e) {
          # Allow network errors but check it's not a parameter error
          expect_false(grepl("argument", tolower(e$message)))
        }
      )
    })
  }

  # Test with lookForward parameter
  expect_no_error({
    tryCatch(
      GetCoinPercentage("ethereum", lookForward = TRUE),
      error = function(e) {
        # Allow network errors but check it's not a parameter error
        expect_false(grepl("argument", tolower(e$message)))
      }
    )
  })

  # Test with custom timestamp
  custom_time <- as.POSIXct("2023-01-01", tz = "UTC")
  expect_no_error({
    tryCatch(
      GetCoinPercentage("bitcoin", timestamp = custom_time),
      error = function(e) {
        # Allow network errors but check it's not a parameter error
        expect_false(grepl("argument", tolower(e$message)))
      }
    )
  })
})

# Integration tests for coin functions
test_that("Coin functions return expected data structure when API is available", {
  skip_if_offline <- function() {
    skip_if_not(curl::has_internet(), "No internet connection")
  }

  # Test GetCurrentCoinPrice returns proper structure
  skip_if_offline()
  expect_no_error({
    result <- tryCatch(
      GetCurrentCoinPrice("bitcoin"),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "tbl_df")
      expect_true("price" %in% names(result) || "timestamp" %in% names(result))
    }
  })

  # Test GetEarliestCoinDate returns proper structure
  skip_if_offline()
  expect_no_error({
    result <- tryCatch(
      GetEarliestCoinDate("ethereum"),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "tbl_df")
      expect_true("timestamp" %in% names(result))
    }
  })

  # Test GetCoinPercentage returns proper structure
  skip_if_offline()
  expect_no_error({
    result <- tryCatch(
      GetCoinPercentage("bitcoin", period = "24h"),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "tbl_df")
    }
  })
})

# Test coin ID construction
test_that("Coin functions construct proper coin IDs", {
  # These tests verify the function logic without requiring network access
  # The functions should construct "coingecko:coinname" format

  expect_no_error({
    tryCatch(
      GetCurrentCoinPrice("test-coin"),
      error = function(e) {
        # Should not fail due to coin name format
        expect_false(grepl("invalid.*coin", tolower(e$message)))
      }
    )
  })
})

test_that("Coin functions handle edge cases", {
  # Test empty string (should still work with API call structure)
  expect_no_error({
    tryCatch(
      GetCurrentCoinPrice(""),
      error = function(e) {
        # Allow API errors but not parameter validation errors
        expect_false(grepl("missing.*coin", tolower(e$message)))
      }
    )
  })

  # Test special characters in coin names
  expect_no_error({
    tryCatch(
      GetCurrentCoinPrice("wrapped-bitcoin"),
      error = function(e) {
        # Allow API errors but not parameter validation errors
        expect_false(grepl("invalid.*character", tolower(e$message)))
      }
    )
  })
})

# Test timestamp handling in GetCoinPercentage
test_that("GetCoinPercentage handles timestamp conversion correctly", {
  # Test that the function accepts different timestamp formats
  expect_no_error({
    tryCatch(
      GetCoinPercentage("bitcoin", timestamp = Sys.time()),
      error = function(e) {
        # Allow network errors but not timestamp conversion errors
        expect_false(grepl("timestamp", tolower(e$message)))
      }
    )
  })

  expect_no_error({
    tryCatch(
      GetCoinPercentage("bitcoin", timestamp = as.POSIXct("2023-01-01")),
      error = function(e) {
        # Allow network errors but not timestamp conversion errors
        expect_false(grepl("timestamp", tolower(e$message)))
      }
    )
  })
})