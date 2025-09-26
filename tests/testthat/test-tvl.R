# Tests for TVL-related functions

test_that("GetListProtocol validates parameters correctly", {
  # Test that function exists and has correct parameters
  expect_true(exists("GetListProtocol"))
  expect_equal(length(formals(GetListProtocol)), 3)
  expect_equal(names(formals(GetListProtocol)), c("tvl_limit", "which_chain", "details"))

  # Test parameter defaults
  expect_equal(formals(GetListProtocol)$tvl_limit, 0)
  expect_equal(formals(GetListProtocol)$which_chain, NULL)
  expect_equal(formals(GetListProtocol)$details, FALSE)
})

test_that("GetListProtocol handles different parameter combinations", {
  # Test with network-independent parameter validation
  expect_no_error({
    tryCatch(
      GetListProtocol(tvl_limit = 1000000000),
      error = function(e) {
        # Allow network errors but check it's not a parameter error
        expect_false(grepl("argument", tolower(e$message)))
      }
    )
  })

  expect_no_error({
    tryCatch(
      GetListProtocol(tvl_limit = 0, which_chain = "Ethereum", details = TRUE),
      error = function(e) {
        # Allow network errors but check it's not a parameter error
        expect_false(grepl("argument", tolower(e$message)))
      }
    )
  })
})

test_that("GetTvlHistoricalProtocol validates parameters correctly", {
  expect_true(exists("GetTvlHistoricalProtocol"))
  expect_equal(length(formals(GetTvlHistoricalProtocol)), 2)
  expect_equal(names(formals(GetTvlHistoricalProtocol)), c("protocol", "category"))

  # Test defaults
  expect_equal(formals(GetTvlHistoricalProtocol)$protocol, "MakerDAO")
  expect_equal(formals(GetTvlHistoricalProtocol)$category, FALSE)
})

test_that("GetTvlHistoricalAll has correct signature", {
  expect_true(exists("GetTvlHistoricalAll"))
  expect_equal(length(formals(GetTvlHistoricalAll)), 0)
})

test_that("GetTvlHistoricalChain validates parameters correctly", {
  expect_true(exists("GetTvlHistoricalChain"))
  expect_equal(length(formals(GetTvlHistoricalChain)), 1)
  expect_equal(names(formals(GetTvlHistoricalChain)), "chain")

  # Test default
  expect_equal(formals(GetTvlHistoricalChain)$chain, "Ethereum")
})

test_that("GetCurrentTvl validates parameters correctly", {
  expect_true(exists("GetCurrentTvl"))
  expect_equal(length(formals(GetCurrentTvl)), 1)
  expect_equal(names(formals(GetCurrentTvl)), "chain")

  # Test default
  expect_equal(formals(GetCurrentTvl)$chain, "Ethereum")
})

test_that("GetTvlCurrentAll has correct signature", {
  expect_true(exists("GetTvlCurrentAll"))
  expect_equal(length(formals(GetTvlCurrentAll)), 0)
})

test_that("GetTvlCurrentProtocol validates parameters correctly", {
  expect_true(exists("GetTvlCurrentProtocol"))
  expect_equal(length(formals(GetTvlCurrentProtocol)), 1)
  expect_equal(names(formals(GetTvlCurrentProtocol)), "protocol")

  # Test default
  expect_equal(formals(GetTvlCurrentProtocol)$protocol, "Uniswap")
})

# Integration tests that would work with actual API (when network is available)
test_that("TVL functions return expected data structure when API is available", {
  skip_if_offline <- function() {
    skip_if_not(curl::has_internet(), "No internet connection")
  }

  # Test GetListProtocol returns a tibble
  skip_if_offline()
  expect_no_error({
    result <- tryCatch(
      GetListProtocol(tvl_limit = 1E10),  # High limit to get fewer results
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "tbl_df")
      expect_true("name" %in% names(result))
    }
  })

  # Test that functions can handle different chain parameters
  skip_if_offline()
  expect_no_error({
    result <- tryCatch(
      GetTvlHistoricalChain("Ethereum"),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "tbl_df")
      expect_true("date" %in% names(result))
    }
  })
})

# Test for proper error handling
test_that("TVL functions handle invalid inputs gracefully", {
  # These tests don't require network access
  expect_no_error({
    tryCatch(
      GetListProtocol(tvl_limit = -1),  # Negative limit should work (will just return all)
      error = function(e) {
        # Should not be a parameter validation error
        expect_false(grepl("invalid.*limit", tolower(e$message)))
      }
    )
  })
})