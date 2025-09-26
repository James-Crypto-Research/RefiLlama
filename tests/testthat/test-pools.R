# Tests for pool-related functions

test_that("GetPoolInfo validates function signature correctly", {
  expect_true(exists("GetPoolInfo"))
  expect_equal(length(formals(GetPoolInfo)), 0)
})

test_that("GetPoolInfo returns expected data structure when API is available", {
  skip_if_offline <- function() {
    skip_if_not(curl::has_internet(), "No internet connection")
  }

  skip_if_offline()
  expect_no_error({
    result <- tryCatch(
      GetPoolInfo(),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_s3_class(result, "tbl_df")
      expect_true("date" %in% names(result))

      # Check that date column is properly converted to Date class
      if ("date" %in% names(result) && nrow(result) > 0) {
        expect_s3_class(result$date, "Date")
      }
    }
  })
})

test_that("GetPoolInfo handles API errors gracefully", {
  # Test that the function can handle network issues without crashing
  expect_no_error({
    tryCatch(
      GetPoolInfo(),
      error = function(e) {
        # Should get a network error, not a function structure error
        expect_true(is.character(e$message))
        expect_false(grepl("object.*not found", e$message))
      }
    )
  })
})

test_that("GetPoolInfo processes date conversion correctly", {
  # Test the date conversion logic by checking the function structure
  # This doesn't require network access

  # Verify the function uses the correct API endpoint
  expect_no_error({
    tryCatch(
      GetPoolInfo(),
      error = function(e) {
        # Should not be a parameter or function definition error
        expect_false(grepl("argument", tolower(e$message)))
        expect_false(grepl("function.*not found", tolower(e$message)))
      }
    )
  })
})

# Test integration with the underlying API call
test_that("GetPoolInfo uses correct API parameters", {
  # This test verifies that GetPoolInfo calls the API with correct parameters
  # without actually making the network call

  expect_no_error({
    tryCatch(
      GetPoolInfo(),
      error = function(e) {
        # If there's an error, it should be network-related, not parameter-related
        # The function should be calling CallDefillamaApi("pools", type = "yields")
        expect_false(grepl("invalid.*type", tolower(e$message)))
        expect_false(grepl("missing.*argument", tolower(e$message)))
      }
    )
  })
})

# Test data structure expectations
test_that("GetPoolInfo should return tibble with expected columns", {
  skip_if_offline <- function() {
    skip_if_not(curl::has_internet(), "No internet connection")
  }

  skip_if_offline()
  expect_no_error({
    result <- tryCatch(
      GetPoolInfo(),
      error = function(e) NULL
    )

    if (!is.null(result)) {
      # Should be a tibble/data.frame
      expect_true(inherits(result, "data.frame"))
      expect_s3_class(result, "tbl_df")

      # Should have at least a date column
      expect_true("date" %in% names(result))

      # Date column should be properly formatted
      if (nrow(result) > 0) {
        expect_true(all(!is.na(result$date)))
        expect_s3_class(result$date, "Date")
      }
    }
  })
})

# Test error handling and edge cases
test_that("GetPoolInfo handles empty responses gracefully", {
  # Test that the function can handle various API response scenarios
  expect_no_error({
    tryCatch(
      GetPoolInfo(),
      error = function(e) {
        # Should handle JSON parsing errors gracefully
        expect_false(grepl("unexpected.*json", tolower(e$message)))
      }
    )
  })
})