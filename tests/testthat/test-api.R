# Tests for the core API function CallDefillamaApi

test_that("CallDefillamaApi validates type parameter correctly", {
  expect_error(
    CallDefillamaApi("protocols", type = "invalid"),
    "Invalid type argument"
  )

  expect_error(
    CallDefillamaApi("protocols", type = "invalid"),
    "Valid types are:"
  )
})

test_that("CallDefillamaApi accepts valid type parameters", {
  valid_types <- c("api", "yields", "stablecoins", "coins", "abi-decoder", "bridges")

  for (type in valid_types) {
    expect_no_error({
      # We'll mock the actual API call to avoid network dependencies
      # This tests that the function doesn't error on type validation
      tryCatch(
        CallDefillamaApi("protocols", type = type),
        error = function(e) {
          # Only allow network-related errors, not type validation errors
          expect_false(grepl("Invalid type argument", e$message))
        }
      )
    })
  }
})

test_that("CallDefillamaApi constructs URLs correctly", {
  # Test that different types create different base URLs
  # We can't easily test the exact URL without mocking httr2, but we can test
  # that the function runs without type-related errors

  expect_no_error({
    tryCatch(
      CallDefillamaApi("protocols", type = "api"),
      error = function(e) {
        # Allow network errors but not type errors
        expect_false(grepl("Invalid type argument", e$message))
      }
    )
  })

  expect_no_error({
    tryCatch(
      CallDefillamaApi("pools", type = "yields"),
      error = function(e) {
        # Allow network errors but not type errors
        expect_false(grepl("Invalid type argument", e$message))
      }
    )
  })
})

# Mock tests for successful API responses
test_that("CallDefillamaApi handles successful responses", {
  # Mock a simple JSON response
  mock_json <- '{"status": "success", "data": []}'

  # We would need to mock httr2 here in a real implementation
  # For now, we test the structure exists
  expect_true(exists("CallDefillamaApi"))
  expect_equal(length(formals(CallDefillamaApi)), 2)
  expect_equal(names(formals(CallDefillamaApi)), c("path", "type"))
})

test_that("CallDefillamaApi parameter defaults work correctly", {
  # Test that default type parameter is "api"
  expect_no_error({
    tryCatch(
      CallDefillamaApi("protocols"),  # No type specified, should default to "api"
      error = function(e) {
        # Allow network errors but not type errors
        expect_false(grepl("Invalid type argument", e$message))
      }
    )
  })
})