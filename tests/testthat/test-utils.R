# Tests for utility functions

test_that("JsonToTibble validates function signature correctly", {
  expect_true(exists("JsonToTibble"))
  expect_equal(length(formals(JsonToTibble)), 1)
  expect_equal(names(formals(JsonToTibble)), "x")
})

test_that("JsonToTibble handles valid JSON input", {
  # Test with simple JSON that includes a date field
  simple_json <- '[{"date": 1609459200, "value": 100}]'

  expect_no_error({
    result <- JsonToTibble(simple_json)
    expect_s3_class(result, "tbl_df")
    expect_true("date" %in% names(result))
    expect_true("value" %in% names(result))

    # Check that date conversion worked
    expect_s3_class(result$date, "Date")
    expect_equal(result$date[1], as.Date("2021-01-01"))
  })
})

test_that("JsonToTibble handles JSON without date field", {
  # Test with JSON that doesn't have a date field
  json_no_date <- '[{"name": "test", "value": 123}]'

  expect_no_error({
    result <- JsonToTibble(json_no_date)
    expect_s3_class(result, "tbl_df")
    expect_true("name" %in% names(result))
    expect_true("value" %in% names(result))
  })
})

test_that("JsonToTibble handles empty JSON array", {
  empty_json <- "[]"

  expect_no_error({
    result <- JsonToTibble(empty_json)
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
  })
})

test_that("JsonToTibble handles complex nested JSON", {
  # Test with more complex JSON structure
  complex_json <- '[{
    "date": 1609459200,
    "protocol": "Uniswap",
    "tvl": 1000000,
    "chains": ["Ethereum", "Polygon"]
  }]'

  expect_no_error({
    result <- JsonToTibble(complex_json)
    expect_s3_class(result, "tbl_df")
    expect_true("date" %in% names(result))
    expect_s3_class(result$date, "Date")
  })
})

test_that("JsonToTibble handles invalid JSON gracefully", {
  invalid_json <- '{"invalid": json}'

  expect_error({
    JsonToTibble(invalid_json)
  })
})

test_that("JsonToTibble handles date conversion edge cases", {
  # Test with different date formats
  json_with_dates <- '[
    {"date": 1609459200, "name": "entry1"},
    {"date": 1609545600, "name": "entry2"}
  ]'

  expect_no_error({
    result <- JsonToTibble(json_with_dates)
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 2)
    expect_s3_class(result$date, "Date")

    # Check that dates are properly converted
    expect_equal(result$date[1], as.Date("2021-01-01"))
    expect_equal(result$date[2], as.Date("2021-01-02"))
  })
})

test_that("JsonToTibble uses correct date conversion parameters", {
  # Test that the function uses the correct origin and timezone
  json_with_timestamp <- '[{"date": 0, "value": "epoch"}]'

  expect_no_error({
    result <- JsonToTibble(json_with_timestamp)
    expect_s3_class(result, "tbl_df")
    expect_s3_class(result$date, "Date")

    # Should convert Unix epoch (0) to 1970-01-01
    expect_equal(result$date[1], as.Date("1970-01-01"))
  })
})

test_that("JsonToTibble handles missing date values", {
  # Test with some missing date values
  json_missing_dates <- '[
    {"date": 1609459200, "value": 100},
    {"value": 200},
    {"date": null, "value": 300}
  ]'

  expect_no_error({
    result <- JsonToTibble(json_missing_dates)
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 3)

    # First row should have valid date
    expect_false(is.na(result$date[1]))
    expect_equal(result$date[1], as.Date("2021-01-01"))

    # Rows with missing dates should have NA
    if (nrow(result) > 1) {
      expect_true(any(is.na(result$date[2:3])))
    }
  })
})

test_that("JsonToTibble handles string date values", {
  # Test that the function handles string dates that can be converted to numeric
  json_string_dates <- '[
    {"date": "1609459200", "value": 100}
  ]'

  expect_no_error({
    result <- JsonToTibble(json_string_dates)
    expect_s3_class(result, "tbl_df")
    expect_s3_class(result$date, "Date")
    expect_equal(result$date[1], as.Date("2021-01-01"))
  })
})

test_that("JsonToTibble preserves other data types", {
  # Test that non-date columns preserve their data types
  mixed_json <- '[{
    "date": 1609459200,
    "string_col": "test",
    "numeric_col": 123.45,
    "logical_col": true,
    "integer_col": 999
  }]'

  expect_no_error({
    result <- JsonToTibble(mixed_json)
    expect_s3_class(result, "tbl_df")

    # Check data types are preserved (after tibble conversion)
    expect_type(result$string_col, "character")
    expect_type(result$numeric_col, "double")
    expect_type(result$logical_col, "logical")
  })
})