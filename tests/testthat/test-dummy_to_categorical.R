library(testthat)

test_that("dummy_to_categorical behaves correctly", {

  # Test 1: Converts valid one-hot encoded data correctly
  df <- data.frame(A = c(1, 0, 0),
                   B = c(0, 1, 0),
                   C = c(0, 0, 1))
  result <- dummy_to_categorical(df)
  expect_equal(as.character(result), c("A", "B", "C"))

  # Test 2: Handles logical input correctly
  df <- data.frame(A = c(TRUE, FALSE, FALSE),
                   B = c(FALSE, TRUE, FALSE),
                   C = c(FALSE, FALSE, TRUE))
  result <- dummy_to_categorical(df)
  expect_equal(as.character(result), c("A", "B", "C"))

  # Test 3: Assigns default when all values are 0
  df <- data.frame(A = c(1, 0, 0, 0),
                   B = c(0, 1, 0, 0),
                   C = c(0, 0, 1, 0))
  result <- dummy_to_categorical(df, default = "None")
  expect_equal(as.character(result), c("A", "B", "C", "None"))

  # Test 4: Errors when input contains non-logical, non-binary values
  df <- data.frame(A = c(1, 0, 0),
                   B = c(0, 2, 0),  # Invalid: 2 is not binary
                   C = c(0, 0, 1))
  expect_error(dummy_to_categorical(df),
               "All input variables must be logical \\(TRUE/FALSE\\) or binary numeric \\(0/1\\).")

  # Test 5: Errors when a row contains more than one TRUE/1
  df <- data.frame(A = c(1, 0, 1),
                   B = c(0, 1, 1),  # Invalid: Third row has two 1s
                   C = c(0, 0, 0))
  expect_error(dummy_to_categorical(df),
               "Each row must have at most one TRUE/1 value.")

  # Test 6: Works when levels are explicitly provided
  df <- data.frame(X = c(1, 0, 0),
                   Y = c(0, 1, 0),
                   Z = c(0, 0, 1))
  result <- dummy_to_categorical(df, levels = c("First", "Second", "Third"))
  expect_equal(as.character(result), c("First", "Second", "Third"))

  # Test 7: Errors when given an empty data frame
  df <- data.frame()
  expect_equal(dummy_to_categorical(df), factor())

})
