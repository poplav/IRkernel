context('Inspector')

test_inspector <- Inspector$new()

test_that('should get code to inspect given code and cursor_pos on first line', {
    code <- "numeric_test"
    cursor_pos <- 12
    actual_code <- test_inspector$get_code_to_inspect(code, cursor_pos)
    expected_code <- "numeric_test"
    expect_equal(expected_code, actual_code)
})

test_that('should get code to inspect given code and cursor_pos on second line', {
    code <- "numeric_test <- 3\nnumeric_test"
    cursor_pos <- 30
    actual_code <- test_inspector$get_code_to_inspect(code, cursor_pos)
    expected_code <- "numeric_test"
    expect_equal(expected_code, actual_code)
})


test_that('should get code to inspect given code and cursor_pos with multiple spaces', {
    code <- "    numeric_test"
    cursor_pos <- 16
    actual_code <- test_inspector$get_code_to_inspect(code, cursor_pos)
    expected_code <- "numeric_test"
    expect_equal(expected_code, actual_code)
})