test_that("prefix operators", {
  knitr::knit("test-question-prefix.Rmd", encoding = "UTF-8", quiet = TRUE)
  expect_identical_except_bytecode("test-question-prefix.md", "ans.md")
})


test_that("pseudo postfix operators", {
  knitr::knit("test-question-postfix.Rmd", encoding = "UTF-8", quiet = TRUE)
  expect_identical_except_bytecode("test-question-postfix.md", "ans.md")
})

test_that("infix operator", {
  # For better test, dummy S4 generic and method are required
  expect_error(dummy_e1?dummy_e2)
})
