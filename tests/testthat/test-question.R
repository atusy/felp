test_that("pseudo postfix operators", {
  rmarkdown::render("test-question-postfix.Rmd", encoding = "UTF-8", quiet = TRUE)
  expect_identical_except_bytecode("test-question-postfix.md", "ans.md")
})
