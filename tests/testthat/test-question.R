test_that("prefix operators", {
  rmarkdown::render("test-question-prefix.Rmd", encoding = "UTF-8", quiet = TRUE)
  expect_identical_except_bytecode("test-question-prefix.md", "ans.md")
})


test_that("pseudo postfix operators", {
  rmarkdown::render("test-question-postfix.Rmd", encoding = "UTF-8", quiet = TRUE)
  expect_identical_except_bytecode("test-question-postfix.md", "ans.md")
})
