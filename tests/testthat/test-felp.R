test_that("felp works as if str + help", {
  rmarkdown::render("test-felp.Rmd", encoding = "UTF-8", quiet = TRUE)
  expect_identical_except_bytecode("test-felp.md", "ans.md")
})
