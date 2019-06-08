test_that("multiplication works", {
  rmarkdown::render("test-felp.Rmd", output_file =  "test-felp-ans.md", encoding = "UTF-8", quiet = TRUE)
  tst <- readLines("test-felp.md")
  ans <- readLines("test-felp-ans.md")
  expect_identical(
    tst[!grepl("^    ## <bytecode:", tst)],
    ans[!grepl("^    ## <bytecode:", ans)]
  )
})
