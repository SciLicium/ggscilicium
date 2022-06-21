test_that("Titillium font family available", {
  expect_true("titillium" %in% sysfonts::font_families())
})
