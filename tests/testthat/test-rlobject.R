context("test-rlobject")

test_that("initialize works", {
  o <- Rrrl::RLObject$new();
  
  testthat::expect_true(!is.null(o$id));
})
