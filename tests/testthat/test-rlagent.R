context("test-rlagent")

test_that("agent_initialize", {
  
  a <- Rrrl::RLAgent$new();
  testthat::expect_true(is.null(a$name));
  testthat::expect_true(!is.null(a$id));
  
  b <- Rrrl::RLAgent$new(name = "Player1");
  # testthat::expect_true(!is.null(b$name));
  # testthat::expect_true(!is.null(b$id));
})
