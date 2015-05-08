library(testthat)
library(SpeClust)

context("drop")

test_that("drop", {
  
  expect_equal( drop("https://www.facebook.com/123456789/posts/12345678"), 
                "/123456789/posts/12345678")
  expect_equal( drop("https://www.facebook.com/123456789/posts/XXXXXXX"), 
                "/123456789/posts/XXXXXXX")
  
})