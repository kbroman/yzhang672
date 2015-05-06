library(testthat)
library(SpeClust)

context("combind_fanwords")

test_that("combind_fanwords" , {
  
  A1 = spMatrix(nrow = 4,  ncol = 3,  
                i = c(1, 1, 2, 3, 3, 4),
                j = c(1, 2, 3, 1, 2, 3),
                x = c(1, 1, 1, 1, 1, 1))
  A2 = spMatrix(nrow = 5,  ncol = 4 , 
                i = c(1, 2, 2, 3, 4, 5, 5),
                j = c(1, 2, 3, 1, 2, 4, 4), 
                x = c(1, 1, 1, 1, 1, 1, 1))
  rownames(A1) = c("a", "b", "c", "d"); colnames(A1) = c("1", "2", "3")
  rownames(A2) = c("a", "b", "d", "e", "f"); colnames(A2) = c("1", "3", "4", "5")
  
  A = spMatrix(nrow = 6,  ncol = 5,  
               i = c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 6, 6),  
               j = c(1, 2, 1, 3, 3, 4, 1, 2, 3, 1, 3, 5, 5), 
               x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
  
  rownames(A) = c("a", "b", "c", "d", "e", "f"); colnames(A) = c("1", "2", "3", "4", "5")
  
  expect_equal(combind_fanwords(A1, A2),A)
  
  
})