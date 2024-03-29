library(SixSigma)
library(testthat)

#Control Charts

x <- ss.cc("mr", ss.data.pb1, CTQ = "pb.humidity")

testout <- ss.data.pb1
testout[31,] <- list(31,17)

y <- ss.cc("mr", testout, CTQ = "pb.humidity")

####################################################################
#Unit tests
#Author: Paula Martinez Vaquero
####################################################################

test_that("expected data type",{
  expect_type(x,"list")
})

test_that("length of x",{
  expect_length(x,5)
})

test_that("expected data type",{
  expect_type(y,"list")
})

test_that("length of y",{
  expect_length(y,5)
})

test_that("expected data type",{
  expect_type(x$LCL,"double")
})

test_that("expected data type",{
  expect_type(x$CL,"double")
})

test_that("expected data type",{
  expect_type(x$UCL,"double")
})

test_that("expected data type",{
  expect_type(x$phase,"character")
})

test_that("expected data type",{
  expect_type(x$out,"integer")
})

test_that("LCL, CL and UCL values from x", {
  expect_equal(x$LCL,0)
  expect_equal(round(x$CL,6),1.569483)
  expect_equal(round(x$UCL,6),5.126767)
  expect_equal(x$phase,"I")
  expect_equal(x$out,integer(0))
})

test_that("LCL, CL and UCL values from y", {
  expect_equal(y$LCL,0)
  expect_equal(round(y$CL,4),1.7286)
  expect_equal(round(y$UCL,6),5.646528)
  expect_equal(y$phase,"I")
  expect_equal(y$out,30)
})
