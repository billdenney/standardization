test_that("match_NA", {
  expect_equal(
    match_NA(
      data1 = data.frame(col1=c(2,3,"NA","")),
      data2 = data.frame(col1=c(2,3,"NA",""))
    ),
    data.frame(col1_Output = 1L, col1_Input = 1L)
  )
})
