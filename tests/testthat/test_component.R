library(MyGraph)

test_that("component correct",{
  #given
  x<-matrix(c(0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0),nrow=6)
  #when 
  result<- component(x)
  
  #then
  expect_equal(result,2)
  
})

test_that("zero component",{
  #given
  x<-matrix(c(0,0,0,0),nrow=2)
  
  #when
  final <- component(x)
  
  #then
  expect_equal(final,0)
})
