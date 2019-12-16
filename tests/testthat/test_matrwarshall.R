library("MyGraph")

test_that("warshall correct",{
  #given
  rn=c("a","b","c")
  m<-matrix(c(0,0,0,2000,0,0,4000,1000,0),nrow=3)
  rownames(m)=rn
  colnames(m)=rn
  final <- matrix(c(0,0,0,2000,0,0,3000,1000,0),nrow=3)
  rownames(final)=rn
  colnames(final)=rn
  #when
  fm <- floydWarshall(m)
  nr <- nrow(fm)
  
  #then
  
  expect_that(fm,is_a("matrix"))
  expect_equal(nr,nrow(m))
  expect_lt(fm[1,3],4000)
  expect_equal(final,fm)

})


  
  
  
  
