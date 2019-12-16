library(MyGraph)

test_that("pageRank on not strong connected graph",{
  #given 
  v1<-c( 0.167,0.500,0.000,0.000,0.000,0.000)
  v2<-c(0.000,0.000,0.000,0.000,0.333,0.333)
  comp<-2
  adj<-matrix(c(0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0),nrow=6)
  names(v1)<-c("a","b","c","d","e","f")
  names(v2)<-c("a","b","c","d","e","f")
  rownames(adj)<-c("a","b","c","d","e","f")
  colnames(adj)<-c("a","b","c","d","e","f")
  final<-list(v1,v2)
  
  #when
  result<-pageRank(adj)
  dim_result=length(result)
  
  #then
  expect_that(result,is_a("list"))
  expect_equal(dim_result,comp)
})
