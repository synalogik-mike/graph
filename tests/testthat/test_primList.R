library(MyGraph)

test_that("Prim on strong connected graph correct",{
  #given 
  m1<- matrix(c(0,1000,1552,1000,0,2600,1552,2600,0),nrow=3)
  m2<- matrix(c(0,1,1,1,0,1,1,1,0),nrow=3)
  m2final<-matrix(c(0,1,1,1,0,0,1,0,0),nrow=3)
  rn=c("a","b","c")
  cn=c("a","b","c")
  rownames(m2)=rn
  colnames(m2)=cn
  rownames(m2final)=rn
  colnames(m2final)=cn
  ncomp=1
  finalresult<-list(2552,m2final)
  final<-list(finalresult)
  
  #when
  result<-prim(m1,m2)
  dim_result=length(result)
  
  #then
  expect_that(result,is_a("list"))
  expect_equal(dim_result,ncomp)
  expect_equal(result,final)
})

test_that("Prim on not strong connected graph correct",{
  #given
  
  m1<-matrix(c(0,1000,1552,0,0,0,1000,0,2600,0,0,0,1552,2600,0,0,0,0,0,0,0,0,0,4250,0,0,0,0,0,1400,0,0,0,4250,1400,0),nrow=6)
  m2<-matrix(c(0,1,1,0,0,0,1,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0),nrow=6)
  m1final <- matrix(c(0,1,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),nrow=6)
  m2final <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0),nrow=6)
  ncomp=2
  rn=c("a","b","c","d","e","f")
  cn=c("a","b","c","d","e","f")
  rownames(m1)=rn
  colnames(m1)=cn
  rownames(m2)=rn
  colnames(m2)=cn
  rownames(m1final)=rn
  colnames(m1final)=cn
  rownames(m2final)=rn
  colnames(m2final)=cn
  l1<-list(2552,m1final)
  l2<-list(5650,m2final)
  finalresult<-list(l1,l2)
  
  #when
  result<-prim(m1,m2)
  dim_result=length(result)
  
  #then
  expect_that(result,is_a("list"))
  expect_equal(dim_result,ncomp)
  expect_equal(result,finalresult)
  
})

test_that("Prim on graph with all node not connected",{
  #given
  m1<-matrix(c(0,0,0,0,0,0,0,0,0),nrow=3)
  m2<-matrix(c(0,0,0,0,0,0,0,0,0),nrow=3)
  ncomp<-0
  rn=c("a","b","c")
  cn=c("a","b","c")
  rownames(m2)=rn
  colnames(m2)=cn
  finalresult<-list(0,m2)
  final<-list(finalresult)
  
  #when
  result<-prim(m1,m2)
  dim_result=length(result)
  
  #then
  expect_that(result,is_a("list"))
  expect_equal(dim_result,1)
  expect_equal(result,final)
    
})

test_that("prim on graph with no named adjacency matrix", {
  #given
  m<-matrix(c(0,0,0,0,0,0,0,0,0),nrow=3)
  m1<-matrix(c(0,0,0,0,0,0,0,0,0),nrow=3)
  
  #when
  tryCatch({prim(m,m1)},error=function(e){
    #then
    expect_equal("Not compatible with STRSXP: [type=NULL].",e$message)
    print("Matrix rows and cols names must be set")
  })
})
