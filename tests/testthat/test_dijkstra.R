library(MyGraph)

test_that("Dijkstra Correct",{
  
  #given
  m<-matrix(c(0,1000,2000,2000,0,0,4000,1000,0),nrow=3)
  rn2<-c("A","B","C")
  names(rn2)<-rn2
  rownames(m)=rn2
  colnames(m)=rn2
  s<-"A"
  
  m1<- matrix(c(0,1000,2000,2000,0,0,3000,1000,0),nrow=3)
  rownames(m1)=rn2
  colnames(m1)=rn2
  m2<- matrix(c(0,0,0,0,0,0,1,0,0),nrow=3)
  rownames(m2)=rn2
  colnames(m2)=rn2
  final<-list(m1,m2)
  
  #when
  flm<-dijkstra(m,s)
  dimlist<- length(flm)
  dim_m<- nrow(flm[[1]])
  
  #then
  expect_that(flm,is_a("list"))
  expect_equal(dimlist,2)
  expect_equal(dim_m,3)
  expect_equal(flm[[1]],m1)
  expect_equal(flm[[2]],m2)
})

test_that("Uncorrect dijkstra with null matrix",{
  #given
  m<-matrix(c(0,1000,2000,2000,0,0,4000,1000,0),nrow=3)
  rn2<-c("A","B","C")
  names(rn2)<-rn2
  rownames(m)=rn2
  colnames(m)=rn2
  s<-"D"
  
  #when
  tryCatch({dijkstra(m,s)},error=function(e){
    #then
    expect_equal("no name 'D' found", e$message) 
    print("the start source that you give isn't a vertex of the graph")
  })
})

test_that("Uncorrect dijkstra matrix without rownames and colnames",{
  m<-matrix(c(0,1000,2000,2000,0,0,4000,1000,0),nrow=3)
  rn2<-c("A","B","C")
  names(rn2)<-rn2
  s<-"D"
  #when
  tryCatch({dijkstra(m,s)},error=function(e){
    #then
    expect_equal("'names' attribute is null", e$message) 
    print("Set the rownames and colnames of the matrix ")
  })
})

test_that("matrix of Unconnected Graph",{
  #given
  m<-matrix(c(0,0,0,0,0,0,0,0,0),nrow=3)
  rn2<-c("A","B","C")
  names(rn2)<-rn2
  rownames(m)=rn2
  colnames(m)=rn2
  s<-"A"
  m1<- matrix(c(0,0,0,Inf,0,0,Inf,0,0),nrow=3)
  rownames(m1)=rn2
  colnames(m1)=rn2
  m2<- matrix(c(0,0,0,-1,0,0,-1,0,0),nrow=3)
  rownames(m2)=rn2
  colnames(m2)=rn2
  
  #when
  flm<-dijkstra(m,s)
  dimlist<- length(flm)
  dim_m<- nrow(flm[[1]])
  
  #then
  expect_that(flm,is_a("list"))
  expect_equal(dimlist,2)
  expect_equal(dim_m,3)
  expect_equal(flm[[1]],m1)
  expect_equal(flm[[2]],m2)
  
  })
