library(MyGraph)

test_that("listoflist to matrix correct",{
  #given
  l1 <- list(10,"A","B")
  l2 <- list(20,"B","A")
  
  l <- list(l1,l2)
  
  rn<- c("A","B")
  names(rn)=rn
  
  m<-matrix(c(0,0,0,0),nrow=2)
  rownames(m)<-rn
  colnames(m)<-rn
  
  final <- matrix(c(0,20,10,0),nrow=2)
  rownames(final)<-rn
  colnames(final)<-rn
  
  #when
  fm <- weight(l,m)
  dim_m<-nrow(fm)
  
  #then
  expect_that(fm,is_a("matrix"))
  expect_equal(dim_m,2)
  expect_equal(final,fm)
})

test_that("listoflist to matrix with list element in list NULL",{
  #given
  l1 <- list(NA)
  l2 <- list(NA)
  l=list(l1,l2)
  rn<- c("A","B")
  names(rn)=rn
  m<-matrix(c(0,0,0,0),nrow=2)
  rownames(m)<-rn
  colnames(m)<-rn
  
  #when
  tryCatch({weight(l,m)},error=function(e){
    #then
    expect_equal("Index out of bounds: [index=1; extent=1].", e$message) 
    print("list in list must not be NULL these means that the graph is not connected")
  })
})

test_that("names of rownames not setted ",{
  #given
  l1 <- list(10,"A","B")
  l2 <- list(20,"B","A")
  l<- list(l1,l2)
  rn<- c("A","B")
  m<-matrix(c(0,0,0,0),nrow=2)
  rownames(m)<-rn
  colnames(m)<-rn
  #when
  tryCatch({weight(l,m)},error=function(e){
    expect_equal("'names' attribute is null", e$message) 
    print("names attribute of rownames must be setted with rownames -> names(rn)=rn or you can't find index in the matrix")
    })
  })


test_that("names of rownames attribute not setted ",{
  #given
  l1 <- list(10,"A","B")
  l2 <- list(20,"B","A")
  l<- list(l1,l2)
  rn<- c()
  names(rn)<-rn
  m<-matrix(c(0,0,0,0),nrow=2)
  #when
  tryCatch({weight(l,m)},error=function(e){
    expect_equal("no name 'A' found", e$message) 
    print("get the rowname from the adjacency matrix or you can't find index in the matrix")
  })
})

test_that("list input set as NULL ",{
  #given
  l<-list(NA)
  rn<- c("A","B")
  names(rn)=rn
  m<-matrix(c(0,0,0,0),nrow=2)
  rownames(m)<-rn
  colnames(m)<-rn
  #when
  tryCatch({weight(l,m)},error=function(e){
    expect_equal("Index out of bounds: [index=1; extent=1].", e$message)
    print("input list must not be NULL this means that graph is not connected")
  })
})
  
