library(MyGraph)

test_that("adjacencyCorrect",{
  #given
  x<-matrix(c(0,1,0,0),nrow=2)
  rn <- c("a","b")
  rownames(x)=rn
  colnames(x)=rn
  y<-matrix(c(1,0,1,1),nrow=2)
  rownames(y)=rn
  colnames(y)=rn
  lista<-list(x,y)
  final<-matrix(c(1,1,1,1),nrow=2)
  rownames(final)=rn
  colnames(final)=rn
  
  #when
  fm<-adjacencyMatrix(lista)
  dim_m<-nrow(fm)
  
  
  #then
  expect_that(fm,is_a("matrix"))
  expect_equal(dim_m,2)
  expect_equal(final,fm)
  
  
  
})

test_that("adjacencyUncorrectwithMatrix and not list of Matrix",{
  #given
  uncorrect <- matrix(c(0,1,0,1), nrow=2)
  #when
  tryCatch({adjacencyMatrix(uncorrect)},error=function(e){
                                                    #then
                                                    expect_equal("Not a matrix.", e$message) 
                                                    print("input must be a list of matrix")
                                                    })
})

test_that("adjacency Uncorrect with list of Matrix ",{
  #given
  uncorrect2 <- matrix(c(NA,NA,NA,NA), nrow=2)
  l<-list(uncorrect2)
  #when
  tryCatch({adjacencyMatrix(l)},error=function(e){
    #then
    expect_equal("Not compatible with STRSXP: [type=NULL].", e$message) 
    print("matrix in list must not be NULL this means that the graph is not connected")
    })
})


test_that("adjacency Uncorrect with list of Matrix without rownames and colnames",{
  #given
  uncorrect3 <- matrix(c(0,1,0,1),nrow=2)
  l<-list(uncorrect3)
  #when
  tryCatch({adjacencyMatrix(l)},error=function(e){
    #then
    expect_equal("Not compatible with STRSXP: [type=NULL].",e$message)
    print("matrix in list had to be set row and col names")
    })
})
