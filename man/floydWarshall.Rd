\name{FloydWarshall}
\alias{floydWarshall}
\docType{package}
\title{
The Floyd Warshall Algorithm 
}
\description{
The Floyd Warshall Algorithm is for solving the All Pairs Shortest Path problem.
}
\usage{
floydWarshall(NumericMatrix adjacencyweigth)

where NumericMatrix adjacencyWeigth is the matrix returned by weigth () function 
}
\examples{
\dontrun{
library("aRangodb")
library(Rcpp)
library("MyGraph")

#db connection and graph creation 
#create your own adjacency matrix with adjacencyMatrix() if you're in doubt 
?adjacencyMatrix() will help you

input<-getAdjacencyTensor()
am<-adjacencyMatrix(input)

#query MUST return values on the edge and FROM and TO parameters

weigthMatrix<-weight(query,am)

warshall<-floydWarshall(weightMatrix)
}
}