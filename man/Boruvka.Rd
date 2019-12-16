\name{Boruvka}
\alias{boruvka}
\docType{package}
\title{
The Boruvka algorithm 
}
\description{
Borůvka's algorithm is a greedy algorithm for finding a minimum spanning tree 
}
\usage{
Boruvka(NumericMatrix graph, NumericMatrix adjacency) 
Where NumericMatrix Graph is the matrix returned by weigth () function and NumericMatrix adjacency 
is the matrix returned by adjacencyMatrix() function.
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

boruvka <- Boruvka(weightMatrix,am)
}
}