\name{Prim}
\alias{prim}
\docType{package}
\title{
The Prim Algorithm 
}
\description{
The PRIM Algorithm is for found the prim's MST 
}
\usage{
prim(NumericMatrix adjacencyweigth, NumericMatrix adjacencyMatrix)

where NumericMatrix adjacencyWeigth is the matrix returned by weigth() function 
and NumericMatrix adjacencyMatrix is the matrix returned by adjacencyMatrix() 
function
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

Prim<-prim(weightMatrix,am)
}
}