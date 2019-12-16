\name{weigth}
\alias{weigthMatrix}
\docType{package}
\title{
Create adjacency matrix of a weighted graph 
}
\description{
Return a NumericMatrix with the value that is on the edge between two nodes 
}
\usage{
weight(List query, NumericMatrix adjacencyMatrix)

where List query is a list of list elements, the query MUST return the value on the edge and FROM, TO parameters
}
\examples{
\dontrun{
library("aRangodb")
library(Rcpp)
library("MyGraph")

#db connection and graph creation 
#create your own adjacency matrix with adjacencyMatrix() if you're in doubt ?adjacencyMatrix() will help you

input<-getAdjacencyTensor()
am<-adjacencyMatrix(input)

#query MUST return values on the edge and FROM and TO parameters

weigthMatrix<-weigth(query,am)
}
}