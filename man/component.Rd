\name{component}
\alias{component}
\docType{package}
\title{
Component of a graph
}
\description{
Return number of components of graph 
}
\usage{
component(NumericMatrix adjacencyMatrix)

where adjacencyMatrix is the adjacency matrix of the graph obtained with adjacencyMatrix. 
if in doubt -> ?adjacencyMatrix will help you
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

comp <- component(am)
}
}

