\name{adjacencyMatrix}
\alias{adjacencyMatrix}
\docType{package}
\title{
Create adjacency matrix from graph traversal
}
\description{
adjacencyMatrix(List input)
}
\usage{
adjacencyMatrix(List input) where List input is a list returned by getAdjacencyTensor function 	
}
\examples{
\dontrun{
library("aRangodb")
library(Rcpp)
#db connection and graph creation 
#create your own adjacency matrix with adjacencyMatrix() if you're in doubt 
?adjacencyMatrix() will help you

input<-getAdjacencyTensor()
am<-adjacencyMatrix(input)
}
}
