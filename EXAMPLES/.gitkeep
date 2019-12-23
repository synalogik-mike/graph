library("aRangodb")
library("MyGraph")

#server connection and db selection

arangoConnection <- arango_connection("dev.kode-datacenter.net","41000","matteo","1234")
mydb <- arangoConnection %>% arango_database("prova")

#create the collections
persons <- mydb %>% arango_collection("person",createOnFail=TRUE)
city <- mydb %>% arango_collection("city",createOnFail=TRUE)
weather <- mydb %>% arango_collection("weather",createOnFail=TRUE)
meteo <- mydb %>% arango_collection("meteo",createOnFail=TRUE)


#insert document into collections:
#city %>%
# document_insert (key="") %>%
# document_set(capital=FALSE) %>%
# collection_update()

#extract all the documents from the collections
all.cities <- city %>% all_documents()
all.person <- persons %>% all_documents()
all.weather <- weather %>% all_documents()

#create the graphs
residenceGraph <- mydb %>% arango_graph("residence",createOnFail = TRUE)
distanceGraph <- mydb %>% arango_graph("distance",createOnFail = TRUE)

#add the definitions of possible edges that the graph can store

#distanceGraph %>%
#define_edge("city","distance_from","city")

#create edge collections :
#livesIncollection <- mydb %>% arango_collection("lives_in"
#eprevistoCollection <- mydb %>% arango_collection("e_previsto")
#hadweathercollection <- mydb %>% arango_collection("had_weather")
#lovesCollection <- mydb %>% arangoCollection("loves")
#distancefromCollection <-  mydb %>% arango_collection("distance_from")

#add edges

#distanceGraph <- distanceGraph %>%
#  add_edges("distance_from" %owns% edge (all.cities $LosAngeles %->% all.cities $Miami, distance=4250))


#create the traversal of all the graph
all.graph <- distanceGraph%>%
  all_graph()

#create the adjacency list of the graph
adjacency <- all.graph$getAdjacencyTensor()

matrix<-adjacencyMatrix(adjacency)
View(matrix)

pesato<- mydb %>% aql("FOR c IN city
                        FOR v,e IN OUTBOUND c._id
                        GRAPH 'distance'
                        OPTIONS {bfs: true, uniqueEdges : 'path', uniqueVertices : 'path'} FILTER e.distance != null RETURN {dist:e.distance, from:e._from, to:e._to }")
peso<- pesato()
MatrixPesata <- weight(peso,matrix)
View(MatrixPesata)

warshall<-floydWarshall(MatrixPesata)
View(warshall)

s<-"city/Pontedera"
dijkstraMatrix<-dijkstra(MatrixPesata,s)
View(dijkstraMatrix)

componentiConnesse<-component(matrix)
View(componentiConnesse)

kruskalStrongAndNotStrong<-Kruskal(MatrixPesata,matrix)
View(kruskalStrongAndNotStrong)

primStrongAndNotStrong<-prim(MatrixPesata,matrix)
View(primStrongAndNotStrong)

pagerank<-pageRank(matrix)
View(pagerank)

bellmanFORD<-BellmanFord(MatrixPesata,s)
View(bellmanFORD)

boruvka<-Boruvka(MatrixPesata,matrix)
View(boruvka)
