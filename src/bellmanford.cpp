#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List BellmanFord (NumericMatrix graph,CharacterVector source){
    int n=graph.nrow();
    int i,j,u,v,flag=0;
    int k=0;
     
    List result;
    int contedge=0;
    CharacterVector rn;
    String s=as<String>(source);
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){
            if(graph(i,j)!=0){
                contedge++; //conto gli archi presenti nel grafo per crearmi la matrice che conterrà la partenza e la destinazione dell'arco
            }
        }
    }
    NumericMatrix edge(contedge,2);//matrice partenza -> destinazione archi
    for(i=0;i<n;i++){
          for(j=0;j<n;j++){
              if(graph(i,j)!=0){
                  edge(k,0)=i; //inizializzo la matrice contenente partenza e destinazione (inseriti come scritti) degli archi dell'albero
                  edge(k,1)=j;
                  k++;
              }
          }
      }
    NumericVector distance(n); //vettore costi distanze
    NumericVector parent(n); //vettore parenti
    for(i=0;i<n;i++){
        distance(i)=R_PosInf; //inizializzo vettore distance tutto a Inf
        parent(i)=-1; //inizializzo vettore parenti tutto a -1
    }
    rn=rownames(graph);//ottengo il vettore con tutti i nomi del grafo
    int is=rn.findName(s); //ricerco indice source
    distance(is)=0;//setto la distanza della radice alla radice a 0
    parent(is)=0;//setto parente della radice a 0
    for(i=0;i<contedge;i++){
        for(j=0;j<contedge;j++){
            u=edge(j,0);//prendo il nodo di partenza dell'arco
            v=edge(j,1);//prendo il nodo di destinazione dell'arco
            if(distance(v)>distance(u)+graph(u,v)){ //se la distanza dal nodo di partenza più l'arco che va dal nodo partenza al nodo destinazione è minore del valore della distanza del nodo di destinazione
                distance(v)=distance(u)+graph(u,v); // la distanza del nodo di destinazione prende il valore della distanza del nodo di partenza più il peso dell'arco dal nodo partenza al nodo arrivo
                parent(v)=u+1; //il parente del nodo destinazione è il nodo partenza
            }
        }
    }
    for(j=0;j<k;j++){
        u=edge(k,0);
        v=edge(k,1);
        if(distance(v)>distance(u)+graph(u,v)){
            flag=1; // se il grafo contiente cicli negativi ritorna 1 altrimenti ritorna 0
    
        }
    }
    //se il grafo non contiene cicli negativi ritorna i costi e i parenti e il flag settato a 0 che indica che il grafo non ha cicli negativi, altrimenti ritorna il vettore distance settato tutto a Inf eccetto il nodo di partenza e il vettore parent settato tutto a -1 eccetto il nodo di partenza, il tutto verrà inserito in lista compreso il flag.
   //se il flag è settato a 0 ma il vettore parent e distance contengono lo stesso Inf e -1 vuol dire che quei nodi non sono raggiunti dal nodo di partenza
    result=List::create(distance,parent,flag);
    return result;
}
