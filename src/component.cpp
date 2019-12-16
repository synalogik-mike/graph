#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
void dfs (int x, NumericMatrix g, NumericVector vis1){
    int n1=g.nrow(); //numero di nodi
    vis1(x)=1; //setto come visitato il nodo che ho passato come radice di dfs
    int j;
    for(j=0;j<n1;j++){
        if((vis1(j)==0)&(g(x,j)==1)){ //se non ho ancora visitato il nodo j e c'è un arco tra il nodo x (radice dfs) e j allora vuol dire che c'è un collegamento e quindi eseguo dfs su quel nodo.
            dfs(j,g,vis1);
        }
    }
}
// [[Rcpp::export]]
int component(NumericMatrix m){
    int cont=0;
    int n=m.nrow();
    NumericVector vis(n); //vettore dei nodi visitati
    NumericMatrix nonorientato(n,n);
    int i,j;
    for(i=0;i<n;i++){
           for(j=0;j<n;j++){
               nonorientato(i,j)=0; //inizializzo la matrice del grafo non orientato
           }
    }
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){
            if(m(i,j)==1){
                nonorientato(i,j)=1; //creo la matrice di adiacenza del grafo non orientato prendendo come esempio la matrice di adiacenza in input
                nonorientato(j,i)=1;
            }
        }
    }
    for(i=0;i<n;i++){
        vis(i)=0;
    }
    for(i=0;i<n;i++){ //per ogni nodo controllo se è stato già visitato
        if(vis(i)==0){ //se non è stato visitato vuol dire che ho una componente connessa da contare
            cont++;//con cont=1 il grafo è fortemente connesso
            dfs(i,nonorientato,vis); //richiamo la dfs sul nodo non visitato
        }
    }
    
    if(cont==n){
        cont=0;//se il numero di componenti è uguale al numero di nodi allora vuol dire che non ho eseguito dfs su nessun nodo, che il grafo non è collegato
    }
    return cont; //ritorno il numero di componenti connesse .
}
