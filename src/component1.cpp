#include <Rcpp.h>

void dfs1 (int x, Rcpp::NumericMatrix g, Rcpp::NumericVector vis1,int cont){
    int n1=g.nrow(); //numero di nodi
    vis1(x)=1; //setto come visitato il nodo che ho passato come radice di dfs
    int j;
    for(j=0;j<n1;j++){
        if((g(x,j)==1)&(vis1(j)==0)){ //se non ho ancora visitato il nodo j e c'è un arco tra il nodo x (radice dfs) e j allora vuol dire che c'è un collegamento e quindi eseguo dfs su quel nodo.
            g(x,j)=cont;
            g(j,x)=cont;
            dfs1(j,g,vis1,cont);
            }
        }
    }


int component1(Rcpp::NumericMatrix peso, Rcpp::NumericMatrix componenti){
    int cont=0;
    int n=peso.nrow();
    Rcpp::NumericVector vis(n); //vettore dei nodi visitati
    Rcpp::NumericMatrix nonorientato(n,n);
    int i,j;
    for(i=0;i<n;i++){
           for(j=0;j<n;j++){
               nonorientato(i,j)=0; //inizializzo la matrice del grafo non orientato
           }
    }
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){
            if(peso(i,j)>0){
                nonorientato(i,j)=peso(i,j); //creo la matrice di adiacenza del grafo non orientato prendendo come esempio la matrice di adiacenza in input
                nonorientato(j,i)=peso(i,j);
                componenti(j,i)=1;
            }
        }
    }
    for(i=0;i<n;i++){
        vis(i)=0;
    }
    
    for(i=0;i<n;i++){ //per ogni nodo controllo se è stato già visitato
        if(vis(i)==0){ //se non è stato visitato vuol dire che ho una componente connessa da contare
            cont++;//con cont=1 il grafo è fortemente connesso
            dfs1(i,componenti,vis,cont); //richiamo la dfs sul nodo non visitato
        }
    }
    
    if(cont==n){
        cont=0;//se il numero di componenti è uguale al numero di nodi allora vuol dire che non ho eseguito dfs su nessun nodo, che il grafo non è collegato
    }
    return cont; //ritorno il numero di componenti connesse .
}
