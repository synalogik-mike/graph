#include <Rcpp.h>
#include "component1.h"
using namespace Rcpp;

// [[Rcpp::export]]
int find(int index,NumericVector parenti){ //ricerco l'antenato del nodo index
    while(parenti(index)!=index){
        index=parenti(index);
    }
    return index;
}


// [[Rcpp::export]]
int union1 (int x, int y, NumericVector parenti){
    int a=find(x,parenti); //ricerco indice di x e y in parenti
    int b=find(y,parenti);
    if(a!=b){ //se non hanno antenati comuni
        parenti(x)=y; //aggiorno parenti e metto parenti di indice x uguale a y (aggiungo arco all'albero)
        return 1; //se non forma un ciclo ritorno 1 altrimenti 0
    }else{
        return 0;
    }
}

// [[Rcpp::export]]
List chiamakruskal(NumericMatrix weigth, NumericMatrix adj, int comp){
    int cost=0;//variabile costo
    int n=adj.nrow(); //dimensione matrice
    int cont=0;
    //creo matrice di adiacenza come grafo non orientato
    NumericMatrix nonorientato(n,n);
    NumericMatrix tree(n,n); //matrice adiacenza albero
    CharacterVector rn,cn;
    rn=rownames(adj);
    cn=colnames(adj);
    rownames(tree)=rn;
    colnames(tree)=cn;
    int i,j;
    for(i=0;i<n;i++){
        for(j=0;j<n;j++){
            nonorientato(i,j)=0;
        }
    }
    for(i=0;i<n;i++){
           for(j=0;j<n;j++){
               tree(i,j)=0;//inizializzo matrice adiacenza albero a 0
               if(weigth(i,j)>0){
                   nonorientato(i,j)=weigth(i,j);//inizializzo la matrice del grafo dei pesi come non orientato.
                   nonorientato(j,i)=weigth(i,j);
                   
                   }
               if(adj(i,j)==comp){
                   cont++;//conto il numero di archi che ci sono nella componente
               }
           }
       }
    cont=cont/2; //dato che la matrice di adiacenza è non orientata mi conterà il doppio degli archi presenti quindi per ottenere il numero preciso di archi dimezzo
    NumericVector parent(n);//vettore parenti
    int contatore_archi=0; //inizializzo contatore archi
    int min; //inizializzo elemento minimo a +inf
    int a,b; //indici del minimo
    for(i=0;i<n;i++){ // inizializzo vettore dei parenti
        parent(i)=i;
    }
    int aggiunto;
    if(cont>2){
        cont=cont-1;
    } // se ho una componente con due archi connessi devo valutare entrambi gli archi, se cont è minore di 2 non posso diminuire di 1 altrimenti non mette nulla in albero
    while(contatore_archi<cont){
        min=2147483647;
        //ciclo su tutti gli archi
        for(i=0;i<n;i++){ //ricerco il minimo su tutta la matrice
            for(j=0;j<n;j++){
                //se il nodo di partenza ha parente diverso di nodo di arrivo e l'elemento è minore del minimo allora salvo gli indici del minimo e il minimo, se le due find restituiscono il solito indice allora vuol dire che hanno lo stesso parent e creo un ciclo
                if((adj(i,j)==comp)&(parent(i)!=j)&(nonorientato(i,j)<min)){
                    min=nonorientato(i,j);
                    a=i;
                    b=j;
                }
            }
        }
    //con union segno i parenti dei nodi per non creare cicli
    aggiunto=union1(a,b,parent);
    if(aggiunto==1){ //se ho aggiunto l'arco all'albero
        contatore_archi++; //incremento contatore archi albero
        cost+=min; //incremento cost con l'elemento minimo in tutta la matrice
        tree(a,b)=1;
        tree(b,a)=1;
        
    }else{
        nonorientato(a,b)=R_PosInf; //setto a +infinito l'elemento minimo già analizzato
        }
    }
    List result;
    result=List::create(cost,tree);
    
    return result; //ritorno il costo minimo totale dell'albero.
}




// [[Rcpp::export]]

List Kruskal(NumericMatrix matrixpeso, NumericMatrix adjacencymatrix){
    int dim=adjacencymatrix.nrow();
    NumericMatrix support(dim,dim); //matrice di supporto in cui copio i valori della matrice di adiacenza con gli 0 e gli 1 per mantenere inalterata la matrice di adiacenza
    CharacterVector rn,cn;
    rn=rownames(adjacencymatrix);
    cn=colnames(adjacencymatrix);
    rownames(support)=rn;
    colnames(support)=cn;
    
     
    int i,j;
    for(i=0;i<dim;i++){
        for(j=0;j<dim;j++){
            support(i,j)=adjacencymatrix(i,j); //inizializzo support con i valori della matrice di adiacenza
        }
    }
    int n=component1(matrixpeso, support);//conto il numero di componenti della matrice e mi segno nella matrice di adiacenza a quale componente sono associati gli archi
    List costi; //dichiaro vettore costi di dimensione numero di componenti
    if(n==0){
        List cost;
        List finale;
        NumericMatrix albero(dim,dim);
        rownames(albero)=rn;
        colnames(albero)=cn;
        for(i=0;i<dim;i++){
            for(j=0;j<dim;j++){
                albero(i,j)=0;
            }
        }
        int c=0;
        cost=List::create(c,albero);
        finale.push_back(cost);
        return finale; //ritorna lista di lista con costo e matrice adiacenza albero
    }
    if(n==1){
        costi=List::create(chiamakruskal(matrixpeso,support,1)); // se ho una sola componente chiamo subito kruskal.
    }else{ //se ho più componenti chiamo kruskal su ogni componente.
        
        int i;
        for(i=0;i<n;i++){
            costi.push_back(chiamakruskal(matrixpeso,support,i+1)); //per ogni componente richiamo kruskal con la matrice di adiacenza dei pesi, la matrice di adiacenza modificata e il numero della componente e il risultato verrà inserito in una lista
            
        }
        
    }
    return costi;
}
