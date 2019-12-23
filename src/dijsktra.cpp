#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export(.ricercamin)]]
int ricercamin(NumericVector p, NumericVector u, int n1){
    int trovato=0;
    int m;
    int i=0;
    while(trovato==0 & i<n1){ // inizializzo m con un indice che appartiene all'insieme in modo da non cercare il valore minimo che appartiene a un nodo che già abbiamo analizzato
        if(u[i]==1){
            trovato=1;
            m=i;
        }
        else{
            i++;
        }
    }
    for(i=0;i<n1;i++){
        if((p[i]<p[m]) & (u(i)!=0)){
            m=i;
        }
    }
    return m; //ritorno l'indice del potenziale con il valore minimo
}


// [[Rcpp::export]]
List dijkstra (NumericMatrix w, CharacterVector src){
    
    int n=w.nrow();
    List result;
    CharacterVector rn1=rownames(w);
    NumericMatrix dijkstram(n,n); //matrice in cui copio la matrice in ingresso
    rownames(dijkstram)=rn1; //associo il nome delle righe di dijkstram
    colnames(dijkstram)=rn1; //associo il nome delle colonne di dijkstram
    NumericMatrix predecessors(n,n);//dichiaro la matrice dei predecessori
    rownames(predecessors)=rn1; //associo il nome delle righe di predecessors
    colnames(predecessors)=rn1; //associo il nome delle colonne di predecessors
    NumericVector potenziale(n); //vettore dei potenziali
    NumericVector predecessore (n); //vettore dei predecessori
    NumericVector insieme (n); //vettore insieme man mano che visito un nodo tolgo un elemento dall'insieme
    int i,j,k;
    for(i=0;i<n;i++){ //eseguo copia matrice in ingresso su dijkstram
        for(j=0;j<n;j++){
            dijkstram(i,j)=w(i,j);
            predecessors(i,j)=0; // alla matrice dei predecessori associo tutti 0
        }
    }
    String s=as<String>(src);
    int ip; //indice potenziale radice
    ip=rn1.findName(s); //ricerco l'indice della radice all'interno del vettore che contiene i nomi delle righe
    potenziale(ip)=0; //al potenziale con indice della radice associo 0 a tutti gli altri un valore infinito
    
    for(i=0;i<n;i++){
        if(i!=ip){
            potenziale(i)=R_PosInf; //associo infinito a tutti gli altri elementi
        }
    }
     // al contrario nel vettore dei predecessori all'indice della radice associo 0 e a tutti gli altri elementi associo -1
    predecessore(ip)=0;
    for(i=0;i<n;i++){
        if(i!=ip){
            predecessore(i)=-1;
        }
    }
    
    for(i=0;i<n;i++){
        insieme(i)=1; //insieme all'inizio sarà di tutti 1 man mano che controllo un nodo metto 0 all'indice del nodo
    }
    
    for(i=0;i<n;i++){
        j=ricercamin(potenziale,insieme,n);//a j associo l'indice del potenziale minimo
        for(k=0;k<n;k++){
            //controllo se ho un collegamento e scorro tutto il vettore dei potenziali controllando se l'elemento su cui sono è maggiore (dell'elemento minimo del potenziale sommato all'elemento contenuto nella matrice di adiacenza di riga di indice potenziale minimo colonna k)
            
            if((w(j,k)!=0) & (potenziale(k)>potenziale(j)+w(j,k))){
                //se risulta vera questa condizione al potenziale (k) associo la somma
                potenziale(k)=potenziale(j)+w(j,k);
                predecessore(k)=j; //al predecessore di k associo l'indice minimo cercato prima
            }
        }
        //eseguo questo fin tanto che non ho l'insieme vuoto ovvero ho controllato tutti i nodi
        insieme(j)=0;
    }
    
    //associo il vettore potenziale alla riga con l'indice della radice che ho passato all'inizio
    for(j=0;j<n;j++){
         dijkstram(ip,j)=potenziale(j);
         predecessors(ip,j)=predecessore(j); //associo il vettore dei predecessori alla riga con l'indice della radice che ho passato all'inizio
    }
    result=List::create(dijkstram,predecessors); // metto entrambe le matrici in lista e ritorno la lista
    return result;
}



