


#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]



NumericMatrix adjacencyMatrix (List input){

    Rcpp::NumericMatrix appo;//matrice di appoggio in cui a ogni ciclo copio sopra la matrice contenuta in nodo lista
    appo=Rcpp::as<Rcpp::NumericMatrix>(input[0]);
    int v=appo.nrow();
    Rcpp::NumericMatrix m1(v,v);
    CharacterVector rn,cn; //vettori di carattere dove metto i nomi di righe e colonne di input[i].matrice
    //creo la matrice vuota
    for(int i=0;i<v;i++){
        for(int j=0;j<v;j++){
            m1(i,j)=0;
        }
    }
    int n=input.size();
    //scorro la lista e faccio l'or tra matrici
    for(int x=0;x<n;x++){
        appo=Rcpp::as<Rcpp::NumericMatrix>(input[x]);//alla matrice appo associo la matrice associata alla                                               cella lista di posizione x
        rn=rownames(appo); //prendo i nomi delle righe di appo
        cn=colnames(appo); //prendo i nomi delle colonne di appo
        rownames(m1)=rn; //setto le righe della matrice finale con i nomi delle righe di appo
        colnames(m1)=cn; //setto le colonne della matrice finale con i nomi delle colonne di appo
        //scorro le matrici e faccio or
        for(int i=0;i<v;i++){
               for(int j=0;j<v;j++){
                   if(appo(i,j)==1){
                      m1(i,j)=1;
                   }
                }
        }
    }
    return m1;
}
