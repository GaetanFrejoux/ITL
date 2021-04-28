%{
    #include <stdbool.h>

    int nbLines,nbConcert,totalPlaces;
    //alloue espace nécessaire en supposant qu'un prenom/nom
    //soit plus petit que 128 caractères.
    char dossierNumero[8],nom[128]; 

    //document valide ?
    bool isValid = true;
%}
maj [A-Z]
number [0-9]
nbPlaces [1-9]{number}?
motComp {maj}+"-"?{maj}+
prenomNom {motComp}"/"{motComp}
sautDeLigne \n
majOrNumber {maj}|{number}
nomConcert (({majOrNumber}+"-"?)?{majOrNumber}+)+
day ([012]{number})|(3[01])
month (0?[1-9])|(1[012])
year {number}{2}
date {day}"/"{month}("/"{year})?
heure ([01]{number})|(2[0-3])
minute [0-5]{number}
horaire {heure}":"{minute}
blancs [\t ]+
ligneDossier DOSSIER{blancs}{number}{8}{sautDeLigne}
ligneProprietaire {prenomNom}{sautDeLigne}
ligneConcert T{number}{2,6}{blancs}{nomConcert}{blancs}{date}{blancs}{horaire}{blancs}{nbPlaces}{blancs}places{sautDeLigne}
%%

{sautDeLigne}       { isValid = false; return 0;}
{ligneDossier}      {
                        if(nbLines==0) //Doit être à la premiere ligne
                        {
                            int len = yyleng;     //longueur de texte matché
                            int debut = len-9;    // début du numéro du dossier
                            int fin = len-1;      //fin du numéro de dossier

                            for(int i = debut ; i < fin ; i++){ //boucle pour récuperer les valeurs
                                dossierNumero[i-debut] = yytext[i];
                            }

                            nbLines++;
                        }
                        else
                        {
                            isValid = false;
                            fprintf(stderr,"ligneDossier doit être en première ligne\n");
                            return 0;
                        }
                    }
{ligneProprietaire} {
                        if(nbLines==1) //Doit être à la deuxième ligne
                        {
                            int len = yyleng;     //longueur de texte matché
                            int debut = 0;        // début du prenomNom
                            int fin = len-1;      //fin du prenomNom

                            for(int i = debut ; i < fin ; i++){ //boucle pour récuperer les valeurs
                                nom[i-debut] = yytext[i];
                            }
                            nbLines++;
                        }

                        else
                        {
                            isValid = false;
                            fprintf(stderr,"ligneProprietaire doit être en deuxième ligne\n");
                            return 0;
                        }
                    }
{ligneConcert}      {

                        nbConcert++; 
                        nbLines++; 
                    
                        if(nbLines>=2) 
                        {
                            int len = yyleng;     //longueur de texte matché
                            int i = len-8;    // index de début qui correspond à l'index avant le mot places
                            while(yytext[i]<'0' || yytext[i]>'9'){ //saute tous les caractères qui ne sont pas des chiffres.
                                i--;
                            }
                            /*                                
                                Sachant que le nombre de places est compris entre 1 et 99,
                                Nous pouvons être sûrs que le nombre de chiffres du nombre
                                est entre 1 et 2. Il suffit donc de tester le caractères précèdent.
                                et s'il s'agit d'un chiffre, ajouter sa valeur x10 car il s'agit
                                du chiffre corresponds à la colonne des dizaines et ensuite ajouter
                                le premier chiffre correspond à la colonne des unités.
                            */
                            if(yytext[i-1]>='0' && yytext[1]<='9'){ 
                                totalPlaces+=(int)(yytext[i-1]-'0')*10 + (int)(yytext[i]-'0');
                            }
                            else totalPlaces+=(int)(yytext[i]-'0');
                        }
                        else
                        {
                            isValid = false;
                            fprintf(stderr,"ligneConcert au mauvaise endroit");
                            return 0;
                        }
                    }
.*                  {
                        isValid = false;
                        fprintf(stderr,"Mot non reconnu : %s\n",yytext);
                        return 0;
                    }
<<EOF>>           {return 0;}
%%
int main(){
    yylex();
    if(isValid){
        printf("\n\nPour le dossier %s, %s a acheté %d places de %d concerts\n\n\n",dossierNumero,nom,totalPlaces,nbConcert);
    }
    else {
        fprintf(stderr,"\n\nErreur de saisie\n\n\n");
    }
    return 0;
}