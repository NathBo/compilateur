# page du cours
https://www.lri.fr/~filliatr/ens/compil/

# compilateur

# utilisation
Pour produire l'exécutable `ppurs` : `make ppurs`. Puis on peut le lancer manuellement avec `./ppurs myFile.purs`.

Mais ce processus est automatisé :

Pour lancer l'exécution sur `test.purs`: `make`.

Pour lancer tous les tests : `make testAll` (ou `make testsX` pour les tests de catégorie X [X=1, X=2 ou X=3])

Pour supprimer tous les fichiers temporaires : `make clean`
