

git fetch origin # récupère les branches origin/ mais ne change rien localement
git reset origin/master --hard # beaucoup moins de chances que ça échoue que pull
```
 
Plus blindé:
```
git checkout master
git clean -fdx # Supprime _tous_ les fichiers et dossiers non trackés
git fetch origin
git reset origin/master --hard
```
de cette façon là y'a beaucoup moins de chances de garder un état côté serveur.
 
 
