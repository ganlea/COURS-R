####
####
####
#******** Premier exercice *******#

# opérateurs et expressions:
# expemple d'utilisation operateurs binaires:`
a= 5
b = 2
print ( a%/%b) # 2

# opérateusr unitaires
c= -a
print(c) # -5
print(!c(1,F)) #FALSE  TRUE

# opérateurs relationnels
# opérateur ==
print(5==2) # FALSE
d= c(1,4,FALSE,1)
e = c(7, 4,0,6)
f = d==e
print(f) # FALSE  TRUE  TRUE FALSE

vect1= c(12,2,1,6,0,4,TRUE, 9,1)
index1= vect1==1
vrai1=vect1[index1]
print(vrai1) # 1 1 1

car1= "aller"
print(car1=="finir") #False
print(car1=="Aller") # False
print(car1=="aller") # True

print((a==5) == TRUE) # TRUE
print((TRUE==1)==1) # TRUE
print(5==5L) # True

# opérateur !=
print(5!=2) # TRUE
d= c(1,4,FALSE,1)
e = c(7, 4,0,6)
f = d!=e
print(f) # TRUE FALSE FALSE  TRUE

vect1= c(12,2,1,6,0,4,TRUE, 9,1)
index1= vect1!=1
vrai1=vect1[index1]
print(vrai1) # 12  2  6  0  4  9

car1= "aller"
print(car1!="finir") #TRUE
print(car1!="Aller") # TRUE
print(car1!="aller") # FALSE

print((a==5) != TRUE) # FALSE
print((TRUE!=1)==0) # TRUE

#opérateurs de comparaison
print(1<=TRUE) # TRue
print(FALSE > TRUE) # FALSE
print("aba">"aab") # TRUE
print(2<"a") # TRUE

# autres opérateurs
vect2 =c(F,T,F)
vect3= c(T,T,T)
print(c(1,4,5)&c(1,5,0)) # TRUE  TRUE FALSE
print(TRUE & FALSE) # FALSE
#print(vect2 && vect3)
print(F|T) #TRUE
#print(vect2||vect3)

print(NA | F) # NA
print(NA & F) # NA


















####
####
####
#******** deuxieme exercice *******#

5 * c(2, 3, 8, 10)          
c(2, 6, 8) + c(1, 4, 9)     
c(0, 3, -1, 4)^2            
8 + 1:10                   
c(2, 5) * 1:10              
c(-2, 3, -1, 4) ^ 1:4 # ne donne pas le resultat attendu


(x <- matrix(1:4, 2))       
(y <- matrix(3:6, 2))     
5 * x                       
x + y                       
x * y                       
x %*% y                     
x / y                      
x * c(2, 3)                

5 %% 2                     
5 %% 1:5                 
10 %% 1:15              


for (i in 1:50)
{
  ## Affiche la valeur du compteur toutes les 5 itérations.
  if (0 == i %% 5)
    print(i)
}


5 %/% 1:5
10 %/% 1:15

## Le ET logique est vrai seulement lorsque les deux
## expressions sont vraies.
c(TRUE, TRUE, FALSE) & c(TRUE, FALSE, FALSE)

## Le OU logique est faux seulement lorsque les deux
## expressions sont fausses.
c(TRUE, TRUE, FALSE) | c(TRUE, FALSE, FALSE)

## La négation logique transforme les vrais en faux et vice
## versa.
! c(TRUE, FALSE, FALSE, TRUE)

## On peut utiliser les opérateurs logiques &, | et !
## directement avec des nombres. Dans ce cas, le nombre zéro
## est traité comme FALSE et tous les autres nombres comme
## TRUE.
0:5 & 5:0
0:5 | 5:0
!0:5

## Ainsi, dans une expression conditionnelle, inutile de
## vérifier si, par exemple, un nombre est égal à zéro. On
## peut utiliser le nombre directement et sauver des
## opérations de comparaison qui peuvent devenir coûteuses en
## temps de calcul.
x <- 1                      # valeur quelconque
if (x != 0) x + 1           # TRUE pour tout x != 0
if (x) x + 1                # tout à fait équivalent!

## L'exemple de boucle ci-dessus peut donc être légèrement
## modifié.
for (i in 1:50)
{
  ## Affiche la valeur du compteur toutes les 5 itérations.
  if (!i %% 5)
    print (i)
}

## Dans les calculs numériques, TRUE vaut 1 et FALSE vaut 0.
a <- c("Impair", "Pair")
x <- c(2, 3, 6, 8, 9, 11, 12)
x %% 2
(!x %% 2) + 1
a[(!x %% 2) + 1]

## Un mot en terminant sur l'opérateur '=='. C'est l'opérateur
## à utiliser pour vérifier si deux valeurs sont égales, et
## non '='. C'est là une erreur commune --- et qui peut être
## difficile à détecter --- lorsque l'on programme en R.
5 = 2                       # erreur de syntaxe
5 == 2                      # comparaison

###
### APPELS DE FONCTIONS
###

## Les invocations de la fonction 'matrix' ci-dessous sont
## toutes équivalentes. On remarquera, entre autres, comment
## les arguments sont spécifiés (par nom ou par position).
matrix(1:12, 3, 4)
matrix(1:12, ncol = 4, nrow = 3)
matrix(nrow = 3, ncol = 4, data = 1:12)
matrix(nrow = 3, ncol = 4, byrow = FALSE, 1:12)
matrix(nrow = 3, ncol = 4, 1:12, FALSE)

###
### QUELQUES FONCTIONS UTILES
###

## MANIPULATION DE VECTEURS
x <- c(50, 30, 10, 20, 60, 30, 20, 40) # vecteur non ordonné

## Séquences de nombres.
seq(from = 1, to = 10)          # équivalent à 1:10
seq_len(10)                     # plus rapide que 'seq'
seq(-10, 10, length = 50)       # incrément automatique
seq(-2, by = 0.5, along = x)    # même longueur que 'x'
seq_along(x)                    # plus rapide que 'seq'

## Répétition de nombres ou de vecteurs complets.
rep(1, 10)                  # utilisation de base
rep(x, 2)                   # répéter un vecteur
rep(x, times = 2, each = 4) # combinaison des arguments
rep(x, times = 1:8)         # nombre de répétitions différent
# pour chaque élément de 'x'

## Classement en ordre croissant ou décroissant.
sort(x)                     # classement en ordre croissant
sort(x, decr = TRUE)        # classement en ordre décroissant
sort(c("abc", "B", "Aunt", "Jemima")) # chaînes de caractères
sort(c(TRUE, FALSE))        # FALSE vient avant TRUE

## La fonction 'order' retourne la position, dans le vecteur
## donné en argument, du premier élément selon l'ordre
## croissant, puis du deuxième, etc. Autrement dit, on obtient
## l'ordre dans lequel il faut extraire les données du vecteur
## pour les obtenir en ordre croissant.
order(x)                    # regarder dans le blanc des yeux
x[order(x)]                 # équivalent à 'sort(x)'

## Rang des éléments d'un vecteur dans l'ordre croissant.
rank(x)                     # rang des éléments de 'x'

## Renverser l'ordre d'un vecteur.
rev(x)

## Extraction ou suppression en tête ou en queue de vecteur.
head(x, 3)                  # trois premiers éléments
head(x, -2)                 # tous sauf les deux derniers
tail(x, 3)                  # trois derniers éléments
tail(x, -2)                 # tous sauf les deux premiers

## Expressions équivalentes sans 'head' et 'tail'
x[1:3]                      # trois premiers éléments
x[1:(length(x) - 2)]        # tous sauf les deux derniers
x[(length(x)-2):length(x)]  # trois derniers éléments
rev(rev(x)[1:3])            # avec petits vecteurs seulement
x[c(-1, -2)]                # tous sauf les deux premiers

## Seulement les éléments différents d'un vecteur.
unique(x)

## RECHERCHE D'ÉLÉMENTS DANS UN VECTEUR
which(x >= 30)              # positions des éléments >= 30
which.min(x)                # position du minimum
which.max(x)                # position du maximum
match(20, x)                # position du premier 20 dans 'x'
match(c(20, 30), x)         # aussi pour plusieurs valeurs
60 %in% x                   # 60 appartient à 'x'
70 %in% x                   # 70 n'appartient pas à 'x'

## ARRONDI
(x <- c(-21.2, -pi, -1.5, -0.2, 0, 0.2, 1.7823, 315))
round(x)                    # arrondi à l'entier
round(x, 2)                 # arrondi à la seconde décimale
round(x, -1)                # arrondi aux dizaines
ceiling(x)                  # plus petit entier supérieur
floor(x)                    # plus grand entier inférieur
trunc(x)                    # troncature des décimales

## SOMMAIRES ET STATISTIQUES DESCRIPTIVES
sum(x)                      # somme des éléments
prod(x)                     # produit des éléments
diff(x)                     # x[2] - x[1], x[3] - x[2], etc.
mean(x)                     # moyenne des éléments
mean(x, trim = 0.125)       # moyenne sans minimum et maximum
var(x)                      # variance (sans biais)
(length(x) - 1)/length(x) * var(x) # variance biaisée
sd(x)                       # écart type
max(x)                      # maximum
min(x)                      # minimum
range(x)                    # c(min(x), max(x))
diff(range(x))              # étendue de 'x'
median(x)                   # médiane (50e quantile) empirique
quantile(x)                 # quantiles empiriques
quantile(x, 1:10/10)        # on peut spécifier les quantiles
summary(x)                  # plusieurs des résultats ci-dessus

## SOMMAIRES CUMULATIFS ET COMPARAISONS ÉLÉMENT PAR ÉLÉMENT
(x <- sample(1:20, 6))
(y <- sample(1:20, 6))
cumsum(x)                   # somme cumulative de 'x'
cumprod(y)                  # produit cumulatif de 'y'
rev(cumprod(rev(y)))        # produit cumulatif renversé
cummin(x)                   # minimum cumulatif
cummax(y)                   # maximum cumulatif
pmin(x, y)                  # minimum élément par élément
pmax(x, y)                  # maximum élément par élément

## OPÉRATIONS SUR LES MATRICES
(A <- sample(1:10, 16, replace = TRUE)) # avec remise
dim(A) <- c(4, 4)           # conversion en une matrice 4 x 4
b <- c(10, 5, 3, 1)         # un vecteur quelconque
A                           # la matrice 'A'
t(A)                        # sa transposée
solve(A)                    # son inverse
solve(A, b)                 # la solution de Ax = b
A %*% solve(A, b)           # vérification de la réponse
diag(A)                     # extraction de la diagonale de 'A'
diag(b)                     # matrice diagonale formée avec 'b'
diag(4)                     # matrice identité 4 x 4
(A <- cbind(A, b))          # matrice 4 x 5
nrow(A)                     # nombre de lignes de 'A'
ncol(A)                     # nombre de colonnes de 'A'
rowSums(A)                  # sommes par ligne
colSums(A)                  # sommes par colonne
apply(A, 1, sum)            # équivalent à 'rowSums(A)'
apply(A, 2, sum)            # équivalent à 'colSums(A)'
apply(A, 1, prod)           # produit par ligne avec 'apply'

## PRODUIT EXTÉRIEUR
x <- c(1, 2, 4, 7, 10, 12)
y <- c(2, 3, 6, 7, 9, 11)
outer(x, y)                 # produit extérieur
x %o% y                     # équivalent plus court
outer(x, y, "+")            # « somme extérieure »
outer(x, y, "<=")           # toutes les comparaisons possibles
outer(x, y, pmax)           # idem

###
### STRUCTURES DE CONTRÔLE
###

## Pour illustrer les structures de contrôle, on a recours à
## un petit exemple tout à fait artificiel : un vecteur est
## rempli des nombres de 1 à 100, à l'exception des multiples
## de 10. Ces derniers sont affichés à l'écran.
##
## À noter qu'il est possible --- et plus efficace --- de
## créer le vecteur sans avoir recours à des boucles.
(1:100)[-((1:10) * 10)]              # sans boucle !
rep(1:9, 10) + rep(0:9*10, each = 9) # une autre façon !

## Bon, l'exemple proprement dit...
x <- numeric(0)             # initialisation du contenant 'x'
j <- 0                      # compteur pour la boucle
for (i in 1:100)
{
  if (i %% 10)            # si i n'est pas un multiple de 10
    x[j <- j + 1] <- i  #     stocker sa valeur dans 'x'
  else                    # sinon
    print(i)            #     afficher la valeur à l'écran
}
x                           # vérification

## Même chose que ci-dessus, mais sans le compteur 'j' et les
## valeurs manquantes aux positions 10, 20, ... , 100 sont
## éliminées à la sortie de la boucle.
x <- numeric(0)
for (i in 1:100)
{
  if (i %% 10)
    x[i] <- i
  else
    print(i)
}
x <- x[!is.na(x)]
x

## On peut refaire l'exemple avec une boucle 'while', mais
## cette structure n'est pas naturelle ici puisque l'on sait
## d'avance qu'il faudra faire la boucle exactement 100
## fois. Le 'while' est plutôt utilisé lorsque le nombre de
## répétitions est inconnu. De plus, une boucle 'while' n'est
## pas nécessairement exécutée puisque le critère d'arrêt est
## évalué dès l'entrée dans la boucle.
x <- numeric(0)
j <- 0
i <- 1                      # pour entrer dans la boucle [*]
while (i <= 100)
{
  if (i %% 10)
    x[j <- j + 1] <- i
  else
    print(i)
  i <- i + 1              # incrémenter le compteur!
}
x

## La remarque faite au sujet de la boucle 'while' s'applique
## aussi à la boucle 'repeat'. Par contre, le critère d'arrêt
## de la boucle 'repeat' étant évalué à la toute fin, la
## boucle est exécutée au moins une fois. S'il faut faire la
## manœuvre marquée [*] ci-dessus pour s'assurer qu'une
## boucle 'while' est exécutée au moins une fois... c'est
## qu'il faut utiliser 'repeat'.
x <- numeric(0)
j <- 0
i <- 1
repeat
{
  if (i %% 10)
    x[j <- j + 1] <- i
  else
    print(i)
  if (100 < (i <- i + 1)) # incrément et critère d'arrêt
    break
}
x

###
### FONCTIONS ADDITIONNELLES
###

## La fonction 'search' retourne la liste des environnements
## dans lesquels R va chercher un objet (en particulier une
## fonction). '.GlobalEnv' est l'environnement de travail.
search()

## Liste de tous les packages installés sur votre système.
library()

## Chargement du package 'MASS', qui contient plusieurs
## fonctions statistiques très utiles.
library("MASS")















####
####
####
#******** troisieme exercice *******#
#*********exercice 3.5******#
x = x %/% y * y + x%%y

#*********exercice 3.6******#
x = sample(1:100, 20, replace = F)
print(x)
print (x[1:5]) # cinq premiers élements
print (max(x)) # valeur maximale de l'echantillon
print(mean(x[1:5])) # moyenne des cinq premiers élements
print(mean(tail(x, 5))) # moyenne des cinq derniers élements

#*********exercice 3.7******#
z = matrix (sample(-50:50, 70, replace = T), nrow = 10, ncol = 7)# dans une 
# liste de 200 elements on prend 70 qui constitueront la matrice 
print(z)
print(rowSums(z)) #somme par ligne de la matrice
print(colMeans(z)) # moyenne par colonne de la matrice
print(max(z[c(1:3),c(1:3)])) # maximum de la matrice majeur 3*3 de z
print(filtre <- rowMeans(z) > 7) #filtre pour prendre les lignes de moyenne > 7
print(z[filtre,c(1:7)]) # application du filtre a la matrice

