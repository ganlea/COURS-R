#*****************************exercices senace4*************
#*exercices cours


## importation de donnees 
# type csv
chemin="C:/Users/LENOVO/Desktop/isep2/sem2/cours R/Cours_R_ISEP2"
file = paste0(chemin , "/", "cours004_base.csv")
base = read.csv2(file)
View(base)
reg= base$region
table(reg)

#type txt
chemin = "C:/Users/LENOVO/Desktop/isep2/sem2/cours R/Cours_R_ISEP2"
file = paste0(chemin , "/", "cours004_base.txt")
Baset =read.table("cours004_base.txt", header = TRUE)

##Manipulation des données
# selection des variable
sexe<-base$sexe
View(sexe)

#selctionnons les personnes de sexe maxulin
M1 <- base[base$sexe == "M",]
print(M1)
M2 <-base[ sexe == "M",]
print(M2)
age<-base[base$age>30,]
print(age)

#aggregation des données par région
age_moy_reg<-aggregate(base$age,by=list(base$region),FUN=mean)
print(age_moy_reg)


######Satstistiques descriptives
#calculons la moyennes des ages
mean_age=mean(base$age)
print(mean_age)
#calculons la note moyenne
notes_mean=mean(base$Note)
print(notes_mean)
#calculs des ecarts types
ecartype_age=sd(base$age)
print(ecartype_age)

####Manipulation des données avec dplyr et tidyr
#filtrons les individus ayant plus de 20 ans et ayant une notes superieurs à 10
library(dplyr)
vingtans_notes_sup10<-base%>%filter(age>20,Note>10)
print(vingtans_notes_sup10) 
#### Visualisation des données
library(ggplot2)
## histogramme de l'age des clients
ggplot(base,aes(x=age))+geom_histogram()
#Nuage des points
ggplot(base,aes(x=REVENU,y=age))+geom_point()
# Graphique en boîtes de l'âge par rapport à la région
ggplot(base, aes(x = region, y = age)) + 
  geom_boxplot()
#carte thermique
# Créer une matrice de données
matrice <- matrix(c(2, 5, 3, 6, 4, 7, 5, 8, 6), ncol = 3)

# Créer la carte thermique
heatmap(matrice, main = "Carte thermique des données")




#*********************************exercice2*********************
#importation des données
chemin="C:/Users/LENOVO/Desktop/isep2/sem2/cours R/Cours_R_ISEP2"
file=paste0(chemin,"/Chefmenage.csv")
Base=read.csv2(file)
View(Base) 
#4-description des variables
summary(Base)
summary(Base$age)
summary(Base$sexe)
summary(Base$occupation)
#5-creation de la variable classe d'age
Base$classe_age=cut(Base$age,breaks = c(18,30,50,Inf),labels=c("18-30","30-50","Sup 50"))
# Créer une sous-base ne contenant que les femmes
sous_base_femmes <- subset(Base, sexe == "F")             
View(sous_base_femmes)                    
##faisons une analyse croisée
#sexe et occupation
table_sexe_occupation=table(Base$sexe,Base$occupation)
print(table_sexe_occupation)
#output 8 hommes sur 1O sont occupés
#sexe et situation matrimoniale
table_sexe_situationmatri=table(Base$sexe,Base$situation.matri)
print(table_sexe_situationmatri)
#output 9 femmes sur 10 sont mariées

