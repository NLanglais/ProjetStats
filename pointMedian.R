library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(mapsf)

library(ICSNP)

vote <- read.csv('/Users/guardiola/Desktop/ENSG_troisieme_annee/Projet/resultats-par-niveau-burvot-t1-france-entiere.txt', sep = ';')

commune <- st_read('/Users/guardiola/Desktop/ENSG_troisieme_annee/Projet/communes-20220101-shp/communes-20220101.shp')

dep <- st_read('/Users/guardiola/Desktop/ENSG_troisieme_annee/Projet/departements-20180101-shp/departements-20180101.shp')

#votefiltrerCom <- select( vote , 'Code.du.département', 'Code.de.la.commune','Blancs','Abstentions','Nuls','Nom_1',
#'Voix_1','Nom_2', 'Voix_2','Nom_3', 'Voix_3', 'Nom_4', 'Voix_4','Nom_5', 'Voix_5','Nom_6', 'Voix_6','Nom_7', #'Voix_7','Nom_8', 'Voix_8','Nom_9', 'Voix_9','Nom_10', 'Voix_10','Nom_11','Voix_11', 'Nom_12', 'Voix_12')

votefiltrerDep <- select( vote , 'Libellé.du.département', 'Blancs','Abstentions','Nuls','Nom_1', 'Voix_1','Nom_2', 'Voix_2','Nom_3', 'Voix_3', 'Nom_4', 'Voix_4','Nom_5', 'Voix_5','Nom_6', 'Voix_6','Nom_7', 'Voix_7','Nom_8', 'Voix_8','Nom_9', 'Voix_9','Nom_10', 'Voix_10','Nom_11','Voix_11', 'Nom_12', 'Voix_12')

votefiltrerDep_avecPour <- select( vote , 'Libellé.du.département', 'Blancs','Abstentions','Nuls','Nom_1', 'Voix_1','X..Voix.Ins_1','Nom_2', 'Voix_2','X..Voix.Ins_2','Nom_3', 'Voix_3','X..Voix.Ins_3', 'Nom_4', 'Voix_4','X..Voix.Ins_4','Nom_5', 'Voix_5','X..Voix.Ins_5','Nom_6', 'Voix_6','X..Voix.Ins_6','Nom_7', 'Voix_7','X..Voix.Ins_7','Nom_8', 'Voix_8','X..Voix.Ins_8','Nom_9', 'Voix_9','X..Voix.Ins_9','Nom_10', 'Voix_10','X..Voix.Ins_10','Nom_11','Voix_11','X..Voix.Ins_11', 'Nom_12', 'Voix_12','X..Voix.Ins_12')


votegroupe <- votefiltrerDep %>% group_by(Libellé.du.département)  %>% summarize (
Voix_1 = sum(Voix_1),
Voix_2 = sum(Voix_2),
Voix_3 = sum(Voix_3),
Voix_4 = sum(Voix_4),
Voix_5 = sum(Voix_5),
Voix_6 = sum(Voix_6),
Voix_7 = sum(Voix_7),
Voix_8 = sum(Voix_8),
Voix_9 = sum(Voix_9),
Voix_10 = sum(Voix_10),
Voix_11 = sum(Voix_11),
Voix_12 = sum(Voix_12))

votegroupe <- select(votegroupe , 'Libellé.du.département', 'Blancs','Abstentions','Nuls','Nom_1', 'Voix_1','Nom_2', 'Voix_2','Nom_3', 'Voix_3', 'Nom_4', 'Voix_4','Nom_5', 'Voix_5','Nom_6', 'Voix_6','Nom_7', 'Voix_7','Nom_8', 'Voix_8','Nom_9', 'Voix_9','Nom_10', 'Voix_10','Nom_11','Voix_11', 'Nom_12', 'Voix_12')




#votefiltrer$codeinsee = as.integer(votefiltrer$Code.du.département)*1000  + as.integer(votefiltrer$Code.de.la.commune)
#votegroupe <- votefiltrer %>%filter(Code.du.département%in%c('01')) %>% group_by(Code.de.la.commune) %>% #summarize (Voix_2 = sum(Voix_2))

dep <- subset(dep,nom != 'La Réunion')
dep <- subset(dep,nom != 'Guadeloupe')
dep <- subset(dep,nom != 'Guyane')
dep <- subset(dep,nom != 'Martinique')
dep <- subset(dep,nom != 'Mayotte')
dep <- select(dep , 'nom', 'geometry')



dep = left_join(dep,votegroupe[,c("Libellé.du.département","Voix_1","Voix_2","Voix_3","Voix_4","Voix_5","Voix_6","Voix_7","Voix_8","Voix_9","Voix_10","Voix_11","Voix_12")],
                 by=c("nom"="Libellé.du.département"))
    


dep$centroid = dep$geometry %>% st_centroid() 

centroid_coordinates <-  dep$centroid %>% st_coordinates()
dep[is.na(dep)] <- 0

wmeanx_1 <- sum(centroid_coordinates[,1] * dep$Voix_1 )/ sum(dep$Voix_1)
wmeany_1 <- sum(centroid_coordinates[,2] * dep$Voix_1 )/ sum(dep$Voix_1)
wmeanx_2 <- sum(centroid_coordinates[,1] * dep$Voix_2 )/ sum(dep$Voix_2)
wmeany_2 <- sum(centroid_coordinates[,2] * dep$Voix_2 )/ sum(dep$Voix_2)
wmeanx_3 <- sum(centroid_coordinates[,1] * dep$Voix_3 )/ sum(dep$Voix_3)
wmeany_3 <- sum(centroid_coordinates[,2] * dep$Voix_3 )/ sum(dep$Voix_3)
wmeanx_4 <- sum(centroid_coordinates[,1] * dep$Voix_4 )/ sum(dep$Voix_4)
wmeany_4 <- sum(centroid_coordinates[,2] * dep$Voix_4 )/ sum(dep$Voix_4)
wmeanx_5 <- sum(centroid_coordinates[,1] * dep$Voix_5 )/ sum(dep$Voix_5)
wmeany_5 <- sum(centroid_coordinates[,2] * dep$Voix_5 )/ sum(dep$Voix_5)
wmeanx_6 <- sum(centroid_coordinates[,1] * dep$Voix_6 )/ sum(dep$Voix_6)
wmeany_6 <- sum(centroid_coordinates[,2] * dep$Voix_6 )/ sum(dep$Voix_6)
wmeanx_7 <- sum(centroid_coordinates[,1] * dep$Voix_7 )/ sum(dep$Voix_7)
wmeany_7 <- sum(centroid_coordinates[,2] * dep$Voix_7 )/ sum(dep$Voix_7)
wmeanx_8 <- sum(centroid_coordinates[,1] * dep$Voix_8 )/ sum(dep$Voix_8)
wmeany_8 <- sum(centroid_coordinates[,2] * dep$Voix_8 )/ sum(dep$Voix_8)
wmeanx_9 <- sum(centroid_coordinates[,1] * dep$Voix_9 )/ sum(dep$Voix_9)
wmeany_9 <- sum(centroid_coordinates[,2] * dep$Voix_9 )/ sum(dep$Voix_9)
wmeanx_10 <- sum(centroid_coordinates[,1] * dep$Voix_10 )/ sum(dep$Voix_10)
wmeany_10 <- sum(centroid_coordinates[,2] * dep$Voix_10 )/ sum(dep$Voix_10)
wmeanx_11 <- sum(centroid_coordinates[,1] * dep$Voix_11 )/ sum(dep$Voix_11)
wmeany_11 <- sum(centroid_coordinates[,2] * dep$Voix_11 )/ sum(dep$Voix_11)
wmeanx_12 <- sum(centroid_coordinates[,1] * dep$Voix_12 )/ sum(dep$Voix_12)
wmeany_12 <- sum(centroid_coordinates[,2] * dep$Voix_12 )/ sum(dep$Voix_12)

# ARTHAUD = rouge foncée - 
#ROUSSEL = rouge -
#MACRON = jaune -
#LASSALLE = gris -
#LE PEN = bleu foncée -
#ZEMMOUR = marron -
#MÉLENCHON = rouge  -
#HIDALGO = rose -
#JADOT = vert -
#PÉCRESSE = bleu clair -
#POUTOU = rouge foncée - 
#DUPONT-AIGNAN = violet -  


library(tidyverse)
library(ggrepel)

ggplot()  +  geom_point( aes( wmeanx_1 , wmeany_1 ) , col='brown3' ) + geom_point( aes( wmeanx_2 , wmeany_2 ) , col = 'red') + geom_point( aes( wmeanx_3 , wmeany_3 ) , col ='yellow') + geom_point( aes( wmeanx_4 , wmeany_4 ) , col = 'azure4') + geom_point( aes( wmeanx_5 , wmeany_5 ) , col = 'darkblue') + geom_point( aes( wmeanx_6 , wmeany_6 ), col = 'coral4') + geom_point( aes( wmeanx_7 , wmeany_7 ) , col = 'brown2') + geom_point( aes( wmeanx_8 , wmeany_8 ) , col = 'pink') + geom_point( aes( wmeanx_9 , wmeany_9 ), col = 'green') + geom_point( aes( wmeanx_10 , wmeany_10 ) , col = 'blue') + geom_point( aes( wmeanx_11 , wmeany_11 ) , col = 'darkred') + geom_point( aes( wmeanx_12 , wmeany_12 ) , col = 'blueviolet') + 
  geom_text_repel(
    aes(label = nom), data = dep, color = "black", size = 3
    )+ labs(title="Point moyen modéré par candidat",
       subtitle="en France",
       color="Candidat",
       caption="Source : INSEE") 


ggplot()  +  geom_point( aes( wmeanx_1 , wmeany_1) , col='brown3' ) + geom_text_repel(
   aes( wmeanx_1 , wmeany_1 , label = nom), data = dep, color = "black", size = 3
    )+ labs(title="Point moyen modéré par candidat",
       subtitle="en France",
       color="Candidat",
       caption="Source : INSEE") 


library(ggplot2)
       
ggplot()  +  geom_point( aes( x = wmeanx_1 , y = wmeany_1  , color = "Arthaud")) + geom_point( aes( x = wmeanx_2 , y = wmeany_2 , color = "Roussel")  + scale_color_manual(names = "", values = c("Arthaud" = "brown3" , "Roussel" = "red")) + labs(title = "", x = "", y="")


# Langueoc Roussillon 

bretagne <- st_read('/Users/guardiola/Desktop/ENSG_troisieme_annee/Projet/departements-20180101-shp/bretagne.shp')

bretagne = left_join(bretagne,votegroupe[,c("Libellé.du.département","Voix_1","Voix_2","Voix_3","Voix_4","Voix_5","Voix_6","Voix_7","Voix_8","Voix_9","Voix_10","Voix_11","Voix_12")],
                 by=c("nom"="Libellé.du.département"))
                 
bretagne$centroid = bretagne$geometry %>% st_centroid() 

centroid_coordinates <-  bretagne$centroid %>% st_coordinates()
bretagne[is.na(dep)] <- 0

dep = bretagne

wmeanx_1 <- sum(centroid_coordinates[,1] * dep$Voix_1 )/ sum(dep$Voix_1)
wmeany_1 <- sum(centroid_coordinates[,2] * dep$Voix_1 )/ sum(dep$Voix_1)
wmeanx_2 <- sum(centroid_coordinates[,1] * dep$Voix_2 )/ sum(dep$Voix_2)
wmeany_2 <- sum(centroid_coordinates[,2] * dep$Voix_2 )/ sum(dep$Voix_2)
wmeanx_3 <- sum(centroid_coordinates[,1] * dep$Voix_3 )/ sum(dep$Voix_3)
wmeany_3 <- sum(centroid_coordinates[,2] * dep$Voix_3 )/ sum(dep$Voix_3)
wmeanx_4 <- sum(centroid_coordinates[,1] * dep$Voix_4 )/ sum(dep$Voix_4)
wmeany_4 <- sum(centroid_coordinates[,2] * dep$Voix_4 )/ sum(dep$Voix_4)
wmeanx_5 <- sum(centroid_coordinates[,1] * dep$Voix_5 )/ sum(dep$Voix_5)
wmeany_5 <- sum(centroid_coordinates[,2] * dep$Voix_5 )/ sum(dep$Voix_5)
wmeanx_6 <- sum(centroid_coordinates[,1] * dep$Voix_6 )/ sum(dep$Voix_6)
wmeany_6 <- sum(centroid_coordinates[,2] * dep$Voix_6 )/ sum(dep$Voix_6)
wmeanx_7 <- sum(centroid_coordinates[,1] * dep$Voix_7 )/ sum(dep$Voix_7)
wmeany_7 <- sum(centroid_coordinates[,2] * dep$Voix_7 )/ sum(dep$Voix_7)
wmeanx_8 <- sum(centroid_coordinates[,1] * dep$Voix_8 )/ sum(dep$Voix_8)
wmeany_8 <- sum(centroid_coordinates[,2] * dep$Voix_8 )/ sum(dep$Voix_8)
wmeanx_9 <- sum(centroid_coordinates[,1] * dep$Voix_9 )/ sum(dep$Voix_9)
wmeany_9 <- sum(centroid_coordinates[,2] * dep$Voix_9 )/ sum(dep$Voix_9)
wmeanx_10 <- sum(centroid_coordinates[,1] * dep$Voix_10 )/ sum(dep$Voix_10)
wmeany_10 <- sum(centroid_coordinates[,2] * dep$Voix_10 )/ sum(dep$Voix_10)
wmeanx_11 <- sum(centroid_coordinates[,1] * dep$Voix_11 )/ sum(dep$Voix_11)
wmeany_11 <- sum(centroid_coordinates[,2] * dep$Voix_11 )/ sum(dep$Voix_11)
wmeanx_12 <- sum(centroid_coordinates[,1] * dep$Voix_12 )/ sum(dep$Voix_12)
wmeany_12 <- sum(centroid_coordinates[,2] * dep$Voix_12 )/ sum(dep$Voix_12)

# + geom_sf(data = bretagne)

ggplot()  + geom_point( aes( wmeanx_1 , wmeany_1 ) , col='brown3' ) + geom_point( aes( wmeanx_2 , wmeany_2 ) , col = 'red') + geom_point( aes( wmeanx_3 , wmeany_3 ) , col ='yellow') + geom_point( aes( wmeanx_4 , wmeany_4 ) , col = 'azure4') + geom_point( aes( wmeanx_5 , wmeany_5 ) , col = 'darkblue') + geom_point( aes( wmeanx_6 , wmeany_6 ), col = 'coral4') + geom_point( aes( wmeanx_7 , wmeany_7 ) , col = 'brown2') + geom_point( aes( wmeanx_8 , wmeany_8 ) , col = 'pink') + geom_point( aes( wmeanx_9 , wmeany_9 ), col = 'green') + geom_point( aes( wmeanx_10 , wmeany_10 ) , col = 'blue') + geom_point( aes( wmeanx_11 , wmeany_11 ) , col = 'darkred') + geom_point( aes( wmeanx_12 , wmeany_12 ) , col = 'blueviolet') + labs(title="Point moyen modéré par candidat",
       subtitle="en France",
       color="Candidat",
       caption="Source : INSEE") 
      
# Bretagne 


bretagne <- st_read('/Users/guardiola/Desktop/ENSG_troisieme_annee/Projet/departements-20180101-shp/languedoc_roussillon.shp')

bretagne = left_join(bretagne,votegroupe[,c("Libellé.du.département","Voix_1","Voix_2","Voix_3","Voix_4","Voix_5","Voix_6","Voix_7","Voix_8","Voix_9","Voix_10","Voix_11","Voix_12")],
                 by=c("nom"="Libellé.du.département"))
                 
bretagne$centroid = bretagne$geometry %>% st_centroid() 

centroid_coordinates <-  bretagne$centroid %>% st_coordinates()
bretagne[is.na(dep)] <- 0

dep = bretagne

wmeanx_1 <- sum(centroid_coordinates[,1] * dep$Voix_1 )/ sum(dep$Voix_1)
wmeany_1 <- sum(centroid_coordinates[,2] * dep$Voix_1 )/ sum(dep$Voix_1)
wmeanx_2 <- sum(centroid_coordinates[,1] * dep$Voix_2 )/ sum(dep$Voix_2)
wmeany_2 <- sum(centroid_coordinates[,2] * dep$Voix_2 )/ sum(dep$Voix_2)
wmeanx_3 <- sum(centroid_coordinates[,1] * dep$Voix_3 )/ sum(dep$Voix_3)
wmeany_3 <- sum(centroid_coordinates[,2] * dep$Voix_3 )/ sum(dep$Voix_3)
wmeanx_4 <- sum(centroid_coordinates[,1] * dep$Voix_4 )/ sum(dep$Voix_4)
wmeany_4 <- sum(centroid_coordinates[,2] * dep$Voix_4 )/ sum(dep$Voix_4)
wmeanx_5 <- sum(centroid_coordinates[,1] * dep$Voix_5 )/ sum(dep$Voix_5)
wmeany_5 <- sum(centroid_coordinates[,2] * dep$Voix_5 )/ sum(dep$Voix_5)
wmeanx_6 <- sum(centroid_coordinates[,1] * dep$Voix_6 )/ sum(dep$Voix_6)
wmeany_6 <- sum(centroid_coordinates[,2] * dep$Voix_6 )/ sum(dep$Voix_6)
wmeanx_7 <- sum(centroid_coordinates[,1] * dep$Voix_7 )/ sum(dep$Voix_7)
wmeany_7 <- sum(centroid_coordinates[,2] * dep$Voix_7 )/ sum(dep$Voix_7)
wmeanx_8 <- sum(centroid_coordinates[,1] * dep$Voix_8 )/ sum(dep$Voix_8)
wmeany_8 <- sum(centroid_coordinates[,2] * dep$Voix_8 )/ sum(dep$Voix_8)
wmeanx_9 <- sum(centroid_coordinates[,1] * dep$Voix_9 )/ sum(dep$Voix_9)
wmeany_9 <- sum(centroid_coordinates[,2] * dep$Voix_9 )/ sum(dep$Voix_9)
wmeanx_10 <- sum(centroid_coordinates[,1] * dep$Voix_10 )/ sum(dep$Voix_10)
wmeany_10 <- sum(centroid_coordinates[,2] * dep$Voix_10 )/ sum(dep$Voix_10)
wmeanx_11 <- sum(centroid_coordinates[,1] * dep$Voix_11 )/ sum(dep$Voix_11)
wmeany_11 <- sum(centroid_coordinates[,2] * dep$Voix_11 )/ sum(dep$Voix_11)
wmeanx_12 <- sum(centroid_coordinates[,1] * dep$Voix_12 )/ sum(dep$Voix_12)
wmeany_12 <- sum(centroid_coordinates[,2] * dep$Voix_12 )/ sum(dep$Voix_12)

 + geom_sf(data = bretagne)
+ geom_sf(data = bretagne) 
ggplot()   + geom_point( aes( wmeanx_1 , wmeany_1 ) , col='brown3' ) + geom_point( aes( wmeanx_2 , wmeany_2 ) , col = 'red') + geom_point( aes( wmeanx_3 , wmeany_3 ) , col ='yellow') + geom_point( aes( wmeanx_4 , wmeany_4 ) , col = 'azure4') + geom_point( aes( wmeanx_5 , wmeany_5 ) , col = 'darkblue') + geom_point( aes( wmeanx_6 , wmeany_6 ), col = 'coral4') + geom_point( aes( wmeanx_7 , wmeany_7 ) , col = 'brown2') + geom_point( aes( wmeanx_8 , wmeany_8 ) , col = 'pink') + geom_point( aes( wmeanx_9 , wmeany_9 ), col = 'green') + geom_point( aes( wmeanx_10 , wmeany_10 ) , col = 'blue') + geom_point( aes( wmeanx_11 , wmeany_11 ) , col = 'darkred') + geom_point( aes( wmeanx_12 , wmeany_12 ) , col = 'blueviolet') + labs(title="Point moyen modéré par candidat",
       subtitle="en France",
       color="Candidat",
       caption="Source : INSEE") 

         
                 
                 
                 
                 
                 
                 
                 