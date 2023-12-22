library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(mapsf)
library(tidyverse)
library(ICSNP)
library(ggpubr)
library(factoextra)

# lecture des données 

# bretagne 
dep <- st_read('D:/workspace/ProjetStats-main/DONNÉES/departements-20180101-shp/bretagne.shp')

# toute la france  
dep <- st_read('D:/workspace/ProjetStats-main/DONNÉES/departements-20180101-shp/departements-20180101.shp')
dep <- subset(dep,nom != 'La Réunion')
dep <- subset(dep,nom != 'Guadeloupe')
dep <- subset(dep,nom != 'Guyane')
dep <- subset(dep,nom != 'Martinique')
dep <- subset(dep,nom != 'Mayotte')
dep <- subset(dep,nom != 'Corse-du-Sud')
dep <- subset(dep,nom != 'Haute-Corse')
dep <- select(dep , 'nom', 'geometry')


# lecture des csv avec les votes 
vote2007 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/2007.csv', sep = ';')
vote2012 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/2012.csv', sep = ';')
vote2017 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/PR17_BVot_T1_FE.csv', sep = ';')
vote2022 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/resultats-par-niveau-burvot-t1-france-entiere.csv', sep = ';')

# selection des votes 
vote2007 <- select( vote2007 , 'Libellé.du.département', 'Blancs.et.nuls', 'Voix', 'Voix.1', 'Voix.2', 'Voix.3', 'Voix.4', 'Voix.5', 'Voix.6', 'Voix.7', 'Voix.8', 'Voix.9', 'Voix.10','Voix.11')
vote2012 <- select( vote2012 , 'Libellé.du.département', 'Blancs.et.nuls','Voix', 'Voix.1', 'Voix.2', 'Voix.3', 'Voix.4', 'Voix.5', 'Voix.6', 'Voix.7', 'Voix.8', 'Voix.9')
vote2017 <- select( vote2017 , 'LibellÈ.du.dÈpartement','Blancs', 'Voix_1', 'Voix_2', 'Voix_3', 'Voix_4', 'Voix_5', 'Voix_6', 'Voix_7', 'Voix_8', 'Voix_9', 'Voix_10','Voix_11')
vote2022<- select( vote2022 , 'Libellé.du.département', 'Blancs', 'Voix_1','Voix_2', 'Voix_3', 'Voix_4', 'Voix_5', 'Voix_6', 'Voix_7', 'Voix_8', 'Voix_9', 'Voix_10','Voix_11', 'Voix_12')

# grouper par departements 
votegroupe2007 <- vote2007 %>% group_by(Libellé.du.département)  %>% summarize (
Voix.0 = sum(Blancs.et.nuls), Voix.1 = sum(Voix), Voix.2 = sum(Voix.1), Voix.3 = sum(Voix.2), Voix.4 = sum(Voix.3), Voix.5 = sum(Voix.4), Voix.6 = sum(Voix.5),Voix.7 = sum(Voix.6),Voix.8 = sum(Voix.7), Voix.9 = sum(Voix.8), Voix.10 = sum(Voix.9), Voix.11 = sum(Voix.10), Voix.12 = sum(Voix.11))
votegroupe2012 <- vote2012%>% group_by(Libellé.du.département)  %>% summarize (
Voix_0 = sum(Blancs.et.nuls),Voix_1 = sum(Voix),Voix_2 = sum(Voix.1),Voix_3 = sum(Voix.2),Voix_4  = sum(Voix.3),Voix_5  = sum(Voix.4),Voix_6  = sum(Voix.5),Voix_7  = sum(Voix.6),Voix_8 = sum(Voix.7),Voix_9  = sum(Voix.8),Voix_10 = sum(Voix.9))
votegroupe2017 <- vote2017%>% group_by(LibellÈ.du.dÈpartement)  %>% summarize (
Voix_0 = sum(Blancs), Voix_1 = sum(Voix_1), Voix_2 = sum(Voix_2), Voix_3 = sum(Voix_3), Voix_4 = sum(Voix_4), Voix_5 = sum(Voix_5), Voix_6 = sum(Voix_6),Voix_7 = sum(Voix_7),Voix_8 = sum(Voix_8), Voix_9 = sum(Voix_9), Voix_10 = sum(Voix_10), Voix_11 = sum(Voix_11))
votegroupe2022 <- vote2022 %>% group_by(Libellé.du.département)  %>% summarize (
Voix_0 = sum(Blancs), Voix_1 = sum(Voix_1), Voix_2 = sum(Voix_2), Voix_3 = sum(Voix_3), Voix_4 = sum(Voix_4), Voix_5 = sum(Voix_5), Voix_6 = sum(Voix_6),Voix_7 = sum(Voix_7),Voix_8 = sum(Voix_8), Voix_9 = sum(Voix_9), Voix_10 = sum(Voix_10), Voix_11 = sum(Voix_11), Voix_12 = sum(Voix_12))

# mettre en minuscule les noms de départements 
dep$nom <- dep$nom %>%  str_to_lower()
votegroupe2007$Libellé.du.département <- votegroupe2007$Libellé.du.département %>%  str_to_lower()
votegroupe2012$Libellé.du.département <- votegroupe2012$Libellé.du.département %>%  str_to_lower()
votegroupe2017$LibellÈ.du.dÈpartement <- votegroupe2017$LibellÈ.du.dÈpartement %>%  str_to_lower()
votegroupe2022$Libellé.du.département <- votegroupe2022$Libellé.du.département %>%  str_to_lower()


# enelever les accents et mettre que des tirets 
dep$nom <- gsub(' ' , '-' , dep$nom )
dep$nom <- gsub('è' , 'e' , dep$nom)
dep$nom <- gsub('é' , 'e' , dep$nom)
dep$nom <- gsub('ô' , 'o' , dep$nom)
votegroupe2007$Libellé.du.département <- gsub(' ' , '-' , votegroupe2007$Libellé.du.département )
votegroupe2007$Libellé.du.département <- gsub('è' , 'e' , votegroupe2007$Libellé.du.département )
votegroupe2007$Libellé.du.département  <- gsub('é' , 'e' , votegroupe2007$Libellé.du.département )
votegroupe2007$Libellé.du.département  <- gsub('ô' , 'o' , votegroupe2007$Libellé.du.département )
votegroupe2012 $Libellé.du.département <- gsub(' ' , '-' , votegroupe2012 $Libellé.du.département )
votegroupe2012 $Libellé.du.département <- gsub('è' , 'e' , votegroupe2012 $Libellé.du.département )
votegroupe2012 $Libellé.du.département  <- gsub('é' , 'e' , votegroupe2012 $Libellé.du.département )
votegroupe2012 $Libellé.du.département  <- gsub('ô' , 'o' , votegroupe2012 $Libellé.du.département )
votegroupe2017 $LibellÈ.du.dÈpartement <- gsub(' ' , '-' , votegroupe2017 $LibellÈ.du.dÈpartement )
votegroupe2017$LibellÈ.du.dÈpartement <- gsub('è' , 'e' , votegroupe2017 $LibellÈ.du.dÈpartement )
votegroupe2017$LibellÈ.du.dÈpartement  <- gsub('é' , 'e' , votegroupe2017 $LibellÈ.du.dÈpartement )
votegroupe2017$LibellÈ.du.dÈpartement  <- gsub('ô' , 'o' , votegroupe2017$LibellÈ.du.dÈpartement )
votegroupe2017$LibellÈ.du.dÈpartement <- gsub('ë' , 'e' , votegroupe2017$LibellÈ.du.dÈpartement)
votegroupe2017$LibellÈ.du.dÈpartement <- gsub('ù' , 'o' , votegroupe2017$LibellÈ.du.dÈpartement)
votegroupe2022$Libellé.du.département <- gsub(' ' , '-' , votegroupe2022$Libellé.du.département )
votegroupe2022$Libellé.du.département <- gsub('è' , 'e' , votegroupe2022$Libellé.du.département )
votegroupe2022$Libellé.du.département  <- gsub('é' , 'e' , votegroupe2022$Libellé.du.département )
votegroupe2022$Libellé.du.département  <- gsub('ô' , 'o' , votegroupe2022$Libellé.du.département )

# jointure entre les données de vote et de départements
dep2007 = left_join(dep,votegroupe2007[,c("Libellé.du.département","Voix.0","Voix.1","Voix.2","Voix.3","Voix.4","Voix.5","Voix.6","Voix.7","Voix.8","Voix.9","Voix.10","Voix.11","Voix.12")],
                 by=c("nom"="Libellé.du.département"))
dep2012 = left_join(dep,votegroupe2012[,c("Libellé.du.département","Voix_0","Voix_1","Voix_2","Voix_3","Voix_4","Voix_5","Voix_6","Voix_7","Voix_8","Voix_9","Voix_10")],
                 by=c("nom"="Libellé.du.département"))      
dep2017 = left_join(dep,votegroupe2017[,c("LibellÈ.du.dÈpartement","Voix_0","Voix_1","Voix_2","Voix_3","Voix_4","Voix_5","Voix_6","Voix_7","Voix_8","Voix_9","Voix_10", "Voix_11")],
                 by=c("nom"="LibellÈ.du.dÈpartement"))
dep2022 = left_join(dep,votegroupe2022[,c("Libellé.du.département","Voix_0","Voix_1","Voix_2","Voix_3","Voix_4","Voix_5","Voix_6","Voix_7","Voix_8","Voix_9","Voix_10","Voix_11","Voix_12")],
                 by=c("nom"="Libellé.du.département"))

# initalisation des noms, de la couleur et des parties 
nom2007 <- c('BLANCS', 'BESANCENOT','BUFFET', 'SCHIVARDI', 'BAYROU', 'BOVÉ','VOYNET', 'de VILLIERS','ROYAL', 'NIHOUS','LE PEN', 'LAGUILLER','SARKOZY')
parti2007 <- c( 'Blancs', 'LO','PCF', 'FO', 'Centre', 'gauche','EELV', 'Indep','PS', 'CPNT','RN', 'LO','LR')
nom2012 <- c( 'BLANCS', 'JOLY','LE PEN', 'SARKOZY', 'MÉLENCHON', 'POUTOU','ARTHAUD', 'CHEMINADE','BAYROU', 'DUPONT-AIGNAN','HOLLANDE')
parti2012 <- c( 'Blancs', 'EELV' , 'RN' , 'LR' , 'FGR - LFI' , 'FO' , 'LO' , 'Souv' , 'Centre' , 'DLF' , 'PS')
nom2017 <- c( 'BLANCS', 'DUPONT-AIGNAN','LE PEN', 'MACRON', 'HAMON', 'ARTHAUD','POUTOU', 'CHEMINADE','LASSALLE', 'M...LENCHON','ASSELINEAU','FILLON')
parti2017 <- c( 'Blancs', 'DLF' , 'RN' , 'Centre' , 'PS' , 'LO' , 'FO' , 'Souv' , 'LASSALLE' , 'FGR - LFI' , 'Ass' ,  'LR')
nom2022 <- c('BLANCS', 'ARTHAUD','ROUSSEL', 'MACRON', 'LASSALLE', 'LE PEN','ZEMMOUR', 'MÉLENCHON','HIDALGO', 'JADOT','PÉCRESSE', 'POUTOU','DUPONT-AIGNAN ')
parti2022 <- c( 'Blancs', 'LO' , 'PCF' , 'Centre' , 'LASSALLE' , 'RN' , 'Reconquête' , 'FGR - LFI' , 'PS' , 'EELV' , 'LR' , 'FO' , 'DLF')

color = c( 'BESANCENOT' = 'brown3' , 'LAGUILLER' = 'brown3' , 'BUFFET' = 'red' , 'MACRON' = 'yellow', 'LE PEN' = 'darkblue' , 'de VILLIERS' = 'coral4' , 'BOVÉ' = 'brown2' , 'Royal' = 'pink' , 'VOYNET' = 'green' , 'SARKOZY' = 'blue' ,'SCHIVARDI' =  'darkred', 'BAYROU' = 'yellow' , 'NIHOUS' = 'darkgreen' , 'BLANCS' = 'black', 'ARTHAUD' = 'brown3' ,  'MACRON' = 'yellow' , 'LASSALLE' = 'azure4' , 'ZEMMOUR' = 'coral4' , 'MÉLENCHON' = 'brown2' , 'HIDALGO' = 'pink' , 'JADOT' = 'green' , 'PÉCRESSE' = 'blue' ,'POUTOU' =  'darkred' , 'DUPONT-AIGNAN ' = 'blueviolet' , 'ROUSSEL' = 'red' , 'CHEMINADE' = 'azure4' , 'ZEMMOUR' = 'coral4' , 'MÉLENCHON' = 'brown2' , 'HOLLANDE' = 'pink' , 'JOLY' = 'green' , 'SARKOZY' = 'blue' ,'POUTOU' =  'darkred' , 'DUPONT-AIGNAN' = 'blueviolet' , 'ARTHAUD' = 'brown3' , 'ROUSSEL' = 'red' , 'BAYROU' = 'yellow' , 'CHEMINADE' = 'azure4' , 'ZEMMOUR' = 'coral4' , 'HOLLANDE' = 'pink' , 'JOLY' = 'green' ,'POUTOU' =  'darkred' , 'DUPONT-AIGNAN' = 'blueviolet', 'HAMON' = 'pink' , 'M...LENCHON' = 'brown2' , 'FILLON' = 'blue' , 'ASSELINEAU' = 'gray')

colorParti <- c('LO' = 'brown3' , 'PCF' = 'Red' , 'Centre' = 'yellow' , 'Indep' = 'gray', 'RN' = 'darkblue', 'Reconquête' = 'coral4' , 'FGR - LFI' = 'brown2', 'PS' = 'pink' , 'EELV' = 'green', 'LR' = 'blue', 'FO' = 'darkred', 'DLF' = 'blueviolet' , 'LASSALLE' = 'azure4' , 'Blancs' = 'black')   

colorCourant <- c('LO' = 'brown3' , 'PCF' = 'brown3' , 'Centre' = 'yellow' , 'Indep' = 'gray', 'RN' = 'darkblue', 'Reconquête' = 'darkblue' , 'FGR - LFI' = 'brown3', 'PS' = 'brown3' , 'EELV' = 'brown3', 'LR' = 'darkblue', 'FO' = 'brown3', 'DLF' = 'darkblue')   

                 
# Creation des centroid et creation des points medians          
dep2007$centroid = dep2007$geometry %>% st_centroid() 
centroid_coordinates <-  dep2007$centroid %>% st_coordinates()
dep2007[is.na(dep2007)] <- 0
dep2007[drop = T]
wmean2007 <- t(sapply((seq(0,12)), function(index){
	c(sum(centroid_coordinates[,1] * dep2007[,paste0('Voix.',index) , drop = T] )/ sum(dep2007[,paste0('Voix.',index) , drop = T] ), sum(centroid_coordinates[,2] * dep2007[,paste0('Voix.',index) , drop = T]  )/ sum(dep2007[,paste0('Voix.',index) , drop = T] ), nom2007[index+1])
}))
dep2012$centroid = dep2012$geometry %>% st_centroid() 
centroid_coordinates <-  dep2012$centroid %>% st_coordinates()
dep2012[is.na(dep2012)] <- 0   
dep2012[drop = T]
wmean2012 <- t(sapply((seq(0,10)), function(index){
	c(sum(centroid_coordinates[,1] * dep2012[,paste0('Voix_',index) , drop = T] )/ sum(dep2012[,paste0('Voix_',index) , drop = T] ), 
	sum(centroid_coordinates[,2] * dep2012[,paste0('Voix_',index) , drop = T]  )/ sum(dep2012[,paste0('Voix_',index) , drop = T] ) , nom2012[index+1])
}))
dep2017$centroid = dep2017$geometry %>% st_centroid() 
centroid_coordinates <-  dep2017$centroid %>% st_coordinates()
dep2017[is.na(dep2017)] <- 0   
dep2017[drop = T]
wmean2017 <- t(sapply((seq(0,11)), function(index){
	c(sum(centroid_coordinates[,1] * dep2017[,paste0('Voix_',index) , drop = T] )/ sum(dep2017[,paste0('Voix_',index) , drop = T] ), 
	sum(centroid_coordinates[,2] * dep2017[,paste0('Voix_',index) , drop = T]  )/ sum(dep2017[,paste0('Voix_',index) , drop = T] ) , nom2017[index+1])
}))
dep2022$centroid = dep2022$geometry %>% st_centroid() 
centroid_coordinates <-  dep2022$centroid %>% st_coordinates()
dep2022[is.na(dep2022)] <- 0
dep2022[drop = T]
wmean2022 <- t(sapply((seq(0,12)), function(index){
	c(sum(centroid_coordinates[,1] * dep2022[,paste0('Voix_',index) , drop = T] )/ sum(dep2022[,paste0('Voix_',index) , drop = T] ), sum(centroid_coordinates[,2] * dep2022[,paste0('Voix_',index) , drop = T]  )/ sum(dep2022[,paste0('Voix_',index) , drop = T] ), nom2022[index+1])
}))

# Passage en dataframe et convertir les coordonnées en flotant 
wmean2007 <- as.data.frame(wmean2007)
wmean2012 <- as.data.frame(wmean2012)
wmean2017 <- as.data.frame(wmean2017)
wmean2022 <- as.data.frame(wmean2022)
names(wmean2007 ) <- c('X' , 'Y' , 'nom' )
names(wmean2012 ) <- c('X' , 'Y' , 'nom' )
names(wmean2017 ) <- c('X' , 'Y' , 'nom' )
names(wmean2022 ) <- c('X' , 'Y' , 'nom' )

wmean2007$Y <- as.numeric(wmean2007$Y)
wmean2007$X <- as.numeric(wmean2007$X)
wmean2012 $Y <- as.numeric(wmean2012 $Y)
wmean2012 $X <- as.numeric(wmean2012 $X)
wmean2017$Y <- as.numeric(wmean2017$Y)
wmean2017$X <- as.numeric(wmean2017$X)
wmean2022 $Y <- as.numeric(wmean2022 $Y)
wmean2022 $X <- as.numeric(wmean2022 $X)

# ajout de l'année et des parties 
wmean2007$parti <- parti2007
wmean2012$parti <- parti2012
wmean2017$parti <- parti2017
wmean2022$parti <- parti2022

wmean2007$annee <- 2007
wmean2012$annee <- 2012
wmean2017$annee <- 2017
wmean2022$annee <- 2022

# creation du merge 
wmeanMerge <- rbind( wmean2007  , wmean2012 , wmean2017 ,  wmean2022)

#plot             
w <-  ggplot(wmeanMerge , aes(X , Y)) + geom_label(aes(label = annee) , nudge_y = 0.001) + geom_point(aes(colour = as.character(parti))) + geom_line(aes(colour = as.character(parti))) + scale_colour_manual(values = colorParti) + labs(title="Point moyen modéré par courant et par année",
       subtitle="en Bretagne",
       color="Candidat",
       caption="Source : INSEE") + theme_void()

ww <-  ggplot() + geom_sf(data = dep) + geom_point(data = wmeanMerge, aes(x=X , y=Y , colour = as.character(parti))) + geom_line( data = wmeanMerge, aes(x=X , y=Y , colour = as.character(parti))) + scale_colour_manual(values = colorParti) + labs(title="Point moyen modéré par candidat",
       subtitle="en France",
       color="Candidat",
       caption="Source : INSEE") 

# Filtrage si besoin
#wmeanMerge$X_ <- NaN
#wmeanMerge$Y_ <- NaN
#wmeanMerge$X_[ wmeanMerge$annee == '2007'] <- wmeanMerge$X[ wmeanMerge$annee == '2007'] 
#wmeanMerge$Y_[ wmeanMerge$annee == '2007'] <- wmeanMerge$Y[ wmeanMerge$annee == '2007'] 
#wmeanMerge$X_[ wmeanMerge$annee == '2012'] <- wmeanMerge$X[ wmeanMerge$annee == '2012'] 
#wmeanMerge$Y_[ wmeanMerge$annee == '2012'] <- wmeanMerge$Y[ wmeanMerge$annee == '2012'] 

#wmeanMerge$X_ <- NaN
#wmeanMerge$Y_ <- NaN
#wmeanMerge$X_[ wmeanMerge$parti == 'LR'] <- wmeanMerge$X[ wmeanMerge$parti == 'LR'] 
#wmeanMerge$Y_[ wmeanMerge$parti == 'LR'] <- wmeanMerge$Y[ wmeanMerge$parti == 'LR'] 
#wmeanMerge$X_[ wmeanMerge$parti == 'DLF'] <- wmeanMerge$X[ wmeanMerge$parti == 'DLF'] 
#wmeanMerge$Y_[ wmeanMerge$parti == 'DLF'] <- wmeanMerge$Y[ wmeanMerge$parti == 'DLF'] 
#wmeanMerge$X_[ wmeanMerge$parti == 'Reconquête'] <- wmeanMerge$X[ wmeanMerge$parti == 'Reconquête'] 
#wmeanMerge$Y_[ wmeanMerge$parti == 'Reconquête'] <- wmeanMerge$Y[ wmeanMerge$parti == 'Reconquête'] 
#wmeanMerge$X_[ wmeanMerge$parti == 'RN'] <- wmeanMerge$X[ wmeanMerge$parti == 'RN'] 
#wmeanMerge$Y_[ wmeanMerge$parti == 'RN'] <- wmeanMerge$Y[ wmeanMerge$parti == 'RN'] 

#u <-  ggplot(wmeanMerge , aes(X_ , Y_)) + geom_label(aes(label = annee) , nudge_y = 0.001) + geom_point(aes(colour = as.character(parti))) + geom_line(aes(colour = as.character(parti))) + scale_colour_manual(values = colorParti) + labs(title="Point moyen modéré par candidat et par année",subtitle="en France",color="Candidat", caption="Source : INSEE") 
       

####################################################################################################### kmeans
##################################################################################

# selection uniquement des coordonnées 
wmeanMeans <- select(wmeanMerge, 'X' , 'Y')

# executiondu kmeans
data_for_kmeans = kmeans(wmeanMeans, 2 , iter.max=1000)

# visualisation
fviz_cluster(data_for_kmeans, data = wmeanMeans, palette = c('red', 'blue'), geom = "point", ellipse.type = "convex",ggtheme= theme_bw())


#################################################################################
##################### Assemblée Nationale
#################################################################################

wmeanAss <- wmeanMerge

# suppression pour ne garder que 2022
wmeanAss <- subset(wmeanAss,annee != '2007')
wmeanAss <- subset(wmeanAss,annee != '2012')
wmeanAss <- subset(wmeanAss,annee != '2017')

#initalisation
wmeanAss$dist <- 0
          
# calcul des distances par rapport à un candidat
for (i in 1:nrow(wmeanAss)){
	x <- ( -2.818401 -  wmeanAss$X)*cos((46.70894 + wmeanAss$Y)/2)
	y <- 46.70894 - wmeanAss$Y
	wmeanAss$dist <- sqrt( x^2 + y^2 )
}


#####################################################################################################################   Calculer les indices 
#################################################################################

wmeanIndi <- wmean2022

# suppresion des parti présent qu'une ou deux années  
wmeanIndi <- subset(wmeanIndi , parti != 'LASSALLE')
wmeanIndi <- subset(wmeanIndi , parti != 'Reconquête')
wmeanIndi <- subset(wmeanIndi , parti != 'EELV')
wmeanIndi <- subset(wmeanIndi , parti != 'DLF')
wmeanIndi <- subset(wmeanIndi , parti != 'PCF')

# selection des partis
wmeanIndi <- select(wmeanIndi, 'parti')

# boucle pour prendre toutes les coordonnées par parti 
for (i in 1:nrow(wmeanIndi)){
	for (j in 1:nrow(wmean2022)){
		if (wmeanIndi $parti[i] == wmean2022$parti[j] ){
			wmeanIndi $Y_2022[i] <- wmean2022 $Y[j]
			wmeanIndi $X_2022[i] <- wmean2022 $X[j]
		}
	}
	for (j in 1:nrow(wmean2017)){
		if (wmeanIndi $parti[i] == wmean2017$parti[j] ){
			wmeanIndi $Y_2017[i] <- wmean2017 $Y[j]
			wmeanIndi $X_2017[i] <- wmean2017 $X[j]
		}
	}
	for (j in 1:nrow(wmean2012)){
		if (wmeanIndi $parti[i] == wmean2012 $parti[j] ){
			wmeanIndi $Y_2012[i] <- wmean2012 $Y[j]
			wmeanIndi $X_2012[i] <- wmean2012 $X[j]
		}
	}
	for (j in 1:nrow(wmean2007)){
		if (wmeanIndi $parti[i] == wmean2007$parti[j] ){
			wmeanIndi $Y_2007[i]<- wmean2007$Y[j]
			wmeanIndi $X_2007[i] <- wmean2007 $X[j]
		}
	}
}


# calcul des indices 
for (i in 1:nrow(wmeanIndi)){
	wmeanIndi $Vect07_12[i] <- sqrt( ( wmeanIndi $X_2007[i] - wmeanIndi $X_2012[i] ) ^2 +  ( wmeanIndi $Y_2007[i] - wmeanIndi $Y_2012[i] ) ^2 )
	wmeanIndi $Vect12_17[i] <- sqrt( ( wmeanIndi $X_2017[i] - wmeanIndi $X_2012[i] ) ^2 +  ( wmeanIndi $Y_2017[i] - wmeanIndi $Y_2012[i] ) ^2 )
	wmeanIndi $Vect17_22[i] <- sqrt( ( wmeanIndi $X_2017[i] - wmeanIndi $X_2022[i] ) ^2 +  ( wmeanIndi $Y_2017[i] - wmeanIndi $Y_2022[i] ) ^2 )
	wmeanIndi $Vect_sum[i] <- wmeanIndi $Vect07_12[i] + wmeanIndi $Vect12_17[i] + wmeanIndi $Vect17_22[i]
	wmeanIndi $Vect07_22[i] <- sqrt( ( wmeanIndi $X_2007[i] - wmeanIndi $X_2022[i] ) ^2 +  ( wmeanIndi $Y_2007[i] - wmeanIndi $Y_2022[i] ) ^2 )
	
	wmeanIndi$indice_norme[i] <- wmeanIndi$Vect07_22[i] / wmeanIndi$Vect_sum[i] 
	wmeanIndi$indice_ecart_type[i] <- 1 - sd(c(wmeanIndi$Vect07_12[i]  , wmeanIndi$Vect12_17[i] , wmeanIndi$Vect17_22[i]))
}

# plot des indices 
w <-  ggplot(wmeanIndi , aes(indice_ecart_type  ,indice_norme)) + geom_point(aes(colour = as.character(parti))) + scale_colour_manual(values = colorParti) + labs(title="La propagation par rapport à la persuasion de 2007 à 2022",
       subtitle="en France",
       color="Candidat",
       caption="Source : INSEE") 

w

