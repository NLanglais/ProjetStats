library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(mapsf)
library(tidyverse)
library(ICSNP)

groupVoteDep <- function(vote,dep,annee){
  
  dep <- subset(dep,nom != 'La Réunion')
  dep <- subset(dep,nom != 'Guadeloupe')
  dep <- subset(dep,nom != 'Guyane')
  dep <- subset(dep,nom != 'Martinique')
  dep <- subset(dep,nom != 'Mayotte')
  dep <- subset(dep,nom != 'Corse-du-Sud')
  dep <- subset(dep,nom != 'Haute-Corse')
  dep <- select(dep , 'nom', 'geometry')
  dep <- dep[-61,]
  
  dep$nom <- dep$nom %>% str_to_upper()
  dep$nom <- gsub('Ô','O',dep$nom)
  dep$nom <- gsub('È','E',dep$nom)
  dep$nom <- gsub('É','E',dep$nom)
  dep$nom <- gsub('Ë','E',dep$nom)
  dep$nom <- gsub('Ù','U',dep$nom)
  dep$nom <- gsub(' ','-',dep$nom)
  
  if(annee == 2022){
    votefiltrerDep <- select( vote , 'Libellé.du.département', 'Blancs','Abstentions','Nuls','Nom_1', 'Voix_1','Nom_2', 'Voix_2','Nom_3', 'Voix_3', 'Nom_4', 'Voix_4','Nom_5', 'Voix_5','Nom_6', 'Voix_6','Nom_7', 'Voix_7','Nom_8', 'Voix_8','Nom_9', 'Voix_9','Nom_10', 'Voix_10','Nom_11','Voix_11','Nom_12','Voix_12')
    
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
    
    votegroupe <- select(votegroupe , 'Libellé.du.département', 'Voix_1', 'Voix_2', 'Voix_3', 'Voix_4', 'Voix_5', 'Voix_6', 'Voix_7', 'Voix_8', 'Voix_9', 'Voix_10','Voix_11','Voix_12')
    
    votegroupe$Libellé.du.département <- votegroupe$Libellé.du.département %>% str_to_upper()
    votegroupe$Libellé.du.département <- gsub('Ô','O',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('È','E',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('É','E',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('Ë','E',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('Ù','O',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub(' ','-',votegroupe$Libellé.du.département)
    
    
    
    dep = left_join(dep,votegroupe[,c("Libellé.du.département","Voix_1","Voix_2","Voix_3","Voix_4","Voix_5","Voix_6","Voix_7","Voix_8","Voix_9","Voix_10","Voix_11","Voix_12")],
                    by=c("nom"="Libellé.du.département"))
  }
  if(annee == 2017){
    votefiltrerDep <- select( vote , 'LibellÈ.du.dÈpartement', 'Blancs','Abstentions','Nuls','Nom_1', 'Voix_1','Nom_2', 'Voix_2','Nom_3', 'Voix_3', 'Nom_4', 'Voix_4','Nom_5', 'Voix_5','Nom_6', 'Voix_6','Nom_7', 'Voix_7','Nom_8', 'Voix_8','Nom_9', 'Voix_9','Nom_10', 'Voix_10','Nom_11','Voix_11')
    
    votegroupe <- votefiltrerDep %>% group_by(LibellÈ.du.dÈpartement)  %>% summarize (
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
      Voix_11 = sum(Voix_11))
    
    votegroupe <- select(votegroupe , 'LibellÈ.du.dÈpartement', 'Voix_1', 'Voix_2', 'Voix_3', 'Voix_4', 'Voix_5', 'Voix_6', 'Voix_7', 'Voix_8', 'Voix_9', 'Voix_10','Voix_11')
  
    votegroupe$LibellÈ.du.dÈpartement <- votegroupe$LibellÈ.du.dÈpartement%>% str_to_upper()
    votegroupe$LibellÈ.du.dÈpartement <- gsub('Ô','O',votegroupe$LibellÈ.du.dÈpartement)
    votegroupe$LibellÈ.du.dÈpartement <- gsub('È','E',votegroupe$LibellÈ.du.dÈpartement)
    votegroupe$LibellÈ.du.dÈpartement <- gsub('É','E',votegroupe$LibellÈ.du.dÈpartement)
    votegroupe$LibellÈ.du.dÈpartement <- gsub('Ë','E',votegroupe$LibellÈ.du.dÈpartement)
    votegroupe$LibellÈ.du.dÈpartement <- gsub('Ù','O',votegroupe$LibellÈ.du.dÈpartement)
    votegroupe$LibellÈ.du.dÈpartement <- gsub(' ','-',votegroupe$LibellÈ.du.dÈpartement)

    dep = left_join(dep,votegroupe[,c("LibellÈ.du.dÈpartement","Voix_1","Voix_2","Voix_3","Voix_4","Voix_5","Voix_6","Voix_7","Voix_8","Voix_9","Voix_10","Voix_11")],
                    by=c("nom"="LibellÈ.du.dÈpartement"))
  }
  if(annee == 2012){
    votefiltrerDep <- select( vote , 'Libellé.du.département','Nom.1', 'Voix.1','Nom.2', 'Voix.2','Nom.3', 'Voix.3', 'Nom.4', 'Voix.4','Nom.5', 'Voix.5','Nom.6', 'Voix.6','Nom.7', 'Voix.7','Nom.8', 'Voix.8','Nom.9', 'Voix.9')
    
    votegroupe <- votefiltrerDep %>% group_by(Libellé.du.département)  %>% summarize (
      Voix.1 = sum(Voix.1),
      Voix.2 = sum(Voix.2),
      Voix.3 = sum(Voix.3),
      Voix.4 = sum(Voix.4),
      Voix.5 = sum(Voix.5),
      Voix.6 = sum(Voix.6),
      Voix.7 = sum(Voix.7),
      Voix.8 = sum(Voix.8),
      Voix.9 = sum(Voix.9))
    
    votegroupe <- select(votegroupe , 'Libellé.du.département', 'Voix.1', 'Voix.2', 'Voix.3', 'Voix.4', 'Voix.5', 'Voix.6', 'Voix.7', 'Voix.8', 'Voix.9')
  
    votegroupe$Libellé.du.département <- votegroupe$Libellé.du.département %>% str_to_upper()
    votegroupe$Libellé.du.département <- gsub('Ô','O',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('È','E',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('É','E',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('Ë','E',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('Ù','U',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub(' ','-',votegroupe$Libellé.du.département)
    
    dep = left_join(dep,votegroupe[,c("Libellé.du.département","Voix.1","Voix.2","Voix.3","Voix.4","Voix.5","Voix.6","Voix.7","Voix.8","Voix.9")],
                    by=c("nom"="Libellé.du.département"))
  }
  if(annee == 2007){
    votefiltrerDep <- select( vote , 'Libellé.du.département','Nom','Voix','Nom.1', 'Voix.1','Nom.2', 'Voix.2','Nom.3', 'Voix.3', 'Nom.4', 'Voix.4','Nom.5', 'Voix.5','Nom.6', 'Voix.6','Nom.7', 'Voix.7','Nom.8', 'Voix.8','Nom.9', 'Voix.9','Nom.10', 'Voix.10','Nom.11', 'Voix.11')
    
    votegroupe <- votefiltrerDep %>% group_by(Libellé.du.département)  %>% summarize (
      Voix = sum(Voix),
      Voix.1 = sum(Voix.1),
      Voix.2 = sum(Voix.2),
      Voix.3 = sum(Voix.3),
      Voix.4 = sum(Voix.4),
      Voix.5 = sum(Voix.5),
      Voix.6 = sum(Voix.6),
      Voix.7 = sum(Voix.7),
      Voix.8 = sum(Voix.8),
      Voix.9 = sum(Voix.9),
      Voix.10 = sum(Voix.10),
      Voix.11 = sum(Voix.11))
    
    votegroupe <- select(votegroupe , 'Libellé.du.département', 'Voix', 'Voix.1', 'Voix.2', 'Voix.3', 'Voix.4', 'Voix.5', 'Voix.6', 'Voix.7', 'Voix.8', 'Voix.9','Voix.10','Voix.11')
    
    votegroupe$Libellé.du.département <- votegroupe$Libellé.du.département %>% str_to_upper()
    votegroupe$Libellé.du.département <- gsub('Ô','O',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('È','E',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('É','E',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('Ë','E',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub('Ù','U',votegroupe$Libellé.du.département)
    votegroupe$Libellé.du.département <- gsub(' ','-',votegroupe$Libellé.du.département)
    
    dep = left_join(dep,votegroupe[,c("Libellé.du.département","Voix","Voix.1","Voix.2","Voix.3","Voix.4","Voix.5","Voix.6","Voix.7","Voix.8","Voix.9","Voix.10","Voix.11")],
                    by=c("nom"="Libellé.du.département"))
  }
  
  return(dep)
}


calculPourcentage <- function(dep,vote,annee){
  
  if(annee == 2022){
    dep$PourVoix1 = dep$Voix_1/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix2 = dep$Voix_2/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix3 = dep$Voix_3/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix4 = dep$Voix_4/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix5 = dep$Voix_5/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix6 = dep$Voix_6/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix7 = dep$Voix_7/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix8 = dep$Voix_8/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix9 = dep$Voix_9/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix10 = dep$Voix_10/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix11 = dep$Voix_11/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
    dep$PourVoix12 = dep$Voix_12/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11+dep$Voix_12)
  
    for (i in 1:nrow(dep)) {
      if (dep$PourVoix1[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_1[1]
      }
      if (dep$PourVoix2[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_2[1]
      }
      if (dep$PourVoix3[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_3[1]
      }
      if (dep$PourVoix4[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_4[1]
      }
      if (dep$PourVoix5[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_5[1]
      }
      if (dep$PourVoix6[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_6[1]
      }
      if (dep$PourVoix7[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_7[1]
      }
      if (dep$PourVoix8[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_8[1]
      }
      if (dep$PourVoix9[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_9[1]
      }
      if (dep$PourVoix10[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_10[1]
      }
      if (dep$PourVoix11[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_11[1]
      }
      if (dep$PourVoix12[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i],dep$PourVoix12[i])){
        dep$Gagnant[i] = vote$Nom_12[1]
      }
    }
  }
  if(annee == 2017){
    dep$PourVoix1 = dep$Voix_1/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix2 = dep$Voix_2/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix3 = dep$Voix_3/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix4 = dep$Voix_4/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix5 = dep$Voix_5/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix6 = dep$Voix_6/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix7 = dep$Voix_7/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix8 = dep$Voix_8/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix9 = dep$Voix_9/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix10 = dep$Voix_10/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    dep$PourVoix11 = dep$Voix_11/(dep$Voix_1+dep$Voix_2+dep$Voix_3+dep$Voix_4+dep$Voix_5+dep$Voix_6+dep$Voix_7+dep$Voix_8+dep$Voix_9+dep$Voix_10+dep$Voix_11)
    
    for (i in 1:nrow(dep)) {
      if (dep$PourVoix1[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_1[1]
      }
      if (dep$PourVoix2[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_2[1]
      }
      if (dep$PourVoix3[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_3[1]
      }
      if (dep$PourVoix4[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_4[1]
      }
      if (dep$PourVoix5[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_5[1]
      }
      if (dep$PourVoix6[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_6[1]
      }
      if (dep$PourVoix7[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_7[1]
      }
      if (dep$PourVoix8[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_8[1]
      }
      if (dep$PourVoix9[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_9[1]
      }
      if (dep$PourVoix10[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_10[1]
      }
      if (dep$PourVoix11[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom_11[1]
      }
    }
  }
  if(annee == 2012){
    dep$PourVoix1 = dep$Voix.1/(dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9)
    dep$PourVoix2 = dep$Voix.2/(dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9)
    dep$PourVoix3 = dep$Voix.3/(dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9)
    dep$PourVoix4 = dep$Voix.4/(dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9)
    dep$PourVoix5 = dep$Voix.5/(dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9)
    dep$PourVoix6 = dep$Voix.6/(dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9)
    dep$PourVoix7 = dep$Voix.7/(dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9)
    dep$PourVoix8 = dep$Voix.8/(dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9)
    dep$PourVoix9 = dep$Voix.9/(dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9)
    
    for (i in 1:nrow(dep)) {
      if (dep$PourVoix1[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i])){
        dep$Gagnant[i] = vote$Nom.1[1]
      }
      if (dep$PourVoix2[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i])){
        dep$Gagnant[i] = vote$Nom.2[1]
      }
      if (dep$PourVoix3[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i])){
        dep$Gagnant[i] = vote$Nom.3[1]
      }
      if (dep$PourVoix4[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i])){
        dep$Gagnant[i] = vote$Nom.4[1]
      }
      if (dep$PourVoix5[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i])){
        dep$Gagnant[i] = vote$Nom.5[1]
      }
      if (dep$PourVoix6[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i])){
        dep$Gagnant[i] = vote$Nom.6[1]
      }
      if (dep$PourVoix7[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i])){
        dep$Gagnant[i] = vote$Nom.7[1]
      }
      if (dep$PourVoix8[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i])){
        dep$Gagnant[i] = vote$Nom.8[1]
      }
      if (dep$PourVoix9[i] == max(dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i])){
        dep$Gagnant[i] = vote$Nom.9[1]
      }
    }
  }
  if(annee == 2007){
    dep$PourVoix = dep$Voix/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix1 = dep$Voix.1/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix2 = dep$Voix.2/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix3 = dep$Voix.3/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix4 = dep$Voix.4/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix5 = dep$Voix.5/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix6 = dep$Voix.6/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix7 = dep$Voix.7/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix8 = dep$Voix.8/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix9 = dep$Voix.9/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix10 = dep$Voix.10/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    dep$PourVoix11 = dep$Voix.11/(dep$Voix+dep$Voix.1+dep$Voix.2+dep$Voix.3+dep$Voix.4+dep$Voix.5+dep$Voix.6+dep$Voix.7+dep$Voix.8+dep$Voix.9+dep$Voix.10+dep$Voix.11)
    
    for (i in 1:nrow(dep)) {
      if (dep$PourVoix[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom[1]
      }
      if (dep$PourVoix1[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.1[1]
      }
      if (dep$PourVoix2[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.2[1]
      }
      if (dep$PourVoix3[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.3[1]
      }
      if (dep$PourVoix4[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.4[1]
      }
      if (dep$PourVoix5[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.5[1]
      }
      if (dep$PourVoix6[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.6[1]
      }
      if (dep$PourVoix7[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.7[1]
      }
      if (dep$PourVoix8[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.8[1]
      }
      if (dep$PourVoix9[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.9[1]
      }
      if (dep$PourVoix10[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.10[1]
      }
      if (dep$PourVoix11[i] == max(dep$PourVoix[i],dep$PourVoix1[i],dep$PourVoix2[i],dep$PourVoix3[i],dep$PourVoix4[i],dep$PourVoix5[i],dep$PourVoix6[i],dep$PourVoix7[i],dep$PourVoix8[i],dep$PourVoix9[i],dep$PourVoix10[i],dep$PourVoix11[i])){
        dep$Gagnant[i] = vote$Nom.11[1]
      }
    }
  }
  
  return(dep)
}




###################      MAIN     ###################

vote2007 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/2007.csv', sep = ';')
vote2012 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/2012.csv', sep = ';')
vote2017 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/PR17_BVot_T1_FE.csv', sep = ';')
vote2022 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/resultats-par-niveau-burvot-t1-france-entiere.csv', sep = ';')

vote2017$Nom_9 <- gsub('M…LENCHON','MÉLENCHON',vote2017$Nom_9)

dep <- st_read('D:/workspace/ProjetStats-main/DONNÉES/departements-20180101-shp/departements-20180101.shp')

color = c( 'BESANCENOT' = 'brown3' , 'LAGUILLER' = 'brown3' , 'BUFFET' = 'red' , 'MACRON' = 'yellow', 'LE PEN' = 'darkblue' , 'de VILLIERS' = 'coral4' , 'BOVÉ' = 'brown2' , 'ROYAL' = 'pink' , 'VOYNET' = 'green' , 'SARKOZY' = 'blue' ,'SCHIVARDI' =  'darkred', 'BAYROU' = 'yellow' , 'NIHOUS' = 'darkgreen' , 'BLANCS' = 'black', 'ARTHAUD' = 'brown3' ,  'MACRON' = 'yellow' , 'LASSALLE' = 'azure4' , 'ZEMMOUR' = 'coral4' , 'MÉLENCHON' = 'brown2' , 'HIDALGO' = 'pink' , 'JADOT' = 'green' , 'PÉCRESSE' = 'blue' ,'POUTOU' =  'darkred' , 'DUPONT-AIGNAN ' = 'blueviolet' , 'ROUSSEL' = 'red' , 'CHEMINADE' = 'azure4' , 'ZEMMOUR' = 'coral4' , 'MÉLENCHON' = 'brown2' , 'HOLLANDE' = 'pink' , 'JOLY' = 'green' , 'SARKOZY' = 'blue' ,'POUTOU' =  'darkred' , 'DUPONT-AIGNAN' = 'blueviolet' , 'ARTHAUD' = 'brown3' , 'ROUSSEL' = 'red' , 'BAYROU' = 'yellow' , 'CHEMINADE' = 'azure4' , 'ZEMMOUR' = 'coral4' , 'HOLLANDE' = 'pink' , 'JOLY' = 'green' ,'POUTOU' =  'darkred' , 'DUPONT-AIGNAN' = 'blueviolet', 'HAMON' = 'pink' , 'M...LENCHON' = 'brown2' , 'FILLON' = 'blue' , 'ASSELINEAU' = 'gray')

dep2007 <- groupVoteDep(vote2007,dep,2007)
dep2012 <- groupVoteDep(vote2012,dep,2012)
dep2017 <- groupVoteDep(vote2017,dep,2017)
dep2022 <- groupVoteDep(vote2022,dep,2022)

dep2007 <- calculPourcentage(dep2007,vote2007,2007)
dep2012 <- calculPourcentage(dep2012,vote2012,2012)
dep2017 <- calculPourcentage(dep2017,vote2017,2017)
dep2022 <- calculPourcentage(dep2022,vote2022,2022)

ggplot() + geom_sf(data = dep2007, aes(fill = Gagnant)) + scale_fill_manual(values = color)
ggplot() + geom_sf(data = dep2012, aes(fill = Gagnant)) + scale_fill_manual(values = color)
ggplot() + geom_sf(data = dep2017, aes(fill = Gagnant)) + scale_fill_manual(values = color)
ggplot() + geom_sf(data = dep2022, aes(fill = Gagnant)) + scale_fill_manual(values = color)
