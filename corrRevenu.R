library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(mapsf)
library(tidyverse)
library(ICSNP)

groupVoteRevenu <- function(vote,revenu,annee){
  
  if(annee == 2017){
    votefiltrerRev <- select( vote , 'LibellÈ.du.dÈpartement', 'Blancs','Abstentions','Nuls','Nom_1', 'Voix_1','Nom_2', 'Voix_2','Nom_3', 'Voix_3', 'Nom_4', 'Voix_4','Nom_5', 'Voix_5','Nom_6', 'Voix_6','Nom_7', 'Voix_7','Nom_8', 'Voix_8','Nom_9', 'Voix_9','Nom_10', 'Voix_10','Nom_11','Voix_11')
    
    votegroupe <- votefiltrerRev %>% group_by(LibellÈ.du.dÈpartement)  %>% summarize (
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
    revenu$Nom <- gsub(' ','-',revenu$Nom)
    
    revenu <- select(revenu, 'Nom', 'Taux.Revenu.Fiscal')
    
    revenu <- revenu[-102,]
    revenu <- revenu[-102,]
    
    revenu = left_join(revenu,votegroupe[,c("LibellÈ.du.dÈpartement","Voix_1","Voix_2","Voix_3","Voix_4","Voix_5","Voix_6","Voix_7","Voix_8","Voix_9","Voix_10","Voix_11")],
                    by=c("Nom"="LibellÈ.du.dÈpartement"))
  }
  if(annee == 2012){
    votefiltrerRev <- select( vote , 'Libellé.du.département','Nom.1', 'Voix.1','Nom.2', 'Voix.2','Nom.3', 'Voix.3', 'Nom.4', 'Voix.4','Nom.5', 'Voix.5','Nom.6', 'Voix.6','Nom.7', 'Voix.7','Nom.8', 'Voix.8','Nom.9', 'Voix.9')
    
    votegroupe <- votefiltrerRev %>% group_by(Libellé.du.département)  %>% summarize (
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
    revenu$Nom <- gsub('CORSE-DU-SUD','CORSE SUD',revenu$Nom)
    revenu$Nom <- gsub(' ','-',revenu$Nom)
    
    revenu <- select(revenu, 'Nom', 'Taux.Revenu.Fiscal')
    
    revenu <- revenu[-101,]
    revenu <- revenu[-101,]
    
    revenu = left_join(revenu,votegroupe[,c("Libellé.du.département","Voix.1","Voix.2","Voix.3","Voix.4","Voix.5","Voix.6","Voix.7","Voix.8","Voix.9")],
                    by=c("Nom"="Libellé.du.département"))
  }
  
  return(revenu)
}


correlationVoteRev <- function(vote,revenu,annee){
  if(annee == 2017){
    cor1 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_1)
    cor2 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_2)
    cor3 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_3)
    cor4 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_4)
    cor5 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_5)
    cor6 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_6)
    cor7 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_7)
    cor8 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_8)
    cor9 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_9)
    cor10 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_10)
    cor11 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix_11)
    
    Candidats <- c(vote[1,'Nom_1'],vote[1,'Nom_2'],vote[1,'Nom_3'],vote[1,'Nom_4'],vote[1,'Nom_5'],vote[1,'Nom_6'],vote[1,'Nom_7'],vote[1,'Nom_8'],vote[1,'Nom_9'],vote[1,'Nom_10'],vote[1,'Nom_11'])
    
    CorrelationRevenuDepartementNbVote <- c(cor1$estimate,cor2$estimate,cor3$estimate,cor4$estimate,cor5$estimate,cor6$estimate,cor7$estimate,cor8$estimate,cor9$estimate,cor10$estimate,cor11$estimate)
    
    corr <- data.frame(Candidats,CorrelationRevenuDepartementNbVote)
  }
  if(annee == 2012){
    cor1 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix.1)
    cor2 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix.2)
    cor3 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix.3)
    cor4 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix.4)
    cor5 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix.5)
    cor6 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix.6)
    cor7 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix.7)
    cor8 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix.8)
    cor9 = cor.test(revenu$Taux.Revenu.Fiscal,revenu$Voix.9)
    
    
    Candidats <- c(vote[1,'Nom.1'],vote[1,'Nom.2'],vote[1,'Nom.3'],vote[1,'Nom.4'],vote[1,'Nom.5'],vote[1,'Nom.6'],vote[1,'Nom.7'],vote[1,'Nom.8'],vote[1,'Nom.9'])
    
    CorrelationRevenuDepartementNbVote <- c(cor1$estimate,cor2$estimate,cor3$estimate,cor4$estimate,cor5$estimate,cor6$estimate,cor7$estimate,cor8$estimate,cor9$estimate)
    
    corr <- data.frame(Candidats,CorrelationRevenuDepartementNbVote)
  }
  return(corr)
}





##############     MAIN     ###############

vote2012 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/2012.csv', sep = ';')
vote2017 <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/PR17_BVot_T1_FE.csv', sep = ';')

revenu <- read.csv('D:/workspace/ProjetStats-main/DONNÉES/impotrevenudep-1984-2020.csv', sep = ';')

revenu$Nbre.foyers.fiscaux.tot <- gsub(' ','',revenu$Nbre.foyers.fiscaux.tot)
revenu$Revenu.fiscal.référence.tot <- gsub(' ','',revenu$Revenu.fiscal.référence.tot)

revenu$Taux.Revenu.Fiscal <- as.numeric(revenu$Revenu.fiscal.référence.tot)/as.numeric(revenu$Nbre.foyers.fiscaux.tot)

revenu2012 <- revenu[revenu$Année.de.revenus=="2012",]
revenu2017 <- revenu[revenu$Année.de.revenus=="2017",]

revenu2012 <- groupVoteRevenu(vote2012,revenu2012,2012)
revenu2017 <- groupVoteRevenu(vote2017,revenu2017,2017)

corr2012 <- correlationVoteRev(vote2012,revenu2012,2012)
corr2017 <- correlationVoteRev(vote2017,revenu2017,2017)
