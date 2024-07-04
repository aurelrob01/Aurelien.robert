#Je charge le chemin d'accès aux fichiers

setwd("W:\\3_PROJETS\\ATLAS_21\\INSEE\\dossier_complet_31_12_2022")

#chargement des librairies

library(dplyr)
library(readr)

#lecture du fichier csv

data1 <- read.csv("dossier_complet.csv", header = TRUE,  ";")

#sélection des colonnes pour notre indicateur 

data_indice <- subset(data1, select = c("CODGEO", "C19_FAMMONO", "P19_NSCOL15P_BEPC", "P19_CHOMEUR1564", "P19_POP6074", "P19_POP7589", "P19_POP", "C19_FAM", "P19_NSCOL15P", "P19_ACT1564"))

#import du fichier Filosofi 2019 qui contient les revenus médians des communes 

data2 <- read.csv("cc_filosofi_2019_COM.csv", header = TRUE, sep = ";")

# Remplacer les occurrences de "s" par NA

data2$MED19[data2$MED19 == "s"] <- NA

#sélection de la colonne des revenus médians 

data_revenu <- data2[, c(1,4)]

#extraction de la colonne revenu

extr_revenu <- data_revenu$MED19

#fusion à data_indice

data_indice <- cbind(data_indice, revenu_median = extr_revenu)

# Exemple : Convertir la colonne "MaColonne" en numérique, en évitant les valeurs non numériques

data_indice$revenu_median <- ifelse(is.na(as.numeric(data_indice$revenu_median)), NA, as.numeric(data_indice$revenu_median))

data_indice$revenu_median[is.na(data_indice$revenu_median)] <- median(data_indice$revenu_median, na.rm = TRUE)

#arrondir les décimales 

data_indice <- round(data_indice, 1)

#somme des âges 

data_indice$somme_age <- data_indice$P19_POP6074 + data_indice$P19_POP7589

#Calcul des différents taux  

    data_indice$population <- (data_indice$somme_age / data_indice$P19_POP) * 100 
    data_indice$taux_famille_mono <- (data_indice$C19_FAMMONO / data_indice$C19_FAM) * 100
    data_indice$taux_non_formation <- (data_indice$P19_NSCOL15P_BEPC / data_indice$P19_NSCOL15P) * 100
    data_indice$taux_chomeur <- (data_indice$P19_CHOMEUR1564 / data_indice$P19_ACT1564) * 100

# Supprimer les colonnes "P19_POP6074", "P19_POP7589", "P19_POP" et "age"

colonnes_a_supprimer <- c("P19_POP6074", "P19_POP7589", "P19_POP", "age", "somme_age","C19_FAM", "C19_FAMMONO", "P19_NSCOL15P_BEPC", "P19_NSCOL15P", "P19_ACT1564", "P19_CHOMEUR1564", "geometry")
data_indice <- data_indice[, -which(names(data_indice) %in% colonnes_a_supprimer)]
data_indice <- round(data_indice, 1)
#renommer les colonnes pour plus de clarté 

names(data_indice)[1:6] <- c("CodeGeo", "Revenu_med", "Taux_plus60ans", "Taux_famille_mono", "Taux_non_formation", "Taux_chômage")

#ajouter les donnée immobilières

data_social <- read.csv("ensemble du parc social.csv", header = TRUE, sep = ";")
data_parc_total <-read.csv("parc total.csv", header = TRUE, sep = ";")

#joindre les deux tables grâce au champ "CodGeo"

jointure_logement <- left_join(data_parc_total, data_social, by = "CodGeo")

#suppression des colonnes superflues 

colonnes_a_supprimer1 <- c("Libellé", "LIB")
jointure_logement <- jointure_logement[, -which(names(jointure_logement) %in% colonnes_a_supprimer1)]

#calcul du taux de logements sociaux 

jointure_logement$taux_logement_social <- (jointure_logement$Ensemble.du.parc.social / jointure_logement$P19_LOG * 100)

#suppression des colonnes inutiles 

colonnes_a_supprimer2 <- c("P19_LOG", "Ensemble.du.parc.social")
jointure_logement <- jointure_logement[, -which(names(jointure_logement) %in% colonnes_a_supprimer2)]
jointure_logement <- round(jointure_logement, 1)

#ajouter cette donnée au fichier "data_indice" pour avoir nos données complètes 

names(jointure_logement)[1] <- c("CodeGeo")

data_indice_jointe <- left_join(data_indice, jointure_logement, by = "CodeGeo") %>%
  mutate(taux_logement_social = ifelse(is.na(taux_logement_social), 0, taux_logement_social))



#sauver les tables en csv 

setwd("C:\\Users\\aurelien.robert\\Documents\\Aurélien")
write.csv(data_indice, file = "C:\\Users\\aurelien.robert\\Documents\\Aurélien\\data_indice.csv")
write.csv(jointure_logement, file = "C:\\Users\\aurelien.robert\\Documents\\Aurélien\\jointure_logement.csv")
write.csv(data_indice_jointe, file = "W:\\3_PROJETS\\ATLAS_21\\INSEE\\dossier_complet_31_12_2022\\data_indices_complets")

#test de création du z score 
# Variables à considérer

col_names <- names(data_indice)

###variable du revenu médian###

indicateurs <- c("Revenu_med")

# Calcul des scores Z pour chaque indicateur
data_indice_jointe <- data_indice_jointe %>%
  mutate(across(all_of(indicateurs), scale, .names = "Z_revenu_med"))

###variable du taux de plus 60 ans###

indicateurs <- c("Taux_plus60ans")

# Calcul des scores Z pour chaque indicateur
#data_indice_jointe <- data_indice_jointe %>%
  #mutate(across(all_of(indicateurs), -scale, .names = "Z_plus60ans"))

data_indice_jointe <- data_indice_jointe %>%
  mutate(across(all_of(indicateurs), scale, .names = "Z_plus60ans"))

###variable du taux de famille monoparentale

indicateurs <- c("Taux_famille_mono")

# Calcul des scores Z pour chaque indicateur
data_indice_jointe <- data_indice_jointe %>%
  mutate(across(all_of(indicateurs), scale, .names = "Z_famille_mono"))

#test calcul z score avec formule 

###variable avec le taux de non formation 

indicateurs <- c("Taux_non_formation")

# Calcul des scores Z pour chaque indicateur
data_indice_jointe <- data_indice_jointe %>%
  mutate(across(all_of(indicateurs), scale, .names = "Z_non_formation"))

###variable avec le taux de logement social###

indicateurs <- c("taux_logement_social")

# Calcul des scores Z pour chaque indicateur
data_indice_jointe <- data_indice_jointe %>%
  mutate(across(all_of(indicateurs), scale, .names = "Z_logement_social"))

#Calcul des score Z pour le chomage 

indicateurs <- c("Taux_chômage")
data_indice_jointe <- data_indice_jointe %>%
  mutate(across(all_of(indicateurs), scale, .names = "Z_chômage"))


data_indice_jointe <- round(data_indice_jointe, 2)

#Multiplication des indices par -1

indice <- data_indice_jointe

#Remplacement des NA par la médiane 

indice$Z_revenu_med[is.na(indice$Z_revenu_med)] <- median(indice$Z_revenu_med, na.rm = TRUE)

#Normalisation des indices 

indice$Z_plus60ans <- (indice$Z_plus60ans - min(indice$Z_plus60ans)) / (max(indice$Z_plus60ans) - min(indice$Z_plus60ans))
indice$Z_revenu_med <- ((indice$Z_revenu_med - min(indice$Z_revenu_med)) / (max(indice$Z_revenu_med) - min(indice$Z_revenu_med)))
indice$Z_famille_mono <- ((indice$Z_famille_mono - min(indice$Z_famille_mono)) / (max(indice$Z_famille_mono) - min(indice$Z_famille_mono)))
indice$Z_non_formation <- (indice$Z_non_formation - min(indice$Z_non_formation)) / (max(indice$Z_non_formation) - min(indice$Z_non_formation))
indice$Z_logement_social <- (indice$Z_logement_social  - min(indice$Z_logement_social )) / (max(indice$Z_logement_social ) - min(indice$Z_logement_social ))
indice$Z_chômage <- (indice$Z_chômage  - min(indice$Z_chômage )) / (max(indice$Z_chômage ) - min(indice$Z_chômage ))
#Multiplication des indices par -1

indice$Z_plus60ans <- 1 - indice$Z_plus60ans 
indice$Z_famille_mono <- 1 - indice$Z_famille_mono 
indice$Z_non_formation <- 1 - indice$Z_non_formation  
indice$Z_logement_social <- 1 - indice$Z_logement_social 
indice$Z_chômage <- 1 - indice$Z_chômage

indice <- round(indice, 2)

#addition de chaque z score dans une nouvelle colonne 

indice$indice_de_fragilité_sociale <- indice$Z_revenu_med + indice$Z_plus60ans + indice$Z_famille_mono + indice$Z_non_formation + indice$Z_logement_social + indice$Z_chômage
#indice$indice_de_fragilité_sociale <- (indice$indice_de_fragilité_sociale  - min(indice$indice_de_fragilité_sociale )) / (max(indice$indice_de_fragilité_sociale ) - min(indice$indice_de_fragilité_sociale ))

#quantile plot 

install.packages("car")
library(car)

qqPlot(indice$indice_de_fragilité_sociale)
hist(indice$indice_de_fragilité_sociale, main = "Histogramme des scores z finaux", xlab = "Score z final")

#test de standardisation robuste
#indice$indice_de_fragilité_sociale <- scale(indice$indice_de_fragilité_sociale, center = FALSE, scale = 1)
#indice$indice_de_fragilité_sociale <- (indice$indice_de_fragilité_sociale  - min(indice$indice_de_fragilité_sociale )) / (max(indice$indice_de_fragilité_sociale ) - min(indice$indice_de_fragilité_sociale ))

#indice$indice_de_fragilité_sociale  <- round(indice$indice_de_fragilité_sociale, 2)

# Exemple d'histogramme
#hist(indice$indice_de_fragilité_sociale, main = "Histogramme des scores z finaux", xlab = "Score z final")

write.csv(indice, file = "W:\\3_PROJETS\\ATLAS_21\\INSEE\\dossier_complet_31_12_2022\\indice_de_fragilité_sociale.csv")



###### CARTOGRAPHIE ######

Sys.setenv("HTTP_PROXY" = "http://pfrie-std.proxy.e2.rie.gouv.fr:8080")
Sys.setenv("HTTPS_PROXY" = "http://pfrie-std.proxy.e2.rie.gouv.fr:8080")


library(ggplot2)
install.packages((sf))
library(sf)


#lecture de la couche shapefile des communes 

communes <- st_read(dsn = "N:\\BDCARTO_V5\\ADMINISTRATIF", layer = 'commune_s_d21')

#On vire les colonnes qui ne nous sont pas utiles 

#colonnes_a_supprimer <- c("Id_GéoFLA", "Code_Commu", "Statut", "Abscisse_C", "Ordonnée_", "Abscisse_1", "Ordonné_1", "Altitude_M", "Code_Canto", "Code_Arron","Code_Dépa", "Nom_Dépar", "Code8r2GI", "Nom_Régio", "EXTRACTION", "RECETTE", "geometry")
#communes <- communes[, -which(names(communes) %in% colonnes_a_supprimer)]

#On joint notre indice de fragilité à la couche commune 

names(indice)[1] <- c("INSEE_COM")
communes$INSEE_COM <- as.double(communes$INSEE_COM)

communes_jointure_indice <- left_join(x = communes, y = indice, by = c("INSEE_COM"))

communes_jointure_indice$indice_de_fragilité_sociale[is.na(communes_jointure_indice$indice_de_fragilité_sociale)] <- median(communes_jointure_indice$indice_de_fragilité_sociale, na.rm = TRUE)

#communes_jointure <- left_join(communes, indice, by = "INSEE_Comm") %>%
  #mutate(taux_logement_social = ifelse(is.na(taux_logement_social), 0, taux_logement_social))

# cartographie 

install.packages("RColorBrewer")
library(RColorBrewer)

ggplot(data = communes_jointure_indice) +
  geom_sf(aes(fill = indice_de_fragilité_sociale)) +
  scale_fill_gradientn(colors = brewer.pal(11, "Spectral"))

top_n(communes_jointure_indice, 10, indice_de_fragilité_sociale)
commune1000 <- subset(select(communes_jointure_indice,indice_de_fragilité_sociale, NOM, POPULATION), communes_jointure_indice$POPULATION > 1000)
topn10 <- top_n(commune1000,10, indice_de_fragilité_sociale)

top_n(communes_jointure_indice, -10, indice_de_fragilité_sociale)

#quantile population 

qqPlot(communes_jointure_indice$POPULATION)

### enregistrement de la table pour export sur qgis ###

path_sortie='W:\\3_PROJETS\\ATLAS_21\\INSEE\\dossier_complet_31_12_2022\\'
nom='Indice_fragilité_sociale.shp'
path_final=paste(path_sortie,nom)
path_final

st_write(communes_jointure_indice, path_final)

write.csv(communes_jointure_indice, file = "W:\\3_PROJETS\\ATLAS_21\\INSEE\\dossier_complet_31_12_2022\\indice_de_fragilité_final.csv")



### tests statistiques : on entre en zone radioactive !!! ###
### Iere partie : les bloxplots ####

######valeur age######

boxplot(communes_jointure_indice$Taux_plus60ans)
AgeOUT <- boxplot.stats(communes_jointure_indice$Taux_plus60ans)$out 
communes_jointure_indice[,c(24)]

colonnes_a_supprimer <- c("geometry")
communes_jointure_indice <- communes_jointure_indice[, -which(names(communes_jointure_indice) %in% colonnes_a_supprimer)]

#for (i in 21:25) {
  #print(i)
  #nom_col = paste (colnames(communes_jointure_indice[,c(i)]),"_OUT")
  #print(nom_col)}

###boucle : j'ai remplacé le df 'communes_jointure_indice' par l'indice de base (data_indice_jointe) sans les communes car il contenait le champ geometry ####


#je copie le dataframe pour pouvoir manipuler les colonnes à ma guise 

indice_copie <- data_indice_jointe

#remove les colonnes superflues 

colonnes_a_supprimer <- c("Z_revenu_med", "Z_plus60ans", "Z_famille_mono", "Z_non_formation", "Z_logement_social", "Z_chômage", "_OUT")
indice_copie <- indice_copie[, -which(names(indice_copie) %in% colonnes_a_supprimer)]





for (i in 2:7) {
  print(i)
  nom_col = paste (colnames(indice_copie[,c(i)]),"_OUT")
  print(nom_col)
  varOUT <- boxplot.stats(indice_copie$nom_col)$out 
  print(varOUT)
  CASE1 <- case_when(
    indice_copie$nom_col %in% varOUT ~ 1,
    TRUE = 0)
  print(CASE1)
  indice_copie <- mutate(indice_copie, !!nom_col := CASE1)
}

for (i in 2:7) {
  print(i)
  nom_col <- paste(colnames(indice_copie[, c(i)]), "_OUT")
  print(nom_col)
  varOUT <- boxplot.stats(indice_copie[[nom_col]])$out
  print(varOUT)
  
  new_col <- paste(colnames(indice_copie[, c(i)]), "_FLAG_OUT")
  
  indice_copie <- mutate(indice_copie, !!new_col := case_when(
    indice_copie[[nom_col]] %in% varOUT ~ 1,
    TRUE ~ 0
  ))
}


for (i in 2:7) {
  nom_col <- colnames(indice_copie)[i]
  print(nom_col)
  
  varOUT <- boxplot.stats(indice_copie[[nom_col]])$out
  print(varOUT)
  
  new_col <- paste(nom_col, "_OUT", sep = "")
  
  indice_copie <- indice_copie %>%
    mutate(!!new_col := ifelse(indice_copie[[nom_col]] %in% varOUT, 1, 0))
}

indice_copie$OUT_TOTAL <- indice_copie$Revenu_med_OUT + indice_copie$Taux_plus60ans_OUT + indice_copie$Taux_famille_mono_OUT + indice_copie$Taux_non_formation_OUT + indice_copie$Taux_chômage_OUT + indice_copie$taux_logement_social_OUT

names(indice_copie)[1] <- c("INSEE_COM")
communes$INSEE_COM <- as.double(communes$INSEE_COM)

indice_copie <- left_join(x = communes, y = indice_copie, by = c("INSEE_COM"))

ggplot(data = indice_copie) +
  geom_sf(aes(fill = OUT_TOTAL)) +
  scale_fill_gradientn(colors = brewer.pal(11, "YlGn"))

top_n(indice_copie, 10, OUT_TOTAL)

#jointure avec indice 


#normalisation des champs avant export 

colonnes_a_supprimer <- c(" _OUT", " _FLAG_OUT", "Revenu_med_FLAG_OUT", "Taux_plus60ans_FLAG_OUT",  "Taux_famille_mono_FLAG_OUT", "Taux_non_formation_FLAG_OUT", "Taux_chômage_FLAG_OUT", "taux_logement_social_FLAG_OUT")
indice_copie <- indice_copie[, -which(names(indice_copie) %in% colonnes_a_supprimer)]


#export de l'OUT en shapefile 

path_sortie='W:\\3_PROJETS\\ATLAS_21\\INSEE\\dossier_complet_31_12_2022\\'
nom='OUT.shp'
path_final=paste(path_sortie,nom)

st_write(indice_copie, path_final)


#communes_jointure_indice <- mutate(communes_jointure_indice, plus60ansOUT = which(communes_jointure_indice$Taux_plus60ans %in% c(AgeOUT)))
