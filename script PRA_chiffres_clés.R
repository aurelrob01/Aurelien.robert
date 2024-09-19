Sys.setenv("HTTP_PROXY" = "http://pfrie-std.proxy.e2.rie.gouv.fr:8080")
Sys.setenv("HTTPS_PROXY" = "http://pfrie-std.proxy.e2.rie.gouv.fr:8080")
install.packages("cowplot")

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(Cairo)
library(RColorBrewer)
library(gridExtra)
library(cowplot)

chiffres_cles_auxois <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\Auxois\\Chiffres_cles.csv", sep = ",", header = TRUE)
chiffres_cles_auxois[,2:4] <- round(chiffres_cles_auxois[,2:4], digits = 2)

chiffres_cles_viticole <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\cote_viticole\\Chiffres clés.csv", sep = ",", header = TRUE)
chiffres_cles_viticole <- chiffres_cles_viticole[,-2]
chiffres_cles_viticole[,2:4] <- round(chiffres_cles_viticole[,2:4], digits = 2)

chiffres_cles_plaine <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\la plaine\\Chiffres clés.csv", sep = ",", header = TRUE)
chiffres_cles_plaine <- chiffres_cles_plaine[,-2]
chiffres_cles_plaine[,2:4] <- round(chiffres_cles_plaine[,2:4], digits = 2)

chiffres_cles_vallee <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\la vallée\\Chiffres clés.csv", sep = ",", header = TRUE)
chiffres_cles_vallee <- chiffres_cles_vallee[,-2]
chiffres_cles_vallee[,2:4] <- round(chiffres_cles_vallee[,2:4], digits = 2)

chiffres_cles_morvan <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\morvan\\Chiffres clés.csv", sep = ",", header = TRUE)
chiffres_cles_morvan <- chiffres_cles_morvan[,-2]
chiffres_cles_morvan[,2:4] <- round(chiffres_cles_morvan[,2:4], digits = 2)

chiffres_cles_plateau <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\plateau langrois\\Chiffres clés.csv", sep = ",", header = TRUE)
chiffres_cles_plateau <- chiffres_cles_plateau[,-2]
chiffres_cles_plateau[,2:4] <- round(chiffres_cles_plateau[,2:4], digits = 2)

chiffres_cles_tonnerois <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\tonnerois\\Chiffres clés.csv", sep = ",", header = TRUE)
chiffres_cles_tonnerois <- chiffres_cles_tonnerois[,-2]
chiffres_cles_tonnerois[,2:4] <- round(chiffres_cles_tonnerois[,2:4], digits = 2)

chiffres_cles_val_de_saone <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\val de saone\\Chiffres clés.csv", sep = ",", header = TRUE)
chiffres_cles_val_de_saone <- chiffres_cles_val_de_saone[,-2]
chiffres_cles_val_de_saone[,2:4] <- round(chiffres_cles_val_de_saone[,2:4], digits = 2)

chiffres_cles_vingeanne <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\vingeanne\\Chiffres clés.csv", sep = ",", header = TRUE)
chiffres_cles_vingeanne <- chiffres_cles_vingeanne[,-2]
chiffres_cles_vingeanne[,2:4] <- round(chiffres_cles_vingeanne[,2:4], digits = 2)


departement <- data.frame(col1 = chiffres_cles_auxois$nom, col2 = 0, col3 = 0, col4 = 0)

departement$col2 <- chiffres_cles_auxois$X2010 + chiffres_cles_viticole$X2010 + chiffres_cles_plaine$X2010 + chiffres_cles_vallee$X2010 + chiffres_cles_morvan$X2010 + chiffres_cles_plateau$X2010 + chiffres_cles_tonnerois$X2010 + chiffres_cles_val_de_saone$X2010 + chiffres_cles_vingeanne$X2010
departement$col3 <- chiffres_cles_auxois$X2020 + chiffres_cles_viticole$X2020 + chiffres_cles_plaine$X2020 + chiffres_cles_vallee$X2020 + chiffres_cles_morvan$X2020 + chiffres_cles_plateau$X2020 + chiffres_cles_tonnerois$X2020 + chiffres_cles_val_de_saone$X2020 + chiffres_cles_vingeanne$X2020
departement$col4 <- chiffres_cles_auxois$evolution + chiffres_cles_viticole$evolution  + chiffres_cles_plaine$evolution  + chiffres_cles_vallee$evolution  + chiffres_cles_morvan$evolution  + chiffres_cles_plateau$evolution  + chiffres_cles_tonnerois$evolution  + chiffres_cles_val_de_saone$evolution + chiffres_cles_vingeanne$evolution 


graphi <- data.frame(PRA = c("Auxois", "Côte Viticole", "Plaine", "Vallée", "Morvan","Plateau", "Tonnerois", "Val de Saone", "Vingeanne"),
                     EXPLOIT_2010 = c(1118,1229,957,155,229,751,57,291,106),
                     EXPLOIT_2020 = c(850,1168,755,135,170,673,58,231,98))
graphi

graphi_long <- reshape2::melt(graphi, id.vars = "PRA", variable.name = "Annee", value.name = "NB_EXPLOITATIONS")
graphi_long <- graphi_long %>%
  arrange(desc(NB_EXPLOITATIONS))

graphi_long$PRA <- factor(graphi_long$PRA, levels = unique(graphi_long$PRA))

setwd("W:\\3_PROJETS\\ATLAS_21\\Agriculture")
CairoSVG("graphique_nb_exploit.svg", width = 10, height = 6)

ggplot(graphi_long, aes(x = PRA, y = NB_EXPLOITATIONS, fill = Annee)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") +
  theme_minimal() +
  labs(title = "Nombre d'exploitations par PRA et par année",
       x = "Petite région agricole",
       y = "Nombre d'exploitations",
       fill = "Année") + 
  scale_fill_manual(values = c("#FFDB00", "#379777"), labels = c("2010", "2020"))

dev.off()


### evolution de la SAU moyenne ###

graphi_sau_moyenne <- data.frame(Année = c("1970", "1979", "1988", "2000", "2010", "2020"),
                                 SAU_moy_21 = c(34, 45.7, 55.2, 74, 93.5, 111.7),
                                 SAU_moy_FR = c(19, 23,28,42,55,69))

CairoSVG("graph_SAU_Moyenne.svg", width = 10, height = 6)

ggplot(data = graphi_sau_moyenne, aes(x = Année, group = 1)) +
  geom_line(aes(y = SAU_moy_21), size = 1.5, color = "#379777") +  
  geom_line(aes(y = SAU_moy_FR), size = 1.5, color = "#E76F51") +  
  geom_point(aes(y = SAU_moy_21), size = 4, color = "#379777", fill = "white", shape = 21, stroke = 1.5) +  
  geom_point(aes(y = SAU_moy_FR), size = 4, color = "#E76F51", fill = "white", shape = 21, stroke = 1.5) + 
  geom_text(aes(y = SAU_moy_21, label = SAU_moy_21), vjust = -1.5, size = 3.5, color = "#379777", fontface ="bold") +  
  geom_text(aes(y = SAU_moy_FR, label = SAU_moy_FR), vjust = -1.5, size = 3.5, color = "#E76F51", fontface = "bold") +  
  theme_minimal() +
  labs(title = "Évolution de la SAU moyenne de Côte d'Or",
       x = "Année",
       y = "SAU moyenne (hectares)",
       fill = "Légende") + 
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8)
  )
dev.off()


### EVOLUTION BIO ###

graphi_bio <- data.frame(PRA = c("Auxois", "Côte viticole", "Plaine", "Vallée", "Morvan", "Plateau", "Tonnerois", "Val de Saône", "Vingeanne"),
                         bio_2010 = c(33,105,41,5,4,40,0,14,5),
                         bio_2020 = c(96,252,99,19,17,139,5,16,12))

graphi_long_bio <- reshape2::melt(graphi_bio, id.vars = "PRA", variable.name = "Annee", value.name = "BIO")
graphi_long_bio <- graphi_long_bio %>%
  arrange(desc(BIO))

graphi_long_bio$PRA <- factor(graphi_long_bio$PRA, levels = unique(graphi_long_bio$PRA))

CairoSVG("graphique_bio.svg", width = 10, height = 6)

ggplot(graphi_long_bio, aes(x = PRA, y = BIO, fill = Annee)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") +
  theme_minimal() +
  labs(title = "Nombre d'exploitations bio en 2010 et 2020",
       x = "Petite région agricole",
       y = "Nombre d'exploitations en bio",
       fill = "Année") + 
  scale_fill_manual(values = c("#9BEC00", "#379777"), labels = c("2010", "2020"))

dev.off()


### otex_pra ####

otex_auxois <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\Auxois\\otex.csv", sep = ",", header = TRUE)
otex_auxois[otex_auxois == -999] <- NA

otex_viticole <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\cote_viticole\\otex.csv", sep = ",", header = TRUE)
otex_viticole[otex_viticole == -999] <- NA

otex_plaine <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\la plaine\\otex.csv", sep = ",", header = TRUE)
otex_plaine[otex_plaine == -999] <- NA

otex_vallee<- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\la vallée\\otex.csv", sep = ",", header = TRUE)
otex_vallee[otex_vallee == -999] <- NA

otex_morvan <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\morvan\\otex.csv", sep = ",", header = TRUE)
otex_morvan[otex_morvan == -999] <- NA

otex_plateau <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\plateau langrois\\otex.csv", sep = ",", header = TRUE)
otex_plateau[otex_plateau == -999] <- NA

otex_tonnerois <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\tonnerois\\otex.csv", sep = ",", header = TRUE)
otex_tonnerois [otex_tonnerois  == -999] <- NA

otex_val_de_saone <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\val de saone\\otex.csv", sep = ",", header = TRUE)
otex_val_de_saone [otex_val_de_saone  == -999] <- NA

otex_vingeanne <- read.csv("W:\\3_PROJETS\\ATLAS_21\\Agriculture\\Petites régions agricoles\\vingeanne\\otex.csv", sep = ",", header = TRUE)
otex_vingeanne [otex_vingeanne  == -999] <- NA

colors <- c("#37B7C3", "#F4CE14", "#FCDC94", "#A5DD9B", "#97BE5A", "#0A6847", "#F97300", "#DEAC80",
            "#9AC8CD", "#D895DA", "#D37676", "#615EFC", "#B3A398", "#BBE2EC", "#BF3131", "#FFF78A")

otex_auxois$n_exploit[is.na(otex_auxois$n_exploit)] <- 0

otex_auxois <- otex_auxois %>%#A5DD9B
  mutate(fraction = n_exploit / sum(n_exploit),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax,n = -1)))


donut_auxois <- ggplot(otex_auxois, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = lib_oteft)) + 
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2,4)) + 
  theme_void() + 
  theme(legend.position = "right")+
  scale_fill_manual(values = colors)
donut_auxois
###

otex_morvan$n_exploit[is.na(otex_morvan$n_exploit)] <- 0
  
otex_morvan <- otex_morvan %>%
  mutate(fraction = n_exploit / sum(n_exploit),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax,n = -1)))

otex_morvan$cut <- "Morvan"

donut_morvan <- ggplot(otex_morvan, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = lib_oteft)) + 
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2,4)) + 
  theme_void() + 
  theme(legend.position = "none")+
  scale_fill_manual(values = colors) + 
  facet_wrap(~cut)

grid.arrange(donut_auxois, donut_morvan, layout_matrix = rbind(c(1,2)))

data_list <- list(otex_auxois, otex_morvan, otex_plaine, otex_vallee, otex_viticole, otex_plateau, otex_tonnerois, otex_val_de_saone, otex_vingeanne)
donut_list <- list()

unique_values <- unique(unlist(lapply(data_list, function(data_list) unique(data_list$lib_oteft))))
color_mapping <- setNames(colors[1:length(unique_values)], unique_values)

for (i in seq_along(data_list)) {
  data_list[[i]]$n_exploit[is.na(data_list[[i]]$n_exploit)] <- 0 
  data_list[[i]] <- data_list[[i]] %>%
    mutate(fraction = n_exploit / sum(n_exploit),
           ymax = cumsum(fraction),
           ymin = c(0, head(ymax, n = -1)))
  donut_list[[i]] <- ggplot(data_list[[i]], aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = lib_oteft)) +
    geom_rect() +
    coord_polar(theta = "y")+
    xlim(c(2,4))+
    theme_void()+
    theme(legend.position = "bottom",
      legend.key.size = unit(0.5, "cm"))+
    scale_fill_manual(values = color_mapping, drop = FALSE)
}

plot_grid(plotlist = donut_list, ncol = 3, align = "hv", axis = "lbr", labels = c("Auxois", "Morvan", "Plaine", "Vallée","Côte viticole", "Plateau", "Tonnerois", "Val de Saône", "Vingeanne"))

###


data_list_combined <- lapply(seq_along(data_list), function(i) {
  data_list[[i]]$group <- names(data_list)[i]
  return(data_list[[i]])
}) %>% bind_rows()

data_list_combined$lib_oteft <- factor(data_list_combined$lib_oteft, levels = unique(data_list_combined$lib_oteft))

gg <- ggplot(data_list_combined, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = lib_oteft)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right",  # Position de la légende à droite
        legend.key.size = unit(0.5, "cm"),  # Taille de l'icône de la légende
        legend.title = element_blank()) +  # Supprimer le titre de la légende
  scale_fill_manual(values = color_mapping[[1]], drop = FALSE) +  # Utiliser la première correspondance de couleurs
  facet_wrap(~ group, ncol = 3,scales = "free")  # Diviser en panneaux en fonction de la colonne group

plot_grid(plotlist = donut_list, ncol = 3, align = "hv", axis = "lbr", 
          labels = c("Auxois", "Morvan", "Plaine", "Vallée", "Côte viticole", 
                     "Plateau", "Tonnerois", "Val de Saône", "Vingeanne"),
          hjust = -0.5, vjust = 1.5)

### bon code####
data_list_combined <- bind_rows(data_list, .id = "group") %>%
  mutate(lib_oteft = factor(lib_oteft, levels = unique(unlist(lapply(data_list, function(df) unique(df$lib_oteft))))))

CairoSVG("graph_otex_pra.svg", width = 12, height = 10)

# Création du graphique ggplot avec facet_wrap
gg <- ggplot(data_list_combined, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = lib_oteft)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right",  # Position de la légende à droite
        legend.key.size = unit(0.5, "cm"),  # Taille de l'icône de la légende
        legend.title = element_blank()) +  # Supprimer le titre de la légende
  scale_fill_manual(values = colors) +  # Utiliser votre palette de couleurs
  facet_wrap(~ group, ncol = 3, scales = "free")  # Diviser en panneaux en fonction de la colonne group
gg
dev.off()

install.packages("rmarkdown")
install.packages("knitr")
install.packages("pagedown")
install.packages("bookdown")
