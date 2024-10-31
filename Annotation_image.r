# Annotation de l' Image de Carte Hydrologique en 3D

# Installer les packages nécessaires
install.packages("MetBrewer")

# Charger les bibliothèques nécessaires
library(magick)
library(MetBrewer)
library(colorspace)
library(ggplot2)
library(glue)
library(stringr)

# Lire l'image de la carte 3D du Bénin
img <- image_read("C:/Users/Utilisateur/Downloads/Rivières_du_Bénin/Essaie1/benin-3d-elevation-rivers.png")

# Définir les couleurs pour les annotations
colors <- met.brewer("OKeeffe2")
text_color <- darken(colors[7], .25)

# Créer les répertoires manquants
output_dir <- "C:/Users/Utilisateur/Downloads/Rivières_du_Bénin/Essaie2/"
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

# Titre sur deux lignes
title <- "Carte hydrologique \ndes Rivières du Bénin"

# Annoter l'image
img |> 
  image_annotate(title, 
                 gravity = "northwest", location = "+50+50", 
                 color = text_color, size = 150, weight = 700, font = "Arial") |> 
   
  image_annotate(glue("© 2024 Genséric AGUIDISSOU | ", 
                      "Données: HydroRIVERS Database"), 
                 gravity = "south", location = "+0+50", 
                 font = "Arial", color = alpha(text_color, .5), size = 80) |> 
  image_write(paste0(output_dir, "Carte_hydrologique_des_Rivières_du_Bénin.png"))
