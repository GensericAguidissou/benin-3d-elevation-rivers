# Projet visualisation des rivières du Bénin en version 3D

# ÉTAPE 1 : INSTALLATION ET CHARGEMENT DES LIBRAIRIES

# Installer et charger les packages nécessaires
install.packages("pacman")  # Installer le package 'pacman' si ce n'est pas déjà fait
pacman::p_load(
    terra,    # Pour la manipulation des données raster
    elevatr,  # Pour télécharger les données d'élévation
    sf,       # Pour la manipulation des données spatiales vectorielles
    geodata,  # Pour télécharger les frontières administratives
    tidyverse, # Pour la manipulation des données et les opérations dplyr
    rayshader # Pour la visualisation en 3D
)


# ÉTAPE 2 : TÉLÉCHARGER LES FRONTIÈRES DU BÉNIN

# Définir le chemin de travail actuel
path <- getwd()

# Télécharger les frontières du Bénin et convertir en objet sf
country_sf <- geodata::gadm(
    country = "BEN",    # Code ISO pour le Bénin
    level = 0,          # Niveau de détail (0 pour les frontières nationales)
    path = path         # Chemin où les données seront sauvegardées
) |> 
    sf::st_as_sf()

# Vérification des données des frontières administratives
print(country_sf)
class(country_sf)
str(country_sf)
plot(sf::st_geometry(country_sf))
head(country_sf)


# ÉTAPE 3 : TÉLÉCHARGER LES RIVIÈRES

# URL du fichier zip des rivières
url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_af_shp.zip"
destfile <- basename(url)  # Nom du fichier de destination

# Télécharger et décompresser si le fichier n'existe pas déjà
if (!file.exists(destfile)) {
    download.file(url = url, destfile = destfile, mode = "wb")  # Téléchargement du fichier zip
    unzip(destfile)  # Décompression du fichier zip
}


# Etape 4 : CHARGER ET FILTRER LES RIVIÈRES

# Charger les données de rivières
filename <- list.files(
    path = "HydroRIVERS_v10_af_shp", 
    pattern = ".shp", 
    full.names = TRUE
)

rivers_sf <- sf::st_read(filename[1])
print(rivers_sf)

# Définir la bounding box du Bénin
bbox_benin <- sf::st_bbox(country_sf)
print(bbox_benin)  # xmin: 0.774345, ymin: 6.234910, xmax: 3.851701, ymax: 12.418351

# Créer un WKT (Well-Known Text) de la bounding box du Bénin
bbox_wkt_benin <- "POLYGON((
    0.774345 6.234910, 
    0.774345 12.418351, 
    3.851701 12.418351, 
    3.851701 6.234910, 
    0.774345 6.234910
))"

# Désactiver le traitement S2 pour éviter les erreurs géométriques
sf::sf_use_s2(FALSE)

# Filtrer les rivières avec les frontières du Bénin en utilisant le WKT
rivers_benin <- sf::st_read(filename[1], wkt_filter = bbox_wkt_benin) |> 
    sf::st_intersection(country_sf)

# Vérifier le nombre de rivières et afficher un aperçu
print(nrow(rivers_benin))  # Nombre de rivières dans le Bénin
plot(sf::st_geometry(rivers_benin))  # Visualiser les rivières dans le Bénin



# ÉTAPE 5 : CALCULER LA LARGEUR DES RIVIÈRES ET TRANSFORMER LE CRS
unique_ord_flow <- sort(unique(rivers_benin$ORD_FLOW))
print(unique_ord_flow)

crs_country <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"

country_river_width <- rivers_benin |> 
    dplyr::mutate(
        width = as.numeric(ORD_FLOW),  # Convertir ORD_FLOW en numérique
        width = dplyr::case_when(      # Assigner des largeurs en fonction de ORD_FLOW
            width == 3 ~ 16, 
            width == 4 ~ 14,
            width == 5 ~ 12,
            width == 6 ~ 10,
            width == 7 ~ 6,
            width == 8 ~ 4,
            TRUE ~ 0
        )
    ) |> 
    sf::st_as_sf() |>  # Convertir en objet sf
    sf::st_transform(crs = crs_country)  # Transformer dans le CRS approprié

print(country_river_width)
plot(sf::st_geometry(country_river_width))


# ÉTAPE 6 : TÉLÉCHARGEMENT ET TRAITEMENT DES DONNÉES D'ÉLÉVATION (DEM)
dem <- elevatr::get_elev_raster(
    locations = country_sf, 
    z = 9, clip = "locations"
)
dem_country <- dem |> 
    terra::rast() |> 
    terra::project(crs_country)
dem_matrix <- rayshader::raster_to_matrix(dem_country)

print(dim(dem_matrix))


# ÉTAPE 7 : RENDU DE LA SCÈNE 3D

dem_matrix |> 
    rayshader::height_shade(
        texture = colorRampPalette(
            c("#fcc69f", "#c67847")
        )(128)
    ) |> 
    rayshader::add_overlay(
        rayshader::generate_line_overlay(
            geometry = country_river_width,
            extent = dem_country,
            heightmap = dem_matrix,
            color = "#387B9C",
            linewidth = country_river_width$width,
            data_column_width = "width"
        ), alphalayer = 1
    ) |> 
    rayshader::plot_3d(
        dem_matrix,
        zscale = 20,
        solid = TRUE,
        shadow = TRUE,
        shadow_darkness = 1,  # Ombre plus douce
        background = "white",
        windowsize = c(1000, 1000),  # Résolution encore plus élevée
        zoom = .5,  # Zoom pour un bon compromis entre vue large et détails
        phi = 89,   # Angle pour un éclairage réaliste
        theta = 0
    )

rayshader::render_camera(
    zoom = .80  # Vue optimale pour le Bénin avec plus de proximité
)


# Etape 8

# Définition de l'URL pour l'éclairage HDR
u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(u)

# Téléchargement du fichier HDR
download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

# Chemin pour enregistrer l'image de rendu final
file_name <- "C:/Users/Utilisateur/Downloads/Rivières_du_Bénin/Benin-3d-elevation-rivers.png"

# Rendu en haute qualité
rayshader::render_highquality(
    filename = file_name,
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity_env = 1.5,  # Intensité lumineuse plus élevée pour le Bénin
    interactive = FALSE,
    width = 4000,  # Haute résolution pour capturer plus de détails
    height = 4000  # Haute résolution pour capturer plus de détails
)
