libraries <- c(
  "tidyverse", "janitor", "dplyr", "tidyr", "purrr",
  "here", "modeest", "lubridate", "ggplot2",
  "readr", "scales"
)

installed <- libraries %in% installed.packages()[, "Package"]
if (any(!installed)) install.packages(libraries[!installed])

lapply(libraries, library, character.only = TRUE)

# Estructura de carpetas (no rompe si ya existen)
dir.create("data", showWarnings = FALSE)
dir.create("data/raw", showWarnings = FALSE)
dir.create("data/clean", showWarnings = FALSE)
dir.create("data/processed", showWarnings = FALSE)

dir.create("output", showWarnings = FALSE)
dir.create("output/tables", showWarnings = FALSE)
dir.create("output/figures", showWarnings = FALSE)

dir.create("scripts", showWarnings = FALSE)

# Scripts que DEBEN venir en la raíz del proyecto
scripts_esperados <- c(
  "00_setup.R",
  "01_exploracion_de_datos_crudos.R",
  "02_base_marzo_limpia.R",
  "03_datos_procesados.R",
  "04_graficos.R",
  "05_conclusiones.R"
)

# Mapear cada archivo a su carpeta destino
mapa_destino <- list(
  "00_setup.R" = "data/raw",
  "01_exploracion_de_datos_crudos.R" = "data/raw",
  "02_base_marzo_limpia.R" = "data/clean",
  "03_datos_procesados.R" = "data/processed",
  "04_graficos.R" = "data/processed",
  "05_conclusiones.R" = "data/processed"
)

# Crear carpetas si no existen
dirs <- unique(unlist(mapa_destino))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# ---------------------------------------------------
# 🔥 MOVER SCRIPTS SOLO SI ESTÁN EN LA RAÍZ DEL PROYECTO
# ---------------------------------------------------

archivos_en_raiz <- list.files(
  path = ".", 
  pattern = "\\.R$", 
  full.names = TRUE, 
  recursive = FALSE
)

archivos_en_raiz_norm <- normalizePath(archivos_en_raiz)

for (archivo in scripts_esperados) {
  
  camino_relativo <- file.path(".", archivo)
  camino_normalizado <- normalizePath(camino_relativo, mustWork = FALSE)
  
  # Si el archivo existe en la raíz
  if (camino_normalizado %in% archivos_en_raiz_norm) {
    
    destino <- file.path(mapa_destino[[archivo]], archivo)
    
    # Evitar mover si ya estaba en el destino
    if (!file.exists(destino)) {
      file.rename(camino_relativo, destino)
      message("✔ Movido: ", archivo, " → ", destino)
    } else {
      message("✔ Ya estaba en destino: ", archivo)
    }
    
  } else {
    message("⚠ No se encontró en raíz: ", archivo)
  }
}

# --- COPIAR TODOS LOS SCRIPTS A /scripts ---
origen_raw       <- list.files("data/raw",       pattern="\\.R$", full.names=TRUE)
origen_clean     <- list.files("data/clean",     pattern="\\.R$", full.names=TRUE)
origen_processed <- list.files("data/processed", pattern="\\.R$", full.names=TRUE)

todos <- c(origen_raw, origen_clean, origen_processed)
file.copy(todos, "scripts", overwrite = TRUE)

# --- DESCARGA DEL ZIP GCBA ---
options(download.file.method = "libcurl")

url_gcba <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ausa/flujo-vehicular-por-unidades-de-peaje-ausa/flujo-vehicular-2024.zip"

zip_destino <- here("data", "raw", "flujo_vehicular_2024.zip")
carpeta_destino <- here("data", "raw", "bases_mensuales_crudas")

if (!file.exists(zip_destino)) {
  message("⬇ Descargando datos desde GCBA…")
  download.file(url_gcba, destfile = zip_destino, mode = "wb")
}

if (!dir.exists(carpeta_destino)) dir.create(carpeta_destino)

if (length(list.files(carpeta_destino)) == 0) {
  message("📦 Descomprimiendo ZIP…")
  unzip(zip_destino, exdir = carpeta_destino)
}

message("✔ Datos crudos disponibles en: data/raw/bases_mensuales_crudas")

writeLines("# Proyecto de análisis de peajes\n\nEstructura estandarizada.", 
           "README.md")
