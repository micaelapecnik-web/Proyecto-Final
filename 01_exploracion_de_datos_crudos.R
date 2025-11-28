leer_archivo_peaje <- function(path) {
  df <- try(
    read_delim(
      path,
      delim = ";",
      locale = locale(encoding = guess_encoding(path)$encoding[1]),
      col_types = cols(.default = "c"),
      na = c("", "N/D", "ND", "n/d", "na", "NA"),
      show_col_types = FALSE),
    silent = TRUE)
  if (inherits(df, "try-error") || ncol(df) == 1) {
    df <- read_delim(
      path,
      delim = ",",
      locale = locale(encoding = guess_encoding(path)$encoding[1]),
      col_types = cols(.default = "c"),
      na = c("", "N/D", "ND", "n/d", "na", "NA"),
      show_col_types = FALSE)}
  df <- df |> clean_names()
  df <- df |> rename(
    fecha_operativa = matches("fecha|mes"),
    hora = matches("hora"),
    categoria_cobrada = matches("cat"),
    id_peaje = matches("id"),
    observacion = matches("observ"),
    sentido = matches("sentido"),
    tipo_cobro = matches("cobro"),
    pasos = matches("pas"))
  df$archivo <- basename(path)
  return(df)}

explorar_datos <- function(df, nombre_base) {df <- df |> mutate(across(everything(), ~na_if(.,"N/D")))
  faltantes <- df |> summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(),
                 names_to="variable",
                 values_to="faltantes")
  constantes <- df |> summarise(across(everything(), ~n_distinct(.,na.rm=TRUE))) %>%
    pivot_longer(everything(),
                 names_to="variable",
                 values_to="n_distintos") %>%
    filter(n_distintos <= 1)
  df_num <- df |> select(where(is.numeric))
   if (ncol(df_num) == 0) {
    descriptivos <- tibble()
    outliers <- tibble()
    correlaciones <- matrix(nrow=0, ncol=0)
  } else {descriptivos <- df_num |> summarise(across(
      everything(),
      list(
        media=~mean(.x,na.rm=TRUE),
        mediana=~median(.x,na.rm=TRUE),
        sd=~sd(.x,na.rm=TRUE),
        min=~min(.x,na.rm=TRUE),
        max=~max(.x,na.rm=TRUE)),
      .names="{.col}_{.fn}"))
    detectar_outliers <- function(x){
      Q1 <- quantile(x,0.25,na.rm=TRUE)
      Q3 <- quantile(x,0.75,na.rm=TRUE)
      IQR <- Q3-Q1
      sum(x < (Q1 - 1.5*IQR) | x > (Q3 + 1.5*IQR), na.rm=TRUE)}
    
    outliers <- df_num |> summarise(across(everything(), detectar_outliers))
   correlaciones <- cor(df_num, use="pairwise.complete.obs")}
  
  list(
    base = nombre_base,
    faltantes = faltantes,
    constantes = constantes,
    descriptivos = descriptivos,
    outliers = outliers,
    correlaciones = correlaciones)}
files <- list.files(
  here("data", "raw", "bases_mensuales_crudas"),
  full.names = TRUE,
  pattern = "\\.csv$")

resultados <- map(
  files,
  ~ explorar_datos(
    df = leer_archivo_peaje(.x),
    nombre_base = basename(.x)))

names(resultados) <- basename(files)

#ESTADÍSTICA DESCRIPTIVA GLOBAL DE LOS DATOS CRUDOS

crudo_todo <- map_dfr(files, leer_archivo_peaje)
crudo_todo <- crudo_todo %>%
  mutate(
    pasos = as.numeric(pasos),
    hora = as.numeric(hora))

#Estadística descriptiva para columnas numéricas
crudo_descriptivos <- crudo_todo %>%
  select(where(is.numeric)) %>%
  summarise(across(
    everything(),
    list(
      media = ~mean(.x, na.rm = TRUE),
      mediana = ~median(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE),
      iqr = ~IQR(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"))

#Frecuencias para columnas categóricas
crudo_frecuencias <- crudo_todo %>%
  select(where(is.character)) %>%
  map_df(~ tibble(
    categoria = names(table(.x, useNA = "ifany")),
    frecuencia = as.numeric(table(.x, useNA = "ifany"))
  ), .id = "variable")

cat("\n===== ESTADÍSTICAS DESCRIPTIVAS (NUMÉRICAS) =====\n")
print(crudo_descriptivos)
write_csv(
  crudo_descriptivos,
  here("output", "tables", "estadisticas_descriptivas_crudo.csv"))


cat("\n===== FRECUENCIAS DE VARIABLES CATEGÓRICAS =====\n")
print(crudo_frecuencias)

crear_resumen_mes <- function(resultados) {
resumen <- map_dfr(resultados, function(res) {
    tibble(
      mes = res$base,
      total_NA = sum(res$faltantes$faltantes),
      constantes = nrow(res$constantes),
      outliers = if (is.null(res$outliers)) 0 else sum(res$outliers, na.rm = TRUE))})
  
  normalizar <- function(x) {
    if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
      return(rep(0, length(x)))   # todo igual → 0
    } else {
      return((x - min(x)) / (max(x) - min(x)))}}
  
  resumen <- resumen %>%
    mutate(
      norm_NA = normalizar(total_NA),
      norm_const = normalizar(constantes),
      norm_out = normalizar(outliers),
      score = 1 - rowMeans(across(starts_with("norm")))
    ) %>%
    arrange(desc(score))
  
  return(resumen)}

tabla_final <- crear_resumen_mes(resultados)
tabla_final
write_csv(
  tabla_final,
  here("output", "tables", "tabla_final_scoring_meses.csv"))

#Justificación de la elección del mes a analizar:
#Aunque Enero obtuvo el mejor puntaje de calidad de datos (menos NA y mayor consistencia),
#no fue seleccionado por presentar una fuerte estacionalidad propia de la temporada turística,
#lo que genera picos de tráfico no representativos del comportamiento habitual.
#Para evitar meses con feriados atípicos o patrones anómalos (Enero, Julio, Diciembre),
#se priorizó Mayo, que combina:
#- alta calidad de datos (2° mejor score),
#- menor cantidad de feriados largos,
#- flujo vehicular y laboral estable,
#- mayor representatividad del tránsito "normal".
#Por ello, Mayo es el mes más adecuado para construir el análisis sin demasiado ruido.

