ruta_mayo <- here("data", "raw", "bases_mensuales_crudas", "Transito Mayo 2024.csv")

mayo_raw <- read_delim(
  ruta_mayo,
  delim = ";",
  locale = locale(encoding = "Latin1"),
  col_types = cols(.default = "c"),
  na = c("", "N/D", "ND", "n/d", "na", "NA"),
  show_col_types = FALSE)

mayo_raw <- mayo_raw |> clean_names()

mayo <- mayo_raw |>
  mutate(
    fecha_operativa = dmy(mes_da_a_aa_o_de_fecha_operativa),
    hora = as.integer(hora_hh),
    categoria_cobrada = cat_cobrada,
    pasos = as.numeric(pasos)
  ) |>
  select(
    fecha_operativa,
    hora,
    categoria_cobrada,
    id_peaje,
    observacion,
    sentido,
    tipo_cobro,
    pasos)

mayo[mayo == ""] <- NA

cols_numericas <- mayo |> select(where(is.numeric)) |> colnames()

outlier_report <- list()

for (col in cols_numericas) {
  
  Q1 <- quantile(mayo[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(mayo[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  
  cantidad_outliers <- sum(
    mayo[[col]] < lim_inf | mayo[[col]] > lim_sup,
    na.rm = TRUE)
  
  outlier_report[[col]] <- cantidad_outliers}

outlier_report <- tibble(
  variable = names(outlier_report),
  outliers_detectados = unlist(outlier_report))

print("=== REPORTE DE OUTLIERS DETECTADOS POR VARIABLE (ANTES DE IMPUTAR) ===")
print(outlier_report)


for (col in cols_numericas) {
  
  Q1 <- quantile(mayo[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(mayo[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  
  mediana <- median(mayo[[col]], na.rm = TRUE)
  
  mayo[[col]] <- ifelse(
    mayo[[col]] < lim_inf | mayo[[col]] > lim_sup,
    mediana,
    mayo[[col]])}

#Los outliers fueron detectados mediante el método del rango intercuartílico (IQR)
#y reemplazados por la mediana en cada variable numérica. La imputación con mediana
#es adecuada debido a que las variables no presentan una distribución normal.


total_na <- sum(is.na(mayo))
total_na

na_por_columna <- colSums(is.na(mayo))
na_por_columna

pct_na <- total_na / (nrow(mayo) * ncol(mayo)) * 100
pct_na

#Aunque la base presenta valores faltantes, no se imputaron con métodos estadísticos
#porque las variables afectadas corresponden mayormente a categorías cualitativas
#(como tipo de cobro, sentido o categoría cobrada), donde completar con medias o modas
#implicaría introducir información artificial que nunca existió en los registros.
#Además, la fuente de los valores faltantes no puede determinarse con certeza,
#por lo que imputarlos podría sesgar la interpretación de los resultados.
#Dado que el análisis posterior puede manejar NA sin inconvenientes (por ejemplo
#mediante el uso de na.rm = TRUE), se decidió conservar los valores faltantes para
#preservar la integridad y trazabilidad de los datos originales.

print("=== RESUMEN DE LA BASE DE MAYO LIMPIA (OUTLIERS IMPUTADOS POR MEDIANA) ===")
print(summary(mayo))
print("Cantidad de filas:")
print(nrow(mayo))

write_csv(
  mayo,
  here("output", "tables", "Transito_Mayo_2024_limpio.csv"))

#ESTADISTICA DESCRIPTIVA PARA LA BASE DE MAYO

#Tendencia central y dispersión (variables numéricas)

stats_numericas <- mayo |>
  summarise(
    media_pasos   = mean(pasos, na.rm = TRUE),
    mediana_pasos = median(pasos, na.rm = TRUE),
    moda_pasos    = mfv(pasos, na_rm = TRUE)[1],
    sd_pasos      = sd(pasos, na.rm = TRUE),
    iqr_pasos     = IQR(pasos, na.rm = TRUE))

print("=== ESTADÍSTICAS DESCRIPTIVAS DE 'pasos' ===")
print(stats_numericas)

#Distribución de frecuencias (variables categóricas)

cat_vars <- mayo |> select(where(is.character)) |> colnames()

frecuencias <- list()

for (v in cat_vars) {
  frecuencias[[v]] <- mayo |> count(.data[[v]], sort = TRUE)}

print("=== FRECUENCIAS DE VARIABLES CATEGÓRICAS ===")
frecuencias

#Histograma de la variable pasos

g_hist <- ggplot(mayo, aes(x = pasos)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribución de pasos",
    x = "Cantidad de pasos",
    y = "Frecuencia")

print(g_hist)

ggsave(
  filename = here("output", "figures", "hist_pasos.jpg"),
  plot = g_hist,
  width = 8, height = 5, dpi = 300)


#Boxplot general de la variable pasos 

g_box <- ggplot(mayo, aes(y = pasos)) +
  geom_boxplot(fill = "tomato", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Boxplot general de pasos",
    y = "Cantidad de pasos")

print(g_box)

ggsave(
  filename = here("output", "figures", "boxplot_general_pasos.jpg"),
  plot = g_box,
  width = 6, height = 5, dpi = 300)

