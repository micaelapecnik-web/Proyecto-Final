mayo_final <- read_csv("output/tables/Transito_Mayo_2024_final.csv",
                       show_col_types = FALSE)

#Promedio diario por tipo de vehículo y tipo de día

mayo_clean <- mayo_final %>% 
  filter(
    tipo_vehiculo %in% c("Livianos o particulares", "Pesados"),
    tipo_dia %in% c("habil", "no habil")
  ) %>%
  mutate(
    tipo_vehiculo = case_when(
      tipo_vehiculo == "Livianos o particulares" ~ "Liviano",
      tipo_vehiculo == "Pesados" ~ "Pesado",
      TRUE ~ NA_character_
    ),
    tipo_dia = case_when(
      tipo_dia == "habil" ~ "Hábil",
      tipo_dia == "no habil" ~ "Finde",
      TRUE ~ NA_character_))

paleta_roja <- c("Liviano" = "#e74c3c", "Pesado" = "#a93226")
tam_texto <- 10

if (!dir.exists("output/figures")) {
  dir.create("output/figures", recursive = TRUE)
}

df_diario <- mayo_clean %>%
  group_by(fecha_operativa, tipo_vehiculo, tipo_dia) %>%
  summarise(pasos_totales = sum(pasos), .groups = "drop")

grafico1 <- ggplot(df_diario, aes(x = fecha_operativa,
                                  y = pasos_totales,
                                  color = tipo_vehiculo)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = paleta_roja) +
  labs(
    title = "Evolución diaria del tránsito por tipo de vehículo",
    subtitle = "Comparación entre días hábiles y fines de semana",
    x = "Fecha",
    y = "Cantidad de pasos diarios",
    color = "Tipo de vehículo",
    caption = "Fuente: Peaje – Base mayo 2024"
  ) +
  theme_minimal(base_size = tam_texto) +
  theme(
    plot.title = element_text(size = tam_texto + 4),
    plot.subtitle = element_text(size = tam_texto),
    axis.text = element_text(size = tam_texto),
    legend.text = element_text(size = tam_texto),
    legend.title = element_text(size = tam_texto))

ggsave("output/figures/grafico_lineas_promedio.jpeg",
       grafico1, width = 9, height = 5, dpi = 300)

#Distribución de pasos por categoría (boxplot)

df_resumen <- mayo_clean %>%
  group_by(fecha_operativa, tipo_vehiculo, tipo_dia) %>%
  summarise(pasos_totales = sum(pasos), .groups = "drop")

grafico2 <- ggplot(df_resumen,
                   aes(x = tipo_dia, y = pasos_totales, fill = tipo_vehiculo)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.4) +
  scale_fill_manual(values = paleta_roja) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribución del tránsito por tipo de día",
    subtitle = "Comparación entre días hábiles y fines de semana",
    x = "Tipo de día",
    y = "Cantidad diaria de pasos",
    fill = "Tipo de vehículo",
    caption = "Fuente: Peaje – Base mayo 2024"
  ) +
  theme_minimal(base_size = tam_texto) +
  theme(
    plot.title = element_text(size = tam_texto + 4),
    plot.subtitle = element_text(size = tam_texto),
    axis.text = element_text(size = tam_texto),
    legend.text = element_text(size = tam_texto),
    legend.title = element_text(size = tam_texto))

ggsave("output/figures/grafico_boxplot_trafico.jpeg",
       grafico2, width = 7, height = 5, dpi = 300)


