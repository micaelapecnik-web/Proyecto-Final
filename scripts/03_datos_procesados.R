ruta_mayo_limpio <- "output/tables/Transito_Mayo_2024_limpio.csv"

mayo <- read_csv(ruta_mayo_limpio, show_col_types = FALSE)

#La hipotesis que se desea analizar se basa en, si existen diferencias sistemáticas 
#en el tipo de vehículos que circulan por el peaje durante días hábiles y fines de semana.

unique(mayo$categoria_cobrada)

#Para analizar nuestra hipotesis, debemos crear una nueva columna en donde diferenciemos
#a los vehiculos en 2 categorias unicamente: "Pesados" y "Livianos o particulares"

mayo_tipo_vehiculo <- mayo |>
  mutate(
    tipo_vehiculo = case_when(
      categoria_cobrada %in% c(
        "Auto", "Auto con trailer", "Moto", "Pago doble Moto"
      ) ~ "Livianos o particulares",
      
      categoria_cobrada %in% c(
        "Pesados 2 Ejes", "Pesados 3 Ejes", "Pesados 4 Ejes",
        "Pesados 5 Ejes", "Pesados 6 Ejes"
      ) ~ "Pesados",
      
      TRUE ~ NA_character_))

#Tambien tenemos que agregar una columna que detalle el dia de la semana, para ver si
#corresponde a un dia habil o no

mayo_dia <- mayo_tipo_vehiculo |> 
  mutate(dia_semana = wday(fecha_operativa,
                      label = TRUE,
                      abbr = FALSE,
                      week_start = 1))

feriados <- as.Date(c("2024-05-01", "2024-05-25"))

mayo_final <- mayo_dia %>%
  mutate(
    tipo_dia = case_when(
      fecha_operativa %in% feriados ~ "no habil",                 
      dia_semana %in% c("Sábado", "Domingo") ~ "no habil",       
      TRUE ~ "habil"))

resumen <- mayo_final %>%
  group_by(tipo_dia, tipo_vehiculo) %>%
  summarise(total_pasajes = sum(pasos, na.rm = TRUE)) %>%
  ungroup()

resumen

#Grafico de barras comparativo

resumen_clean <- resumen %>% 
  filter(!is.na(tipo_vehiculo))

valores_colores <- c(
  "Livianos o particulares" = "#e74c3c", 
  "Pesados"                 = "#a93226"   )
g_grafico_barras <- ggplot(resumen_clean, 
                           aes(x = tipo_dia, y = total_pasajes, fill = tipo_vehiculo)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = valores_colores) +
  scale_y_continuous(labels = scales::comma) +   
  labs(
    title = "Flujo de vehículos por tipo de día",
    x = "Tipo de día",
    y = "Total de pasos",
    fill = "Tipo de vehículo",
    caption = "Fuente: Peaje - Base Mayo 2024"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11))

ggsave("output/figures/grafico_barras_tipo_dia.jpeg",
       g_grafico_barras,
       width = 7, height = 5, dpi = 300)


# Test de hipótesis (Chi-cuadrado)
# El test de Chi-cuadrado evaluará si la distribución de vehículos (Pesados vs Livianos) 
# difiere significativamente entre días hábiles y no hábiles

tabla <- table(mayo_final$tipo_vehiculo, mayo_final$tipo_dia)
chi_result <- chisq.test(tabla)
chi_result

# Como el p-value es mucho menor que 0.05, rechazamos la hipótesis nula.
# Conclusión: Existe evidencia estadísticamente significativa de que la proporción 
# de vehículos pesados y livianos varía entre días hábiles y fines de semana/feriados, 
# confirmando la hipótesis alternativa.


# ANOVA
# Realizamos un ANOVA factorial para evaluar cómo varía el número de pasos diarios
# según tipo de día (hábil vs no hábil) y tipo de vehículo (Pesados vs Livianos), 
# incluyendo la interacción entre ambos factores.

#Con muestras grandes (como tus ~236.000 observaciones), 
#el ANOVA es bastante robusto a desviaciones de normalidad y homogeneidad de varianzas

anova_result <- aov(pasos ~ tipo_dia * tipo_vehiculo, data = mayo_final)
summary(anova_result)

# Resultados:
# - tipo_dia: F = 148.9, p < 2e-16 → el tipo de día tiene un efecto significativo sobre los pasos diarios.
# - tipo_vehiculo: F = 947.9, p < 2e-16 → el tipo de vehículo también tiene un efecto significativo.
# - tipo_dia:tipo_vehiculo (interacción): F = 218.1, p < 2e-16 → existe interacción significativa 
#entre tipo de día y tipo de vehículo.

# Conclusión:
# 1. La cantidad de pasos diarios varía significativamente según si es día hábil o no hábil.
# 2. La cantidad de pasos diarios varía significativamente entre vehículos Pesados y Livianos.
# 3. Además, la diferencia entre Pesados y Livianos depende del tipo de día (interacción significativa),
#    indicando que, por ejemplo, los vehículos Pesados predominan más en días hábiles mientras que 
#    los Livianos predominan más en fines de semana y feriados.

ruta_salida <- "output/tables/Transito_Mayo_2024_final.csv"
write_csv(mayo_final, ruta_salida)
