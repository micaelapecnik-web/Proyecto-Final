ruta_datos <- "output/tables/Transito_Mayo_2024_final.csv"
mayo_final <- read_csv(ruta_datos, show_col_types = FALSE)

test_dias <- mayo_final %>%
  filter(!is.na(tipo_dia)) %>%
  group_by(fecha_operativa, tipo_dia) %>%
  summarise(total_dia = sum(pasos, na.rm = TRUE), .groups = "drop")


#Test de diferencias entre hábiles y fines de semana
#Usamos un test no paramétrico (Wilcoxon), porque los datos no son normales casi nunca

wilcox_res <- wilcox.test(
  total_dia ~ tipo_dia,
  data = test_dias,
  exact = FALSE
)

print("===== RESULTADO TEST WILCOXON =====")
print(wilcox_res)

#Histograma – Distribución del flujo diario

histograma_total <- test_dias %>%
  ggplot(aes(x = total_dia)) +
  geom_histogram(
    binwidth = 500,
    fill = "#b2182b",
    color = "white",
    alpha = 0.9
  ) +
  theme_minimal(base_size = 10) +
  labs(
    title = "Distribución del flujo diario total de vehículos",
    subtitle = "Mayo 2024 – Peaje Buenos Aires–La Plata",
    x = "Total de vehículos por día",
    y = "Frecuencia",
    caption = "Fuente: Sistema de Tránsito – Dataset procesado por el autor")

histograma_total

ggsave(
  "output/figures/histograma_total.jpeg",
  histograma_total,
  width = 7,
  height = 5,
  dpi = 300)

#CONCLUSIONES FINALES
#Los resultados del estudio permiten evaluar la hipótesis planteada sobre 
#la existencia de diferencias sistemáticas en el tipo de vehículos que circulan 
#por el peaje entre días hábiles y no hábiles. En términos descriptivos, se observó 
#que los días hábiles muestran un mayor flujo absoluto de vehículos, especialmente 
#en la categoría de pesados, mientras que los fines de semana presentan un predominio 
#relativo de vehículos livianos. Estos patrones son coherentes con dinámicas 
#esperables de movilidad laboral y recreativa.

#Sin embargo, al analizar el flujo total de vehículos por día mediante un test 
#no paramétrico de Wilcoxon, no se encontraron diferencias estadísticamente 
#significativas entre días hábiles y no hábiles (p = 0.129). Esto indica que, 
#cuando se considera el tránsito diario en su conjunto, la variabilidad dentro 
#de cada grupo es suficientemente amplia como para que las diferencias observadas 
#no sean concluyentes en términos inferenciales. Por el contrario, 
#el análisis por tipo de vehículo —incluyendo pruebas de chi-cuadrado y 
#ANOVA— sí mostró patrones diferenciales relevantes, reflejando que las variaciones 
#en la composición del tránsito dependen tanto del tipo de día como del tipo de vehículo.

#El estudio presenta algunas limitaciones importantes. Por un lado, se trabajó 
#exclusivamente con datos correspondientes a un único mes, lo cual impide generalizar 
#los resultados a dinámicas anuales o estacionales. Además, ciertas observaciones con 
#información faltante debieron eliminarse, lo que reduce parcialmente el tamaño 
#muestral. Adicionalmente, la falta de normalidad y la heterogeneidad de varianzas 
#limitaron el tipo de pruebas estadísticas disponibles.

#A pesar de estas restricciones, los resultados poseen implicancias prácticas 
#relevantes. La identificación de diferencias en la composición vehicular 
#según el tipo de día puede aportar a la planificación operativa de peajes, 
#asignación de recursos y gestión de infraestructura vial. 
#Comprender qué tipo de vehículos circulan y cuándo puede contribuir a una toma 
#de decisiones más eficiente.

#Finalmente, se sugieren futuras líneas de investigación que permitan profundizar 
#el análisis: incorporar múltiples meses o años para evaluar tendencias estacionales; 
#desagregar por franja horaria para detectar picos de congestión; comparar distintos 
#peajes o corredores viales; y aplicar modelos predictivos que permitan anticipar 
#comportamientos de la demanda.


