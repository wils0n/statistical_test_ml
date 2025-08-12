# =============================================================================
# ANÁLISIS ESTADÍSTICO COMPARATIVO DE MODELOS DE PREDICCIÓN
# =============================================================================
# 
# PROPÓSITO:
# Este script compara el rendimiento de 7 modelos diferentes de machine learning
# usando la métrica RMSE (Root Mean Square Error). Se realizan pruebas estadísticas
# para determinar si hay diferencias significativas entre los modelos.
#
# DATOS:
# - 7 modelos evaluados en validación cruzada de 10-folds
# - Métrica: RMSE (valores más bajos = mejor rendimiento)
# - 10 observaciones por modelo (k = 0 a 9)
#
# ANÁLISIS INCLUIDOS:
# 1. Pruebas de normalidad (Shapiro-Wilk)
# 2. Homogeneidad de varianzas (Levene)
# 3. Comparación múltiple (Friedman)
# 4. Comparaciones pareadas (Wilcoxon con corrección Bonferroni)
# 5. Visualizaciones (Q-Q plots y boxplots)
#
# AUTOR: Wilson Julca Mejía
# =============================================================================

# INSTALACIÓN DE PAQUETES NECESARIOS
# Solo ejecutar la primera vez o si los paquetes no están instalados
install.packages("xtable")      # Para tablas en formato LaTeX
install.packages("car")         # Para prueba de Levene
install.packages("PMCMRplus")   # Para pruebas post-hoc avanzadas
install.packages("ggpubr")      # Para gráficos estadísticos elegantes
install.packages("rstatix")     # Para pruebas estadísticas modernas
install.packages("knitr")       # Para formateo de tablas
install.packages("tidyverse")   # Para manipulación de datos

# CARGAR LIBRERÍAS NECESARIAS
library(PMCMRplus)   # Pruebas post-hoc no paramétricas
library(tidyverse)   # Manipulación y visualización de datos
library(ggpubr)      # Gráficos estadísticos profesionales
library(rstatix)     # Pruebas estadísticas modernas
library(dplyr)       # Manipulación de datos (parte de tidyverse)
library(xtable)      # Exportar tablas a LaTeX
library(car)         # Pruebas de homogeneidad de varianzas
library(knitr)       # Formateo de tablas para reportes
library(tidyverse)   # Confirmar carga completa

# =============================================================================
# PREPARACIÓN DE DATOS
# =============================================================================

# DATOS EMBEBIDOS: Resultados RMSE de 7 modelos en validación cruzada 10-fold
# 
# ESTRUCTURA DE LOS DATOS:
# - k: Número de fold (0 a 9) - cada modelo se evalúa 10 veces
# - Model: Nombre del modelo (Modelo 1 a Modelo 7)
# - RMSE: Error cuadrático medio (menor valor = mejor rendimiento)
#
# MODELOS REPRESENTADOS (ejemplos):
# - Modelo 1: Regresión Ridge (regularización L2)
# - Modelo 2: Random Forest (ensemble de árboles)
# - Modelo 3: Árbol de Decisión (modelo simple)
# - Modelo 4: ARIMA (serie temporal autoregresiva)
# - Modelo 5: Holt-Winters (suavizado exponencial)
# - Modelo 6: LSTM (red neuronal recurrente)
# - Modelo 7: CNN (red neuronal convolucional)

base <- data.frame(
  # Índice de fold: cada modelo se evalúa 10 veces (cross-validation)
  k = c(0:9, 0:9, 0:9, 0:9, 0:9, 0:9, 0:9),
  
  # Nombres de los modelos: 7 modelos × 10 repeticiones = 70 observaciones
  Model = c(rep("Modelo 1", 10),
            rep("Modelo 2", 10),
            rep("Modelo 3", 10),
            rep("Modelo 4", 10),
            rep("Modelo 5", 10),
            rep("Modelo 6", 10),
            rep("Modelo 7", 10)),
  
  # Valores RMSE para cada modelo en cada fold
  RMSE = c(
    # Modelo 1: RMSE promedio ≈ 1.13 (rendimiento medio-bajo)
    1.15317248, 1.094146062, 1.08252019, 1.150197779, 1.148999327,
    1.172396587, 1.119769282, 1.113353912, 1.102345521, 1.160173777,
    
    # Modelo 2: RMSE promedio ≈ 1.27 (peor rendimiento de ML)
    1.360459159, 1.238091344, 1.172334603, 1.247108586, 1.250675607,
    1.338198252, 1.353168796, 1.400085837, 1.233751589, 1.267018534,
    
    # Modelo 3: RMSE promedio ≈ 1.64 (peor rendimiento general)
    1.739630935, 1.632993162, 1.513100685, 1.670171753, 1.605910137,
    1.685854461, 1.678031428, 1.670171753, 1.781976037, 1.541814834,
    
    # Modelo 4: RMSE promedio ≈ 0.38 (buen rendimiento)
    0.734062522, 0.966192377, 0.635827734, 0.369267117, 0.418410504,
    0.06882206, 0.237979002, 0.156908609, 0.207878188, 0.044926705,
    
    # Modelo 5: RMSE promedio ≈ 0.24 (mejor rendimiento)
    0.085310509, 0.342572394, 0.286321053, 0.452141056, 0.146183785,
    0.165597451, 0.268832011, 0.292341552, 0.229788156, 0.219851925,
    
    # Modelo 6: RMSE promedio ≈ 0.22 (excelente rendimiento)
    0.46105225, 0.107326859, 0.222540274, 0.195357962, 0.369261406,
    0.069869008, 0.288529072, 0.318150725, 0.09844106, 0.168456021,
    
    # Modelo 7: RMSE promedio ≈ 0.23 (excelente rendimiento)
    0.461937906, 0.189735025, 0.214024678, 0.130751812, 0.30818098,
    0.105565522, 0.252734678, 0.362738926, 0.166936386, 0.104371999
  )
)

# MOSTRAR INFORMACIÓN BÁSICA DE LOS DATOS
cat("=== INFORMACIÓN BÁSICA DE LOS DATOS ===\n")
cat("Total de observaciones:", nrow(base), "\n")
cat("Número de modelos:", length(unique(base$Model)), "\n")
cat("Número de folds por modelo:", length(unique(base$k)), "\n")
cat("Rango de valores RMSE:", round(min(base$RMSE), 3), "a", round(max(base$RMSE), 3), "\n\n")

print("Estructura detallada de los datos:")
print(base)

# =============================================================================
# PASO 1: TRANSFORMACIÓN DE DATOS Y ESTADÍSTICAS DESCRIPTIVAS
# =============================================================================

# CONVERSIÓN A FORMATO ANCHO (WIDE FORMAT)
# Convertimos los datos de formato largo (long) a formato ancho (wide)
# para facilitar la visualización y comparación entre modelos
# Formato largo: cada fila es una observación (modelo + fold + RMSE)
# Formato ancho: cada fila es un fold, cada columna es un modelo

cat("=== PASO 1: TRANSFORMACIÓN DE DATOS ===\n")
cat("Convirtiendo datos de formato largo a formato ancho...\n")

base_wide <- base %>%
  pivot_wider(names_from = Model, values_from = RMSE)

# MOSTRAR TABLA EN FORMATO ANCHO
cat("Datos en formato ancho (cada columna = un modelo, cada fila = un fold):\n")
print(base_wide)

# GENERAR TABLA PARA DOCUMENTOS ACADÉMICOS (LaTeX)
cat("\nTabla en formato LaTeX (para artículos científicos):\n")
print(xtable(base_wide, type='latex'))

# CALCULAR ESTADÍSTICAS DESCRIPTIVAS POR MODELO
cat("\n=== ESTADÍSTICAS DESCRIPTIVAS POR MODELO ===\n")
estadisticas_descriptivas <- base %>%
  group_by(Model) %>%
  summarise(
    Media = round(mean(RMSE), 4),
    Mediana = round(median(RMSE), 4),
    Desv_Est = round(sd(RMSE), 4),
    Mínimo = round(min(RMSE), 4),
    Máximo = round(max(RMSE), 4),
    .groups = 'drop'
  ) %>%
  arrange(Media)  # Ordenar por media (mejor a peor rendimiento)

print(estadisticas_descriptivas)

# RANKING DE MODELOS POR RENDIMIENTO
cat("\n=== RANKING DE MODELOS (MEJOR A PEOR) ===\n")
ranking <- base %>%
  group_by(Model) %>%
  summarise(RMSE_Promedio = round(mean(RMSE), 4), .groups = 'drop') %>%
  arrange(RMSE_Promedio) %>%
  mutate(Ranking = row_number())

for(i in 1:nrow(ranking)) {
  cat("Puesto", ranking$Ranking[i], ":", ranking$Model[i], 
      "- RMSE promedio:", ranking$RMSE_Promedio[i], "\n")
}

# =============================================================================
# PASO 2: ANÁLISIS DE NORMALIDAD (SUPUESTO FUNDAMENTAL)
# =============================================================================

# PROPÓSITO: Verificar si los datos de RMSE siguen una distribución normal
# IMPORTANCIA: Muchas pruebas estadísticas requieren normalidad de datos
# MÉTODO: Prueba de Shapiro-Wilk + Gráficos Q-Q

cat("\n=== PASO 2: ANÁLISIS DE NORMALIDAD ===\n")
cat("Verificando si los datos RMSE siguen una distribución normal...\n\n")

# GRÁFICOS Q-Q (QUANTILE-QUANTILE PLOTS)
# Los gráficos Q-Q comparan la distribución de nuestros datos con una distribución normal teórica
# INTERPRETACIÓN:
# - Puntos en línea recta = datos normales
# - Puntos curvados o dispersos = datos no normales

cat("Generando gráficos Q-Q para análisis visual de normalidad...\n")
ggqqplot(data = base, x = "RMSE", facet.by = "Model", xlab = FALSE,
         ylab = "RMSE",
         title = "Gráficos Q-Q por Modelo",
         subtitle = "Puntos en línea recta indican normalidad")

# PRUEBA ESTADÍSTICA DE SHAPIRO-WILK
# HIPÓTESIS:
# H0 (hipótesis nula): Los datos siguen una distribución normal
# H1 (hipótesis alternativa): Los datos NO siguen una distribución normal
# 
# REGLA DE DECISIÓN:
# Si p-value < 0.05 → Rechazamos H0 (datos NO normales)
# Si p-value ≥ 0.05 → No rechazamos H0 (datos posiblemente normales)

cat("Realizando pruebas de Shapiro-Wilk por modelo...\n")
cat("H0: Los datos siguen distribución normal | H1: Los datos NO son normales\n")
cat("Criterio: p < 0.05 = rechazo de H0 (no normalidad)\n\n")

# Ejecutar prueba y mostrar resultados
shapiro_resultados <- base %>% 
  group_by(Model) %>% 
  shapiro_test(RMSE)

print(shapiro_resultados)

# INTERPRETACIÓN AUTOMÁTICA DE RESULTADOS
cat("\n=== INTERPRETACIÓN DE NORMALIDAD ===\n")
for(i in 1:nrow(shapiro_resultados)) {
  modelo <- shapiro_resultados$Model[i]
  p_valor <- shapiro_resultados$p[i]
  
  if(p_valor < 0.05) {
    cat("✗", modelo, ": NO es normal (p =", round(p_valor, 4), "< 0.05)\n")
  } else {
    cat("✓", modelo, ": Posiblemente normal (p =", round(p_valor, 4), "≥ 0.05)\n")
  }
}

# Contar cuántos modelos son normales
modelos_normales <- sum(shapiro_resultados$p >= 0.05)
cat("\nResumen:", modelos_normales, "de", nrow(shapiro_resultados), "modelos muestran distribución normal.\n")

# GENERAR TABLA EN FORMATO LaTeX
cat("\nTabla de resultados de normalidad (formato LaTeX):\n")
shapiro_results_latex <- xtable(shapiro_resultados)
print(shapiro_results_latex, type = "latex", include.rownames = FALSE)

# =============================================================================
# PASO 3: ANÁLISIS DE HOMOGENEIDAD DE VARIANZAS
# =============================================================================

# PROPÓSITO: Verificar si las varianzas son iguales entre todos los modelos
# IMPORTANCIA: Muchas pruebas (como ANOVA) requieren varianzas homogéneas
# MÉTODO: Prueba de Levene

cat("\n=== PASO 3: HOMOGENEIDAD DE VARIANZAS ===\n")
cat("Verificando si las varianzas son similares entre modelos...\n\n")

# PRUEBA DE LEVENE
# HIPÓTESIS:
# H0: Las varianzas son homogéneas (iguales) entre todos los grupos
# H1: Al menos una varianza es diferente
# 
# REGLA DE DECISIÓN:
# Si p-value < 0.05 → Rechazamos H0 (varianzas heterogéneas/diferentes)
# Si p-value ≥ 0.05 → No rechazamos H0 (varianzas homogéneas/similares)

cat("Realizando prueba de Levene para homogeneidad de varianzas...\n")
cat("H0: Varianzas homogéneas | H1: Varianzas heterogéneas\n")
cat("Criterio: p < 0.05 = rechazo de H0 (varianzas diferentes)\n\n")

levene_result <- leveneTest(RMSE ~ Model, data = base)
print(levene_result)

# INTERPRETACIÓN AUTOMÁTICA
p_levene <- levene_result$`Pr(>F)`[1]
cat("\n=== INTERPRETACIÓN DE HOMOGENEIDAD ===\n")
if(p_levene < 0.05) {
  cat("✗ Las varianzas NO son homogéneas (p =", round(p_levene, 6), "< 0.05)\n")
  cat("  Interpretación: Los modelos tienen variabilidades diferentes\n")
  cat("  Implicación: Se deben usar pruebas no paramétricas\n")
} else {
  cat("✓ Las varianzas son homogéneas (p =", round(p_levene, 6), "≥ 0.05)\n")
  cat("  Interpretación: Los modelos tienen variabilidades similares\n")
  cat("  Implicación: Se pueden usar pruebas paramétricas\n")
}

# MOSTRAR VARIANZAS POR MODELO PARA COMPARACIÓN VISUAL
cat("\n=== VARIANZAS POR MODELO ===\n")
varianzas <- base %>%
  group_by(Model) %>%
  summarise(
    Varianza = round(var(RMSE), 6),
    Desv_Est = round(sd(RMSE), 4),
    .groups = 'drop'
  ) %>%
  arrange(desc(Varianza))

print(varianzas)

# GENERAR TABLA EN FORMATO LaTeX
cat("\nTabla de resultados de homogeneidad (formato LaTeX):\n")
levene_result_latex <- xtable(levene_result)
print(levene_result_latex, type = "latex", include.rownames = FALSE)

# =============================================================================
# PASO 4: PRUEBA DE FRIEDMAN
# =============================================================================

# PROPÓSITO: Comparar múltiples modelos cuando no se cumplen los supuestos para ANOVA
# CUANDO USAR: Cuando datos no son normales O varianzas no son homogéneas
# MÉTODO: Prueba no paramétrica equivalente a ANOVA de medidas repetidas

cat("\n=== PASO 4: PRUEBA DE FRIEDMAN ===\n")
cat("Comparando rendimiento de todos los modelos simultáneamente...\n\n")

# ¿QUÉ ES LA PRUEBA DE FRIEDMAN?
# - Alternativa no paramétrica a ANOVA de medidas repetidas
# - Compara rankings (posiciones) en lugar de valores absolutos
# - No requiere normalidad ni homogeneidad de varianzas
# - Usa los mismos sujetos (folds de CV) para todos los tratamientos (modelos)

cat("=== METODOLOGÍA DE FRIEDMAN ===\n")
cat("• Alternativa no paramétrica a ANOVA de medidas repetidas\n")
cat("• Compara rankings de modelos dentro de cada fold\n")
cat("• No requiere normalidad ni homogeneidad de varianzas\n")
cat("• Ideal para datos de validación cruzada\n\n")

# HIPÓTESIS DE LA PRUEBA
cat("=== HIPÓTESIS ===\n")
cat("H0: No hay diferencias en el rendimiento mediano entre modelos\n")
cat("    (todos los modelos tienen el mismo rendimiento)\n")
cat("H1: Al menos un modelo tiene rendimiento diferente\n")
cat("Criterio: p < 0.05 = hay diferencias significativas\n\n")

# REALIZAR PRUEBA DE FRIEDMAN usando rstatix (más moderno y fácil)
cat("Ejecutando prueba de Friedman...\n")
friedman <- base %>% 
  friedman_test(RMSE ~ Model | k)

print(friedman)

# INTERPRETACIÓN AUTOMÁTICA
cat("\n=== INTERPRETACIÓN DE FRIEDMAN ===\n")
if(friedman$p < 0.05) {
  cat("✓ RESULTADO SIGNIFICATIVO (p =", round(friedman$p, 6), "< 0.05)\n")
  cat("  Conclusión: HAY diferencias significativas entre modelos\n")
  cat("  Implicación: Algunos modelos son estadísticamente mejores que otros\n")
  cat("  Siguiente paso: Realizar comparaciones post-hoc para identificar diferencias específicas\n")
} else {
  cat("✗ RESULTADO NO SIGNIFICATIVO (p =", round(friedman$p, 6), "≥ 0.05)\n")
  cat("  Conclusión: NO hay diferencias significativas entre modelos\n")
  cat("  Implicación: Todos los modelos tienen rendimiento estadísticamente similar\n")
  cat("  Siguiente paso: No se requieren comparaciones adicionales\n")
}

# GENERAR TABLA EN FORMATO MARKDOWN Y LaTeX
cat("\nTabla de resultados de Friedman (formato Markdown):\n")
print(kable(friedman, format = "markdown"))

cat("\nTabla de resultados de Friedman (formato LaTeX):\n")
friedman_latex <- xtable(friedman)
print(friedman_latex, type = "latex", include.rownames = FALSE)


# =============================================================================
# PASO 5: ANÁLISIS POST-HOC (COMPARACIONES MÚLTIPLES)
# =============================================================================

# PROPÓSITO: Identificar cuáles modelos específicos son diferentes entre sí
# CUÁNDO USAR: Solo cuando Friedman es significativo (p < 0.05)
# MÉTODO: Comparaciones por pares con corrección de Bonferroni

cat("\n=== PASO 5: ANÁLISIS POST-HOC ===\n")
cat("Identificando diferencias específicas entre pares de modelos...\n\n")

# ¿QUÉ SON LAS COMPARACIONES POST-HOC?
# - Después de encontrar diferencias globales (Friedman), necesitamos saber DÓNDE están
# - Comparan cada modelo contra cada otro modelo (comparaciones por pares)
# - Usan corrección estadística para evitar inflación del error tipo I
# - Solo se realizan si el test global (Friedman) fue significativo

cat("=== METODOLOGÍA POST-HOC ===\n")
cat("• Se ejecuta solo si Friedman es significativo\n")
cat("• Compara cada modelo contra todos los demás (pares)\n")
cat("• Usa corrección de Bonferroni para controlar error tipo I\n")
cat("• Identifica diferencias estadísticamente significativas\n\n")

# VERIFICAR SI FRIEDMAN FUE SIGNIFICATIVO
if(friedman$p < 0.05) {
  cat("✓ Friedman significativo → Procediendo con comparaciones post-hoc\n\n")
  
  # PREPARAR DATOS PARA COMPARACIONES
  # Filtrar datos para incluir todos los modelos
  cat("Preparando datos para comparaciones por pares...\n")
  datos_test1 <- base %>% 
    filter(Model %in% c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Modelo 7"))
  
  cat("Modelos incluidos en el análisis:", length(unique(datos_test1$Model)), "\n")
  cat("Número total de comparaciones:", choose(length(unique(datos_test1$Model)), 2), "\n\n")
  
  # REALIZAR COMPARACIONES POR PARES CON WILCOXON
  # MÉTODO: Wilcoxon signed-rank test para muestras pareadas
  # CORRECCIÓN: Bonferroni para múltiples comparaciones
  # HIPÓTESIS POR PAR:
  # H0: No hay diferencia entre los dos modelos comparados
  # H1: Hay diferencia significativa entre los dos modelos
  
  cat("=== HIPÓTESIS PARA CADA COMPARACIÓN ===\n")
  cat("H0: No hay diferencia entre el par de modelos\n")
  cat("H1: Hay diferencia significativa entre el par de modelos\n")
  cat("Método: Wilcoxon signed-rank test (muestras pareadas)\n")
  cat("Corrección: Bonferroni (ajuste por múltiples comparaciones)\n")
  cat("Criterio: p.adj < 0.05 = diferencia significativa\n\n")
  
  cat("Ejecutando comparaciones por pares...\n")
  resultado_pares <- datos_test1 %>%
    wilcox_test(RMSE ~ Model, paired = TRUE, p.adjust.method = "bonferroni") %>%
    add_significance()
  
  print(resultado_pares)
  
  # INTERPRETACIÓN AUTOMÁTICA DE RESULTADOS
  cat("\n=== INTERPRETACIÓN DE COMPARACIONES POR PARES ===\n")
  comparaciones_significativas <- resultado_pares %>% filter(p.adj < 0.05)
  
  if(nrow(comparaciones_significativas) > 0) {
    cat("✓ DIFERENCIAS SIGNIFICATIVAS ENCONTRADAS:\n")
    cat("Número de pares significativamente diferentes:", nrow(comparaciones_significativas), "\n\n")
    
    cat("COMPARACIONES SIGNIFICATIVAS (p.adj < 0.05):\n")
    for(i in 1:nrow(comparaciones_significativas)) {
      grupo1 <- comparaciones_significativas$group1[i]
      grupo2 <- comparaciones_significativas$group2[i]
      p_adj <- round(comparaciones_significativas$p.adj[i], 6)
      cat("•", grupo1, "vs", grupo2, "→ p.adj =", p_adj, "\n")
    }
  } else {
    cat("✗ NO SE ENCONTRARON DIFERENCIAS SIGNIFICATIVAS\n")
    cat("Aunque Friedman fue significativo, las comparaciones por pares\n")
    cat("no detectaron diferencias específicas después de la corrección.\n")
    cat("Esto puede ocurrir con correcciones conservadoras como Bonferroni.\n")
  }
  
  # GENERAR TABLA EN FORMATO LaTeX
  cat("\n=== TABLA DE RESULTADOS POST-HOC ===\n")
  cat("Tabla de comparaciones por pares (formato LaTeX):\n")
  resultado_pares_latex <- xtable(resultado_pares)
  print(resultado_pares_latex, type = "latex", include.rownames = FALSE)
  
} else {
  cat("✗ Friedman NO significativo → NO se realizan comparaciones post-hoc\n")
  cat("Razón: No hay evidencia de diferencias globales entre modelos\n")
  cat("Recomendación: Todos los modelos pueden considerarse estadísticamente equivalentes\n")
}

# =============================================================================
# PASO 6: VISUALIZACIÓN DE RESULTADOS
# =============================================================================

# PROPÓSITO: Crear gráficos para visualizar las diferencias entre modelos
# TIPOS: Boxplots para comparar distribuciones y variabilidad
# BENEFICIO: Complementa análisis estadístico con representación visual

cat("\n=== PASO 6: VISUALIZACIÓN DE RESULTADOS ===\n")
cat("Generando gráficos para visualizar diferencias entre modelos...\n\n")

# ¿POR QUÉ VISUALIZAR?
# - Los gráficos complementan las pruebas estadísticas
# - Muestran patrones que los números pueden no revelar claramente
# - Ayudan a interpretar la magnitud práctica de las diferencias
# - Facilitan la comunicación de resultados

cat("=== IMPORTANCIA DE LA VISUALIZACIÓN ===\n")
cat("• Complementa las pruebas estadísticas con evidencia visual\n")
cat("• Muestra distribuciones, outliers y patrones de variabilidad\n")
cat("• Ayuda a interpretar significancia práctica vs estadística\n")
cat("• Facilita comunicación de resultados a audiencias diversas\n\n")

# PREPARAR DATOS PARA VISUALIZACIÓN
# Reordenar modelos por rendimiento (mejor a peor)
cat("Preparando datos para visualización...\n")
orden_modelos <- base %>%
  group_by(Model) %>%
  summarise(RMSE_promedio = mean(RMSE), .groups = 'drop') %>%
  arrange(RMSE_promedio) %>%
  pull(Model)

datos_para_grafico <- base %>%
  mutate(Model = factor(Model, levels = orden_modelos))

cat("Orden de modelos (mejor a peor rendimiento):", paste(orden_modelos, collapse = " > "), "\n\n")

# CREAR BOXPLOT COMPARATIVO
# ELEMENTOS DEL BOXPLOT:
# - Caja: Rango intercuartílico (Q1 a Q3)
# - Línea central: Mediana (Q2)
# - Bigotes: Extienden a 1.5 * IQR desde los cuartiles
# - Puntos: Valores atípicos (outliers)

cat("Creando boxplot comparativo de RMSE por modelo...\n")
cat("Elementos del boxplot:\n")
cat("• Caja: Rango intercuartílico (Q1-Q3)\n")
cat("• Línea central: Mediana\n")
cat("• Bigotes: 1.5 × IQR desde los cuartiles\n")
cat("• Puntos individuales: Valores atípicos\n\n")

# GENERAR EL GRÁFICO PRINCIPAL
grafico_boxplot <- ggplot(datos_para_grafico, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +  # Puntos individuales
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "none"  # Quitar leyenda redundante
  ) +
  labs(
    title = "Comparación de Rendimiento entre Modelos",
    subtitle = "Distribución de RMSE por Modelo (10-fold Cross-Validation)",
    x = "Modelo",
    y = "RMSE",
    caption = "Puntos rojos indican valores atípicos"
  ) +
  scale_fill_manual(values = c("#00429d", "#009473", "#ff8c00", "#9b0000", 
                               "#7200da", "#00a6ed", "#00abcd"))

# MOSTRAR EL GRÁFICO
print(grafico_boxplot)

# INTERPRETACIÓN VISUAL
cat("\n=== GUÍA DE INTERPRETACIÓN VISUAL ===\n")
cat("Cómo interpretar el boxplot:\n")
cat("• Posición vertical: Modelos más abajo = mejor rendimiento (menor RMSE)\n")
cat("• Altura de cajas: Mayor altura = mayor variabilidad\n")
cat("• Solapamiento de cajas: Indica posibles diferencias no significativas\n")
cat("• Outliers (puntos rojos): Rendimientos excepcionalmente buenos o malos\n")
cat("• Bigotes largos: Mayor dispersión en los datos\n\n")

# MOSTRAR ESTADÍSTICAS FINALES PARA ACOMPAÑAR EL GRÁFICO
cat("=== RESUMEN ESTADÍSTICO FINAL ===\n")
resumen_final <- base %>%
  group_by(Model) %>%
  summarise(
    Mediana = round(median(RMSE), 4),
    Media = round(mean(RMSE), 4),
    Q1 = round(quantile(RMSE, 0.25), 4),
    Q3 = round(quantile(RMSE, 0.75), 4),
    IQR = round(Q3 - Q1, 4),
    Outliers = sum(RMSE < (Q1 - 1.5*IQR) | RMSE > (Q3 + 1.5*IQR)),
    .groups = 'drop'
  ) %>%
  arrange(Mediana)

print(resumen_final)

cat("\n=== CONCLUSIONES DEL ANÁLISIS ===\n")
mejor_modelo <- resumen_final$Model[1]
peor_modelo <- resumen_final$Model[nrow(resumen_final)]
cat("📊 Mejor modelo (menor RMSE mediano):", mejor_modelo, "\n")
cat("📊 Peor modelo (mayor RMSE mediano):", peor_modelo, "\n")
cat("📊 Diferencia en medianas:", 
    round(resumen_final$Mediana[nrow(resumen_final)] - resumen_final$Mediana[1], 4), "\n")

if(friedman$p < 0.05) {
  cat("📊 Significancia estadística: SÍ confirmada (Friedman p < 0.05)\n")
} else {
  cat("📊 Significancia estadística: NO confirmada (Friedman p ≥ 0.05)\n")
}

cat("\n🎯 RECOMENDACIÓN FINAL:\n")
cat("Basado en el análisis estadístico y visual,", mejor_modelo, 
    "muestra el mejor rendimiento\n")
cat("para la tarea de predicción de series temporales evaluada.\n")

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat(paste(rep("=", 80), collapse=""), "\n")

