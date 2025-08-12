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
# Autores: Wilson Julca Mejía
# Ejecución: https://www.mycompiler.io/view/EgSsWrU3HNA
# =============================================================================

# Función para instalar paquetes solo si es posible
try_install <- function(packages) {
  cat("=== VERIFICANDO PAQUETES ===\n")
  available_packages <- character(0)
  
  for (pkg in packages) {
    cat("Verificando paquete:", pkg, "... ")
    
    if (require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("✓ DISPONIBLE\n")
      available_packages <- c(available_packages, pkg)
    } else {
      cat("✗ NO DISPONIBLE - usaremos funciones base\n")
    }
  }
  
  cat("=== VERIFICACIÓN COMPLETADA ===\n")
  cat("Paquetes disponibles:", paste(available_packages, collapse = ", "), "\n\n")
  
  return(available_packages)
}

# Lista de paquetes deseados (pero no requeridos)
desired_packages <- c("xtable", "car", "dplyr")

# Verificar qué paquetes están disponibles
available_pkgs <- try_install(desired_packages)

cat("Iniciando análisis con funciones disponibles...\n\n")

# =============================================================================
# DATOS DE ENTRADA: Resultados RMSE de diferentes modelos
# =============================================================================
# Los datos contienen resultados de RMSE para 7 modelos diferentes evaluados
# en 10 folds de validación cruzada (k = 0 a 9)
# Modelos incluidos:
# - Modelo 1
# - Modelo 2
# - Modelo 3
# - Modelo 4
# - Modelo 5
# - Modelo 6
# - Modelo 7

base <- data.frame(
  k = c(0:9, 0:9, 0:9, 0:9, 0:9, 0:9, 0:9),
  Model = c(rep("Modelo 1", 10),
            rep("Modelo 2", 10),
            rep("Modelo 3", 10),
            rep("Modelo 4", 10),
            rep("Modelo 5", 10),
            rep("Modelo 6", 10),
            rep("Modelo 7", 10)),
  RMSE = c(
    # Modelo 1
    1.15317248, 1.094146062, 1.08252019, 1.150197779, 1.148999327,
    1.172396587, 1.119769282, 1.113353912, 1.102345521, 1.160173777,
    # Modelo 2
    1.360459159, 1.238091344, 1.172334603, 1.247108586, 1.250675607,
    1.338198252, 1.353168796, 1.400085837, 1.233751589, 1.267018534,
    # Modelo 3
    1.739630935, 1.632993162, 1.513100685, 1.670171753, 1.605910137,
    1.685854461, 1.678031428, 1.670171753, 1.781976037, 1.541814834,
    # Modelo 4
    0.734062522, 0.966192377, 0.635827734, 0.369267117, 0.418410504,
    0.06882206, 0.237979002, 0.156908609, 0.207878188, 0.044926705,
    # Modelo 5
    0.085310509, 0.342572394, 0.286321053, 0.452141056, 0.146183785,
    0.165597451, 0.268832011, 0.292341552, 0.229788156, 0.219851925,
    # Modelo 6
    0.46105225, 0.107326859, 0.222540274, 0.195357962, 0.369261406,
    0.069869008, 0.288529072, 0.318150725, 0.09844106, 0.168456021,
    # Modelo 7
    0.461937906, 0.189735025, 0.214024678, 0.130751812, 0.30818098,
    0.105565522, 0.252734678, 0.362738926, 0.166936386, 0.104371999
  )
)

# Mostrar estructura de los datos
print("Estructura de los datos:")
print(base)

# =============================================================================
# TRANSFORMACIÓN Y EXPLORACIÓN DE DATOS
# =============================================================================

# Convertir datos de formato largo a formato ancho usando funciones base
# (cada modelo será una columna)
cat("Convirtiendo datos a formato ancho...\n")

# Crear matriz para formato ancho usando funciones base
unique_models <- unique(base$Model)
k_values <- 0:9

# Crear matriz vacía
base_wide <- matrix(NA, nrow = length(k_values), ncol = length(unique_models))
colnames(base_wide) <- unique_models
rownames(base_wide) <- paste("k", k_values, sep = "_")

# Llenar la matriz
for (i in 1:nrow(base)) {
  k_idx <- base$k[i] + 1  # +1 porque R indexa desde 1
  model_idx <- which(unique_models == base$Model[i])
  base_wide[k_idx, model_idx] <- base$RMSE[i]
}

# Convertir a data.frame
base_wide <- as.data.frame(base_wide)
base_wide$k <- k_values

# Mostrar la tabla en formato ancho
cat("Datos en formato ancho (cada modelo como columna):\n")
print(base_wide)

# =============================================================================
# ANÁLISIS DE NORMALIDAD
# =============================================================================

# Crear gráficos Q-Q básicos usando funciones base de R
cat("Generando gráficos Q-Q para análisis de normalidad...\n")

# Configurar ventana de gráficos
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

for (model in unique_models) {
  model_data <- base$RMSE[base$Model == model]
  qqnorm(model_data, main = paste("Q-Q Plot:", model))
  qqline(model_data)
}

# Restaurar configuración de gráficos
par(mfrow = c(1, 1))

# Realizar prueba de Shapiro-Wilk para normalidad por modelo usando funciones base
cat("Realizando pruebas de Shapiro-Wilk para normalidad:\n")
cat("H0: Los datos siguen una distribución normal\n")
cat("H1: Los datos NO siguen una distribución normal\n")
cat("Si p < 0.05, rechazamos H0 (datos no normales)\n\n")

shapiro_results <- data.frame(
  Model = character(0),
  Statistic = numeric(0),
  P_Value = numeric(0),
  stringsAsFactors = FALSE
)

for (model in unique_models) {
  model_data <- base$RMSE[base$Model == model]
  shapiro_test <- shapiro.test(model_data)
  
  shapiro_results <- rbind(shapiro_results, data.frame(
    Model = model,
    Statistic = round(shapiro_test$statistic, 6),
    P_Value = round(shapiro_test$p.value, 6)
  ))
  
  cat("Modelo:", model, "- W =", round(shapiro_test$statistic, 4), 
      "- p-value =", round(shapiro_test$p.value, 6), "\n")
}

cat("\nResultados de normalidad por modelo:\n")
print(shapiro_results)

# Generar tabla de resultados si xtable está disponible
if ("xtable" %in% available_pkgs) {
  cat("Resultados de normalidad en formato LaTeX:\n")
  shapiro_results_latex <- xtable(shapiro_results)
  print(shapiro_results_latex, type = "latex", include.rownames = FALSE)
}

# =============================================================================
# ANÁLISIS DE HOMOGENEIDAD DE VARIANZAS
# =============================================================================

# Realizar la prueba de Levene usando funciones disponibles
cat("Realizando prueba de homogeneidad de varianzas:\n")
cat("H0: Las varianzas son homogéneas entre grupos\n")
cat("H1: Las varianzas NO son homogéneas entre grupos\n")
cat("Si p < 0.05, rechazamos H0 (varianzas heterogéneas)\n\n")

if ("car" %in% available_pkgs) {
  # Usar prueba de Levene si car está disponible
  levene_result <- leveneTest(RMSE ~ Model, data = base)
  print(levene_result)
  
  if ("xtable" %in% available_pkgs) {
    levene_result_latex <- xtable(levene_result)
    cat("Resultados de prueba de Levene en formato LaTeX:\n")
    print(levene_result_latex, type = "latex", include.rownames = FALSE)
  }
} else {
  # Usar prueba alternativa con funciones base
  cat("Usando prueba de Bartlett (alternativa a Levene):\n")
  bartlett_result <- bartlett.test(RMSE ~ Model, data = base)
  print(bartlett_result)
  
  cat("Interpretación: p-value =", round(bartlett_result$p.value, 6), "\n")
  if (bartlett_result$p.value < 0.05) {
    cat("Las varianzas NO son homogéneas (p < 0.05)\n")
  } else {
    cat("Las varianzas son homogéneas (p >= 0.05)\n")
  }
}

# =============================================================================
# PRUEBA DE FRIEDMAN PARA COMPARACIÓN DE MÚLTIPLES GRUPOS
# =============================================================================

# La prueba de Friedman es la alternativa no paramétrica al ANOVA de medidas repetidas
# Se usa cuando los datos no cumplen supuestos de normalidad
# H0: No hay diferencias significativas entre los modelos
# H1: Existe al menos una diferencia significativa entre los modelos
# Si p < 0.05, rechazamos H0 (hay diferencias significativas)

cat("Realizando prueba de Friedman para comparar modelos:\n")

# Preparar datos para la prueba de Friedman usando funciones base
# Crear matriz de datos (filas = bloques/k, columnas = modelos)
friedman_matrix <- matrix(NA, nrow = 10, ncol = length(unique_models))
colnames(friedman_matrix) <- unique_models

for (i in 1:10) {
  k_data <- base[base$k == (i-1), ]
  for (j in 1:length(unique_models)) {
    model_idx <- which(k_data$Model == unique_models[j])
    if (length(model_idx) > 0) {
      friedman_matrix[i, j] <- k_data$RMSE[model_idx]
    }
  }
}

# Realizar prueba de Friedman
friedman_result <- friedman.test(friedman_matrix)
print(friedman_result)

# Mostrar interpretación
cat("\nInterpretación de la prueba de Friedman:\n")
cat("Chi-cuadrado =", round(friedman_result$statistic, 4), "\n")
cat("p-value =", round(friedman_result$p.value, 6), "\n")

if (friedman_result$p.value < 0.05) {
  cat("Conclusión: HAY diferencias significativas entre los modelos (p < 0.05)\n")
  significant_friedman <- TRUE
} else {
  cat("Conclusión: NO hay diferencias significativas entre los modelos (p >= 0.05)\n")
  significant_friedman <- FALSE
}

# Mostrar estadísticas descriptivas por modelo
cat("\nEstadísticas descriptivas por modelo:\n")
for (model in unique_models) {
  model_data <- base$RMSE[base$Model == model]
  cat("Modelo:", model, "\n")
  cat("  Media:", round(mean(model_data), 4), "\n")
  cat("  Mediana:", round(median(model_data), 4), "\n")
  cat("  Desv. Est.:", round(sd(model_data), 4), "\n")
  cat("  Min:", round(min(model_data), 4), "\n")
  cat("  Max:", round(max(model_data), 4), "\n\n")
}


# =============================================================================
# ANÁLISIS POST-HOC: COMPARACIONES PAREADAS SIMPLIFICADAS
# =============================================================================

# Si la prueba de Friedman es significativa, realizamos comparaciones básicas
if (significant_friedman) {
  cat("La prueba de Friedman es significativa. Realizando análisis post-hoc básico...\n\n")
  
  # Realizar comparaciones pareadas con Wilcoxon usando funciones base
  cat("Realizando pruebas de Wilcoxon pareadas:\n")
  cat("(Sin corrección de Bonferroni - versión simplificada)\n\n")
  
  # Crear matriz de p-valores
  n_models <- length(unique_models)
  pvalue_matrix <- matrix(NA, nrow = n_models, ncol = n_models)
  rownames(pvalue_matrix) <- unique_models
  colnames(pvalue_matrix) <- unique_models
  
  # Realizar todas las comparaciones pareadas
  for (i in 1:(n_models-1)) {
    for (j in (i+1):n_models) {
      model1_data <- base$RMSE[base$Model == unique_models[i]]
      model2_data <- base$RMSE[base$Model == unique_models[j]]
      
      # Prueba de Wilcoxon pareada
      wilcox_result <- wilcox.test(model1_data, model2_data, paired = TRUE)
      pvalue_matrix[i, j] <- wilcox_result$p.value
      pvalue_matrix[j, i] <- wilcox_result$p.value
      
      cat("Comparación:", unique_models[i], "vs", unique_models[j], "\n")
      cat("  p-value =", round(wilcox_result$p.value, 6), "\n")
      
      if (wilcox_result$p.value < 0.05) {
        cat("  Resultado: DIFERENCIA SIGNIFICATIVA\n")
      } else {
        cat("  Resultado: No hay diferencia significativa\n")
      }
      cat("\n")
    }
  }
  
  # Mostrar matriz de p-valores
  cat("Matriz de p-valores (comparaciones pareadas):\n")
  print(round(pvalue_matrix, 4))
  
  # =============================================================================
  # VISUALIZACIONES BÁSICAS
  # =============================================================================
  
  cat("\nGenerando gráficos básicos...\n")
  
  # Boxplot básico usando funciones base
  par(mar = c(8, 4, 4, 2))
  boxplot(RMSE ~ Model, data = base, 
          main = "Comparación de Rendimiento de Modelos",
          ylab = "RMSE",
          xlab = "",
          las = 2,  # Rotar etiquetas
          col = rainbow(n_models))
  
  # Agregar línea con la media general
  abline(h = mean(base$RMSE), col = "red", lty = 2)
  
  # Restaurar márgenes
  par(mar = c(5, 4, 4, 2))
  
  # Gráfico de puntos por modelo
  plot(1:n_models, tapply(base$RMSE, base$Model, mean),
       type = "b", pch = 19, col = "blue",
       xlab = "Modelos", ylab = "RMSE Promedio",
       main = "RMSE Promedio por Modelo",
       xaxt = "n")
  axis(1, at = 1:n_models, labels = unique_models, las = 2)
  
  # Agregar barras de error (desviación estándar)
  for (i in 1:n_models) {
    model_data <- base$RMSE[base$Model == unique_models[i]]
    mean_val <- mean(model_data)
    sd_val <- sd(model_data)
    arrows(i, mean_val - sd_val, i, mean_val + sd_val, 
           angle = 90, code = 3, length = 0.1)
  }
  
} else {
  cat("La prueba de Friedman no es significativa.\n")
  cat("No se realizan pruebas post-hoc.\n")
  cat("Conclusión: No hay diferencias estadísticamente significativas entre los modelos.\n")
}

# =============================================================================
# FIN DEL ANÁLISIS
# =============================================================================
print("Análisis estadístico completado.")
print("Revise los resultados de las pruebas para determinar diferencias significativas entre modelos.")

