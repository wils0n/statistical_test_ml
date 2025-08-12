install.packages("xtable")
install.packages("car")
install.packages("PMCMRplus")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("knitr")
install.packages("tidyverse")

library(PMCMRplus)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)
library(xtable)
library(car)
library(knitr)
library(tidyverse)

# cargamos los datos
base <- read_csv("10/rms_models_10.csv")
base

#base$RMSE <- sub(",",".",base$RMSE)
#base$RMSE <- as.numeric(base$RMSE)
#base

base_wide <- base %>%
  pivot_wider(names_from = Model, values_from = RMSE)

# Mostrar la tabla en formato ancho
print(base_wide)

print(xtable(base_wide, type='latex'))

ggqqplot(data = base, x = "RMSE", facet.by = "Model", xlab = FALSE,
         ylab = "RMSE")

base %>% 
  group_by(Model) %>% 
  shapiro_test(RMSE)

# Realizar la prueba de Shapiro-Wilk para normalidad
shapiro_results <- base %>% 
  group_by(Model) %>% 
  shapiro_test(RMSE)

shapiro_results_latex <- xtable(shapiro_results)
print(shapiro_results_latex, type = "latex", include.rownames = FALSE)

# Realizar la prueba de Levene
levene_result <- leveneTest(RMSE ~ Model, data = base)
print(levene_result)
levene_result_latex <- xtable(levene_result)
print(levene_result_latex, type = "latex", include.rownames = FALSE)

friedman <- base %>% 
  friedman_test(RMSE ~ Model | k)
friedman
print(kable(friedman, format = "markdown"))
friedman_latex <- xtable(friedman)
print(friedman_latex, type = "latex", include.rownames = FALSE)


# Verifica si el valor p es significativo
if(friedman$p < 0.05) {
  # Filtrar los datos para cada comparación específica
  datos_test1 <- base %>% 
    filter(Model %in% c("Ridge Regression", "Random Forest Regressor", "Decision Tree Regressor", "ARIMA", "Holt-Winters", "LSTM", "CNN"))
  
  # Realizar las pruebas de Wilcoxon pareadas con ajuste de p-valores
  test1 <- datos_test1 %>%
    pairwise_wilcox_test(RMSE ~ Model, paired = TRUE, p.adjust.method = "bonferroni")
  test1
  
  test1_latex <- xtable(test1)
  print(test1_latex, type = "latex", include.rownames = FALSE)
  
  write.csv(test1, "recursos/results_pair_wilcox_test.csv", row.names = FALSE)
  
  # Asumiendo que test1 y test2 son resultados de pairwise_wilcox_test
  pwc_test1 <- test1 %>% add_xy_position(x = "Model")
  
  # Graficar test1
  ggboxplot(datos_test1, x = "Model", y = "RMSE", fill = "Model") +
    stat_pvalue_manual(pwc_test1, label = "p.adj.signif") +
    labs(subtitle = "Resultados de Test 1")
  
  
  # Preparar datos para gráficos
  datos_para_grafico <- base %>% 
    filter(Model %in% c("Decision Tree Regressor", "Random Forest Regressor", "Ridge Regression" , "ARIMA", "Holt-Winters", "LSTM", "CNN")) %>%
    mutate(Model = factor(Model,
                          levels = c("Decision Tree Regressor", "Random Forest Regressor", "Ridge Regression" , "ARIMA", "Holt-Winters", "LSTM", "CNN"),
                          labels = c("Decision Tree Regressor", "Random Forest Regressor", "Ridge Regression" , "ARIMA", "Holt-Winters", "LSTM", "CNN")))
  
  # Verificar los datos filtrados
  print(summary(datos_para_grafico))
  
  # Verificar los niveles del factor
  print(levels(datos_para_grafico$Model))
  
  # Boxplot o Dotplot
  ggplot(datos_para_grafico, aes(x = Model, y = RMSE, fill = Model)) +
    geom_boxplot() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),  # Esto ocultará los nombres en el eje X
      axis.title.x = element_text(angle = 0, hjust = 0.5)  # Configura el título del eje X
    ) +
    scale_fill_manual(values = c("#00429d", "#009473", "#ff8c00", "#9b0000", "#7200da", "#00a6ed", "#00abcd")) +
    labs(x = "Models", y = "RMSE", fill = "Models")
  
  
  
  
} else {
  print("La prueba de Friedman no es significativa, no se realiza la prueba post-hoc de wilcocox")
}

