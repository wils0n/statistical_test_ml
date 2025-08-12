# 📊 Análisis Estadístico Comparativo de Modelos de Machine Learning

[![License](https://img.shields.io/badge/license-Academic-blue.svg)](LICENSE.txt)
[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![Status](https://img.shields.io/badge/status-Active-green.svg)]()

Un framework completo para realizar análisis estadísticos comparativos entre modelos de machine learning usando validación cruzada y pruebas no paramétricas.

## 🎯 Propósito

Este repositorio contiene scripts en R para comparar estadísticamente el rendimiento de múltiples modelos de machine learning utilizando:

- **Pruebas de normalidad** (Shapiro-Wilk)
- **Análisis de homogeneidad de varianzas** (Levene)
- **Comparaciones múltiples** (Friedman)
- **Análisis post-hoc** (Wilcoxon con corrección Bonferroni)
- **Visualizaciones estadísticas** (Q-Q plots, boxplots)

## 📋 Contenido del Repositorio

```
statistical_test/
│
├── statistical_test.R          # Script principal (versión completa)
├── 1_statistical_test.R        # Versión con comentarios educativos
├── 2_statistical_test_alt.R    # Versión compatible con editores online
├── README.md                   # Este archivo
└── LICENSE.txt                 # Licencia académica
```

## 🚀 Características Principales

### ✅ **Auto-contenido**

- Datos embebidos (no requiere archivos CSV externos)
- Instalación automática de dependencias
- Manejo de errores robusto

### ✅ **Educativo**

- Comentarios detallados en español
- Explicación de hipótesis estadísticas
- Interpretación automática de resultados
- Guías de interpretación visual

### ✅ **Versátil**

- Compatible con R local y editores online
- Exportación a LaTeX para publicaciones
- Visualizaciones profesionales
- Modelos genéricos adaptables

### ✅ **Estadísticamente Robusto**

- Pruebas no paramétricas (sin supuestos estrictos)
- Corrección por comparaciones múltiples
- Validación de supuestos estadísticos

## 📊 Metodología Estadística

### 1. **Análisis de Normalidad**

```r
# Prueba de Shapiro-Wilk por modelo
# H0: Los datos siguen distribución normal
# H1: Los datos NO son normales
shapiro_test(RMSE)
```

### 2. **Homogeneidad de Varianzas**

```r
# Prueba de Levene
# H0: Varianzas homogéneas entre grupos
# H1: Varianzas heterogéneas
leveneTest(RMSE ~ Model, data = base)
```

### 3. **Comparación Global**

```r
# Prueba de Friedman (no paramétrica)
# H0: No hay diferencias entre modelos
# H1: Al menos un modelo es diferente
friedman_test(RMSE ~ Model | k)
```

### 4. **Análisis Post-hoc**

```r
# Comparaciones pareadas con corrección Bonferroni
# Solo si Friedman es significativo (p < 0.05)
wilcox_test(RMSE ~ Model, paired = TRUE, p.adjust.method = "bonferroni")
```

## 🛠️ Instalación y Uso

### **Opción 1: R Local**

1. **Clonar el repositorio:**

```bash
git clone [URL_DEL_REPOSITORIO]
cd statistical_test
```

2. **Ejecutar el script principal:**

```r
source("statistical_test.R")
```

### **Opción 2: Editor Online**

1. **Copiar el contenido de `2_statistical_test_alt.R`**
2. **Pegar en [MyCompiler](https://www.mycompiler.io/new/r) o similar**
3. **Ejecutar directamente**

### **Opción 3: Modificar para tus datos**

```r
# Reemplazar la sección de datos embebidos
base <- data.frame(
  k = c(0:9, 0:9, 0:9),  # Folds de validación cruzada
  Model = c(rep("Modelo A", 10), rep("Modelo B", 10), rep("Modelo C", 10)),
  RMSE = c(
    # Tus valores RMSE para Modelo A (10 valores)
    1.15, 1.09, 1.08, ...,
    # Tus valores RMSE para Modelo B (10 valores)
    1.36, 1.24, 1.17, ...,
    # Tus valores RMSE para Modelo C (10 valores)
    1.74, 1.63, 1.51, ...
  )
)
```

## 📦 Dependencias

### **Paquetes Requeridos:**

```r
install.packages(c(
  "tidyverse",    # Manipulación de datos
  "ggpubr",       # Visualizaciones estadísticas
  "rstatix",      # Pruebas estadísticas modernas
  "xtable",       # Exportación a LaTeX
  "car",          # Prueba de Levene
  "PMCMRplus",    # Pruebas post-hoc avanzadas
  "knitr"         # Formateo de reportes
))
```

### **Versiones Compatibles:**

- R ≥ 4.0.0
- RStudio (opcional pero recomendado)

## 📈 Interpretación de Resultados

### **Prueba de Friedman Significativa (p < 0.05):**

✅ **HAY diferencias entre modelos** → Proceder con análisis post-hoc

### **Prueba de Friedman No Significativa (p ≥ 0.05):**

❌ **NO hay diferencias entre modelos** → Todos son estadísticamente equivalentes

### **Análisis Post-hoc:**

- **p.adj < 0.05:** Diferencia significativa entre el par de modelos
- **p.adj ≥ 0.05:** No hay diferencia significativa entre el par

### **Interpretación Visual (Boxplot):**

- **Posición vertical:** Más abajo = mejor rendimiento (menor RMSE)
- **Altura de cajas:** Mayor altura = mayor variabilidad
- **Solapamiento:** Indica posibles diferencias no significativas
- **Outliers:** Puntos rojos = valores atípicos

## 📄 Ejemplo de Salida

```
=== INTERPRETACIÓN DE FRIEDMAN ===
✓ RESULTADO SIGNIFICATIVO (p = 0.000123 < 0.05)
  Conclusión: HAY diferencias significativas entre modelos
  Implicación: Algunos modelos son estadísticamente mejores que otros
  Siguiente paso: Realizar comparaciones post-hoc

=== CONCLUSIONES DEL ANÁLISIS ===
📊 Mejor modelo (menor RMSE mediano): Modelo 5
📊 Peor modelo (mayor RMSE mediano): Modelo 3
📊 Diferencia en medianas: 1.4321
📊 Significancia estadística: SÍ confirmada (Friedman p < 0.05)

🎯 RECOMENDACIÓN FINAL:
Basado en el análisis estadístico y visual, Modelo 5 muestra el mejor rendimiento
para la tarea de predicción de series temporales evaluada.
```

## 🔬 Aplicaciones

Este framework es ideal para:

- **Comparación de algoritmos de machine learning**
- **Evaluación de hiperparámetros**
- **Análisis de series temporales**
- **Estudios de benchmarking**
- **Investigación académica**
- **Reportes técnicos**

## 📚 Referencias Metodológicas

- **Julca Mejía, W. (2024).** Sistema de recomendación basado en modelos híbridos de filtrado colaborativo para jueces de programación en línea.

- **Julca-Mejia, W., & Paucar-Curasma, H. (2023, August).** A Cloud Based Recommender System for Competitive Programming Platforms with Machine and Deep Learning. _In Congresso sobre Tecnologias na Educação (Ctrl+ e) (pp. 11-20). SBC_.

- **Friedman, M. (1937).** The use of ranks to avoid the assumption of normality implicit in the analysis of variance. _Journal of the American Statistical Association_.

- **Demšar, J. (2006).** Statistical comparisons of classifiers over multiple data sets. _Journal of Machine Learning Research_.

- **García, S., & Herrera, F. (2008).** An extension on "statistical comparisons of classifiers over multiple data sets" for all pairwise comparisons. _Journal of Machine Learning Research_.

## 👨‍💻 Autor

**Wilson Julca Mejía**

- 📧 Email: [wilson.julca@example.com]
- 🎓 Investigación en Machine Learning y Análisis Estadístico
- 📊 Especialista en Comparaciones Estadísticas de Algoritmos

## 📄 Citación

Si utilizas este framework en tu investigación, por favor cita:

```bibtex
@misc{julca2024statistical,
  title={Framework de Análisis Estadístico Comparativo para Modelos de Machine Learning},
  author={Julca-Mejía, Wilson},
  year={2024},
  howpublished={\url{[URL_DEL_REPOSITORIO]}},
  note={Accessed: [FECHA]}
}
```

## 📜 Licencia

Este proyecto está bajo Licencia Académica - ver el archivo [LICENSE.txt](LICENSE.txt) para detalles.

**Uso Permitido:**

- ✅ Investigación académica
- ✅ Propósitos educativos
- ✅ Estudios científicos
- ✅ Modificación y distribución (con atribución)

**Uso Restringido:**

- ❌ Uso comercial sin autorización
- ❌ Distribución sin atribución

// ...existing code...

## 🤝 Contribuciones

### 📋 **Política de Contribuciones:**

#### ✅ **Contribuciones Permitidas:**

- Corrección de errores (bugs)
- Mejoras en documentación
- Optimizaciones de código
- Traducciones a otros idiomas
- Ejemplos educativos adicionales

#### 📝 **Proceso para Contribuir:**

2. **Para contribuciones menores** (correcciones, documentación):

   - Fork el repositorio
   - Crea una rama (`git checkout -b fix/bug-description`)
   - Realiza los cambios
   - Commit con mensaje descriptivo
   - Abre un Pull Request con descripción detallada

3. **Condiciones de Contribución:**
   - Al contribuir, aceptas que tu código se incluya bajo la misma licencia académica
   - Las contribuciones mayores requerirán co-autoría en publicaciones derivadas
   - El autor original mantiene los derechos de distribución

### 🎓 **Para Investigadores y Académicos:**

Si deseas usar este framework como base para tu propia investigación:

1. **Cita apropiadamente** las publicaciones originales del autor
2. **Contacta al autor** para colaboraciones formales
3. **Considera co-autoría** para extensiones significativas

## 📞 Soporte

¿Tienes preguntas o necesitas ayuda?

- 📧 **Email:** [wilson.julca@unmsm.edu.pe]
- 🐛 **Issues:** [GitHub Issues]
- 📖 **Documentación:** Ver comentarios en el código
- 💬 **Discusiones:** [GitHub Discussions]

---

<div align="center">

**⭐ ¡Si este proyecto te fue útil, considera darle una estrella! ⭐**

Desarrollado con ❤️ para la comunidad de investigación en Machine Learning

</div>
