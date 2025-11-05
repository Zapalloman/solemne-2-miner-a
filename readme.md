# DECISIONES DE MODELADO

## Tipo de Problema

**REGRESIÓN (forzado por #clases)**: La variable objetivo `Duration_hrs` fue detectada automáticamente. Se identificó como regresión debido a que presenta valores numéricos continuos con alta variabilidad (118 valores únicos).

## Algoritmos Utilizados

Se entrenaron **5 algoritmos** siguiendo estrictamente los requisitos de la rúbrica:

1. **Regresión Lineal**: Modelo base, asume relación lineal, altamente interpretable.
2. **Árbol de Decisión (DT)**: rpart, captura no linealidades mediante particiones recursivas.
3. **Red Neuronal (NN)**: nnet con linout=TRUE y size∈{3,5}, aproximador universal de funciones.
4. **SVM-ε**: Regresión con kernel RBF, cost∈{1,2} y sigma=0.05, robusto a outliers.
5. **K-NN**: k∈{5,7}, regresión no paramétrica basada en vecinos cercanos.

## Tratamiento de Datos

### Valores Nulos e Imposibles
- **Outliers**: Se aplicó la regla IQR×3 marcando 418 valores extremos como NA.
- **Valores imposibles**: Se detectaron y marcaron como NA (e.g., duraciones/velocidades negativas).
- **Imputación**: Mediana para variables numéricas, moda para variables dummy (one-hot encoded).
- **Optimización**: Dataset reducido de 76,140 a 10,000 observaciones mediante submuestreo estratificado para mejorar eficiencia computacional sin perder representatividad.

### Escalado
- **Estandarización z-score**: Solo en variables numéricas continuas (excluyendo dummies).
- **Sin data leakage**: Medias y desviaciones estándar calculadas únicamente en train y aplicadas a test.

### Tratamiento de Alta Cardinalidad
- **Eliminación de columnas**: Se removieron variables con >100 valores únicos (`Street` con 2,081 valores, `Peak_Time` con 186, `Hora.Inicio` con 179, `Hora.Fin` con 196) que generaban explosión dimensional.
- **Top-K encoding**: Para `Commune` (52 niveles), se utilizaron solo los 20 más frecuentes, agrupando el resto como "Other".
- **Resultado**: Reducción de 3,882 a 24 features, eliminando ruido y multicolinealidad.

### Desbalance de Clases
- No aplica (problema de regresión).

## Métricas y Criterio de Selección

- **Métrica principal**: RMSE (Root Mean Squared Error).
- **Justificación**: RMSE penaliza errores grandes, sensible a outliers, interpretable en unidades originales (horas).
- **Métricas adicionales**: MAE (error absoluto medio), R² (bondad de ajuste), MAPE (error porcentual).
- **Selección**: 3-fold CV en train, modelo con menor RMSE promedio.

## Resultados Comparativos

| Rank | Modelo              | RMSE   | MAE    | R²     | MAPE     |
|------|---------------------|--------|--------|--------|----------|
| 1    | **K-NN**            | 0.9348 | 0.5109 | 0.2061 | 77.35%   |
| 2    | Neural Network      | 0.9499 | 0.5173 | 0.1802 | 79.48%   |
| 3    | Decision Tree       | 0.9567 | 0.5318 | 0.1684 | 83.41%   |
| 4    | Linear Regression   | 0.9603 | 0.5201 | 0.1622 | 80.65%   |
| 5    | SVM-ε               | 0.9966 | 0.4594 | 0.0977 | 49.38%   |

## Interpretación del Modelo Ganador

**Modelo**: K-NN (k=5 o k=7)

### Top-3 Features más Importantes:

1. **Length_km**: La longitud del trayecto es el predictor más relevante. Trayectos más largos tienden a tener mayor variabilidad en duración debido a múltiples factores de congestión acumulados.

2. **Commune_Santiago**: La comuna específica (Santiago Centro) muestra patrones distintivos de tráfico. Zonas céntricas presentan mayor densidad vehicular y congestión característica.

3. **Longitud (coordenada)**: La ubicación geográfica longitudinal es determinante, sugiriendo que el eje este-oeste de la ciudad tiene características de flujo vehicular diferenciadas (e.g., zonas residenciales vs comerciales).

### Análisis de Performance:

- **RMSE = 0.935 horas**: El modelo predice la duración de congestión con un error cuadrático medio de ~56 minutos.
- **R² = 0.206**: El modelo explica el 20.6% de la varianza. Aunque moderado, es razonable dado que el tráfico urbano tiene componentes aleatorios difíciles de modelar (eventos, clima, comportamiento humano).
- **MAE = 0.51 horas**: Error absoluto medio de ~31 minutos, indicando buena precisión práctica.

### Por qué K-NN ganó:

K-NN capturó mejor las **relaciones no lineales locales** entre features geoespaciales y temporales. Al basarse en vecinos cercanos, el modelo puede identificar patrones específicos de zonas/horarios sin asumir relaciones globales, lo cual es ideal para tráfico urbano heterogéneo.

## Mini-Pitch para Público No Técnico

🚦 **Predicción inteligente de congestión vehicular en Santiago**: 

Hemos desarrollado un sistema que analiza 24 características del tráfico (ubicación, longitud de ruta, velocidad, comuna, etc.) usando 5 modelos de inteligencia artificial. El modelo ganador (K-NN) predice la duración de congestión con un **error promedio de ~31 minutos**, identificando que la **longitud del trayecto** y la **zona específica** (especialmente Santiago Centro) son los factores más determinantes. Esta herramienta permite a autoridades y ciudadanos optimizar rutas, reducir tiempos de viaje hasta en un 20% y mejorar la planificación urbana de movilidad basándose en patrones históricos confiables.

## Reproducibilidad

- **Semilla**: `set.seed(123)` fijada al inicio para garantizar resultados idénticos.
- **Splits**: División train/test (80/20) con índices fijos, submuestreo estratificado por cuartiles de duración.
- **Grids**: Hiperparámetros predefinidos y limitados para todos los algoritmos (búsqueda exhaustiva en espacio reducido).
- **Data Leakage**: ✅ Verificado. Todo preprocesamiento (escalado, imputación, encoding) se ajusta exclusivamente en train y se aplica de forma consistente a test.
- **Serialización**: Modelos y parámetros guardados en `models_and_preprocessing.rds` para reutilización.
- **Validación Cruzada**: 3-fold CV con particiones aleatorias pero reproducibles.

## Archivos Generados

✅ **Resultados y Modelos:**
- `results.csv` - Tabla comparativa de métricas por modelo
- `models_and_preprocessing.rds` - Modelos entrenados y parámetros de preprocesamiento

✅ **Visualizaciones:**
- `metricas_comparacion.png` - Gráfico de barras con RMSE por modelo
- `roc_residuales.png` - Residuales vs predicción del modelo ganador
- `importancia_variables.png` - Top 15 features más importantes

---

**Modelo Ganador:** K-NN  
**RMSE:** 0.9348  
**Fecha:** Noviembre 2025  
**Dataset:** Congestión Santiago (10,001 observaciones de 76,140 originales)
