# Proyección del PIB: Análisis del Gasto en Consumo Final mediante Modelos SARIMA

Este proyecto tiene como objetivo modelar y predecir el comportamiento del Producto Interno Bruto (PIB) de Colombia, específicamente el componente de **gasto en consumo final**, utilizando técnicas de análisis de series temporales, particularmente modelos **SARIMA**.

## 📌 Objetivo del estudio

Desarrollar un modelo de pronóstico para el gasto en consumo final utilizando la metodología Box-Jenkins, con el fin de apoyar la toma de decisiones económicas y estratégicas basadas en predicciones confiables.

## 🧩 Datos utilizados

- Fuente: [DANE](https://www.dane.gov.co)
- Serie: Gasto en consumo final del PIB (trimestral)
- Periodo: Desde el primer trimestre de 2005 hasta el segundo trimestre de 2023
- Observaciones: 74
- Unidad: Miles de millones de pesos colombianos

## ⚙️ Metodología

1. Análisis gráfico y exploratorio
2. Pruebas de estacionariedad (KPSS)
3. Diferenciación regular y estacional
4. Identificación de parámetros mediante ACF, PACF y EACF
5. Ajuste de modelos SARIMA y AutoARIMA
6. Validación cruzada con horizontes de pronóstico de 2 y 4 trimestres
7. Evaluación del desempeño mediante métricas como RMSE, MAE, MAPE y Theil’s U

## 📊 Principales resultados

- Se ajustaron y compararon múltiples modelos SARIMA para distintos horizontes.
- El modelo **AutoARIMA(1,0,2)(0,1,1)[4]** mostró mejor desempeño general.
- Se aplicaron pruebas de diagnóstico (residuos, normalidad, autocorrelación, BDS) que validaron los supuestos del modelo.
- El modelo SARIMA se recomendó sobre ARIMA por su mayor precisión en pruebas de pronóstico.

## 📈 Herramientas utilizadas

- **R** (series temporales, `forecast`, `TSA`)
- Métodos estadísticos: ARIMA, SARIMA, validación cruzada, pruebas KPSS, ARCH-LM, BDS, Ljung-Box

## 👥 Autores

- **Fabián Barrios** – Universidad de Córdoba  
- **Camilo Lozano** – Universidad de Córdoba

## 📅 Fecha

Junio de 2024
