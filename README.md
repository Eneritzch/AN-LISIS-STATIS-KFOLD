# Análisis Estadístico con K-Fold en R

## 1. Descripción del proyecto
Este proyecto realiza un análisis estadístico de un dataset utilizando técnicas de validación cruzada K-Fold. Permite explorar los datos, generar gráficos, calcular métricas estadísticas y facilitar la interpretación de resultados de manera interactiva mediante Shiny.

El proyecto está desarrollado en R, integrando librerías para análisis, visualización y procesamiento de datos.

## 2. Objetivo
El objetivo principal de este proyecto es implementar un flujo de trabajo reproducible para el análisis estadístico de datasets, que incluya:

Limpieza y preparación de datos.

Exploración y visualización de la información.

Aplicación de técnicas de validación cruzada K-Fold.

Presentación interactiva de resultados mediante Shiny.

## Dependencias necesarias

El proyecto requiere las siguientes librerías de R:

- **Interfaz y dashboard**:  
  - shiny  
  - shinydashboard  
  - shinyWidgets  

- **Lectura y manipulación de datos**:  
  - readxl  
  - dplyr  
  - tidyr  

- **Análisis multivariante**:  
  - ade4  

- **Tablas interactivas**:  
  - DT  

- **Visualización**:  
  - ggplot2  
  - ggrepel  
  - plotly  
  - pheatmap  
  - RColorBrewer  
  - gridExtra  

## Instalación

1. Clonar el repositorio:

git clone https://github.com/tu-usuario/nombre-del-repositorio.git


Abrir el proyecto en RStudio.

Instalar las librerías necesarias (ver sección Dependencias).

## 6. Ejecución

Abrir app.R en RStudio.

Ejecutar la aplicación (botón Run App).

Subir un archivo .xlsx con tu dataset o usar el ejemplo incluido en data/.

Explorar las pestañas de análisis, gráficos y resultados.

## 7. Dataset

El proyecto utiliza archivos Excel (.xlsx).

El dataset debe contener columnas con variables numéricas para análisis estadístico.