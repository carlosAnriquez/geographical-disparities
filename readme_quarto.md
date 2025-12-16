# Proyecto de Paper Académico en Quarto

Este proyecto contiene un paper académico reproducible en formato PDF y HTML utilizando Quarto.

## Estructura de Archivos

- `index.qmd`: Archivo principal del documento con todo el contenido
- `_quarto.yml`: Configuración del proyecto Quarto
- `styles.css`: Estilos CSS para la versión HTML
- `custom.scss`: Estilos SCSS personalizados
- `references.bib`: Referencias bibliográficas en formato BibTeX
- `senderos.R`: Script R con el código de análisis de datos
- `input/df_filtrado.rds`: Datos de entrada (asegúrate de tener esta carpeta)

## Requisitos

1. **R** (versión 4.0 o superior)
2. **RStudio** (recomendado)
3. **Quarto** (https://quarto.org/docs/get-started/)
4. **Paquetes R necesarios**:
   - pacman
   - dplyr
   - ggplot2
   - tidyverse
   - tibble
   - scales
   - purrr
   - gt
   - sessioninfo
   - sjPlot
   - stringr
   - knitr

en R:
```r
install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tidyverse, tibble, scales, purrr, 
               gt, sessioninfo, sjPlot, stringr, knitr)
```

## Cómo compilar el documento

### Opción 1: Usando RStudio

1. Abre el archivo `index.qmd` en RStudio
2. Haz clic en el botón "Render" (o presiona Ctrl+Shift+K)
3. Selecciona el formato deseado (HTML o PDF)

### Opción 2: Usando la Terminal

Para generar ambos formatos:
```bash
quarto render index.qmd
```

Para generar solo HTML:
```bash
quarto render index.qmd --to html
```

Para generar solo PDF:
```bash
quarto render index.qmd --to pdf
```

### Opción 3: Renderizar todo el proyecto

```bash
quarto render
```

## Formatos de Salida

### PDF
- Fuente: Times New Roman 12pt
- Interlineado: 1.5
- Márgenes: 1 pulgada en todos los lados
- Texto justificado
- Numeración de secciones
- Tabla de contenidos

### HTML
- Diseño responsive
- Código plegable (click para expandir)
- Tabla de contenidos navegable
- Figuras interactivas
- Estilo académico consistente

## Personalización

### Cambiar el Título o Autores

Edita el encabezado YAML en `index.qmd`:
```yaml
---
title: "Tu Título Aquí"
author: 
  - name: "Tu Nombre"
    affiliation: "Tu Institución"
    email: "tu.email@institucion.cl"
---
```

### Modificar Estilos CSS

Edita `styles.css` para cambiar:
- Colores
- Tamaños de fuente
- Espaciados
- Formato de tablas

### Cambiar Configuración del PDF

Edita `_quarto.yml` en la sección `pdf:` para modificar:
- Tamaño de márgenes
- Interlineado
- Tamaño de fuente
- Formato de bibliografía

## Estructura del Contenido

El documento incluye las siguientes secciones:

1. **Resumen**: Síntesis del estudio
2. **Marco Conceptual**: Base teórica y revisión de literatura
3. **Metodología**: Descripción de datos y análisis
4. **Resultados**: Análisis descriptivos y visualizaciones
5. **Conclusiones**: Hallazgos principales e implicaciones

## Agregar Nuevas Referencias

Edita el archivo `references.bib` y añade referencias en formato BibTeX. Luego cítalas en el texto usando `[@clave_cita]`.

## Solución de Problemas

### Error: "pandoc not found"
Instala Quarto desde https://quarto.org/docs/get-started/

### Error: "pdflatex not found"
Instala TinyTeX ejecutando en R:
```r
install.packages("tinytex")
tinytex::install_tinytex()
```

### Error: "package X not found"
Instala el paquete faltante:
```r
install.packages("nombre_paquete")
```

### Problema con fuentes en PDF
Si Times New Roman no está disponible, el sistema usará una fuente serif equivalente automáticamente.

## Licencia y Contacto

[Añade aquí tu información de licencia y contacto]

## Notas Adicionales

- Los archivos de salida se generan en la carpeta `_output/`
- Puedes cambiar esta ubicación editando `output-dir` en `_quarto.yml`
- Para una compilación más rápida durante el desarrollo, puedes usar solo formato HTML
