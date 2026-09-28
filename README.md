# Replicación: producción bibliográfica del DCP

Paquete autocontenido para reproducir los análisis y figuras de la entrada sobre las trayectorias de publicación del plantel del Departamento de Ciencia Política de la Universidad de la República, con corte a septiembre de 2026.

## Reproducción

Requisitos: R 4.5 o posterior y los paquetes `data.table`, `ggplot2`, `scales`, `patchwork`, `cld3` y `stringi`.

Desde esta carpeta:

```bash
Rscript run_all.R
```

El comando elimina los productos anteriores y vuelve a crear todos los archivos de `outputs/tables` y `outputs/figures`. También ejecuta controles de integridad y se detiene si alguno falla. No requiere archivos externos a esta carpeta.

## Estructura

- `data/`: cinco CSV fuente limpios y su diccionario. Los subconjuntos analíticos se reconstruyen en memoria al ejecutar el código.
- `R/`: funciones, cuatro secciones analíticas y controles de validación.
- `outputs/tables/`: indicadores y tablas finales en CSV.
- `outputs/figures/`: figuras finales en PNG.
- `outputs/session_info.txt`: versiones de R, plataforma y paquetes cargados.

## Universo y unidades

- Los CSV fuente conservan el plantel institucional completo de 51 integrantes.
- El plantel institucional comprende 51 integrantes. El universo sustantivo se restringe a los 43 docentes con `dt_2026` igual a 0 o 1, es decir, con funciones de investigación; se excluyen los ocho casos con `dt_2026 == -1`.
- Las publicaciones se observan para los 37 integrantes de ese universo con CVuy público y accesible. Los seis casos sin CVuy accesible se mantienen en los perfiles institucionales, pero no se incorporan como producción cero.
- El análisis bibliométrico se concentra exclusivamente en 858 artículos publicados o aceptados en revistas académicas; 231 corresponden a 2021–2025.
- Los análisis temporales terminan en 2025 porque 2026 es un año incompleto.
- Las tendencias históricas describen las trayectorias del plantel actual. No equivalen a la producción histórica completa del departamento.

## Conteos

- Los conteos de artículos usan cada `article_id` una sola vez.
- Para atribuir artículos a grados se usa `article_weight_analysis`: cada artículo suma uno en total, distribuido entre los integrantes observados que lo firman.
- La productividad por investigador usa conteo completo a nivel individual: un artículo aparece una vez en la trayectoria de cada coautor observado del DCP. Se considera activo en un quinquenio a quien ya había publicado su primer artículo antes de finalizar ese período.
- La coautoría se reconstruye contrastando `authors_full` con los 51 nombres del plantel y con los vínculos autor–artículo canónicos. El archivo `03_auditoria_coautoria_articulos.csv` permite revisar cada decisión.
- El idioma se estima a partir del título mediante `cld3`; es una medida aproximada.
- Los rankings corresponden a las ediciones disponibles en 2025–2026 y describen la posición actual de las revistas, no necesariamente su posición en el año de publicación.

## Datos incluidos

- `authors.csv`: plantel institucional y atributos individuales.
- `articles.csv`: artículos únicos y metadatos bibliométricos.
- `author_article.csv`: vínculos entre integrantes y artículos.
- `journals.csv`: revistas e indicadores de indexación y posición.
- `publications_academic.csv`: universo deduplicado de productos académicos.

Estos son los únicos datos de entrada. No se incluyen archivos heredados, bases duplicadas ni productos intermedios de preparación.
