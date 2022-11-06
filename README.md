# Tarea 2
#### Instalar librerías

```{r}
install.packages("readr")
install.packages("tidyverse")
install.packages("data.table")
install.packages("plotly")
library(readr)
library(tidyverse)
library(data.table)
library(plotly)
```

### Preparar el entorno.

```{r}
# Cargar CSV
# con la función read_delim() de readr
covid_general <-
  read_delim(
    file = "C:/Users/Inspiron 15-3558/Desktop//tarea_2/05_30_22_CSV_GENERAL.csv",
    col_select = c(
      "FECHA",
       "hom_posi",
      "muj_posi",
      "menor_posi",
      "adul_posi",
      "am_posi",
      "nue_posi")
  )
   
```

### Cambiar de nombre a las columnas.

```{r}
covid_general <-
  covid_general |>
  rename(
    fecha = FECHA,
     hombres_positivos =  hom_posi,
     mujeres_positivas = muj_posi,
    menores_positivos = menor_posi,
    adultos_positivos = adul_posi,
    acumulado_adultos_mayores_positivos = am_posi,
    nuevos_positivos = nue_posi
   
  )
```

### Cambiar fecha

```{r}
covid_general <-
  covid_general |>
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))
  
```

### Cambio de tipo de datos de la columna fecha, de str a date #ISO 8601

```{r}
covid_general <-
  covid_general |>
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))
```

### Eliminación de columnas no necesarias.

```{r}
covid_general <- drop_na(data = covid_general)
```

# Tabla 1: 

```{r}
covid_general |>
data.table(options = list)
pageLength = 10
language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
  
```

## Gráfico 1. asos positivos nuevos por dí, del 03/06/2020 al 30/05/2022.

```{r}
grafico_barras <-
covid_general |>
  ggplot(aes(x = fecha, y = nuevos_positivos)) +
  geom_col(color= "cadetblue3") +
  ggtitle("Cantidad de casos positivos nuevos diarios de Covid-19 en 
          Costa Rica del 03/06/2020 al  30/05/2022.") +
  xlab("Fecha") +
  ylab("Cantidad de casos nuevos") 
 grafico_barras 
 
 ### Gráfico interactivo
 ggplotly(grafico_barras) |> 
  config(locale = 'es')
```

## Gráfico 2. Evolución temporal de los casos positivos acumulados de hombres y de los casos positivos acumulados de mujeres.

```{r}
grafico_linea <- 
  covid_general |> 
  ggplot()+
  geom_line(aes(x = fecha, y = hombres_positivos/10, colour = "Hombres"))+
  geom_line(aes(x =fecha, y = mujeres_positivas/10, colour = "Mujeres"))+
  scale_color_manual(name = "Casos  positivos acumulados", values = c("Hombres" = "cadetblue3", "Mujeres" = "coral3"))+
  ggtitle("Evolución temporal de los casos positivos de Covid-19 
  acumulados de hombres y mujeres Costa Rica del 03/06/2020 al 30/05/2022.") +
  xlab("Fecha") +
  ylab("Casos acumulados") 
grafico_linea
### Gráfico interactivo
ggplotly(grafico_linea) |> 
  config(locale = 'es')
```

## Gráfico 3. Casos positivos acumulados en menores, adultos y adultos mayores.

```{r}
grafico_etario <- 
  covid_general |> 
  ggplot()+
  geom_line(aes(x = fecha, y = menores_positivos/10,colour="Menores de Edad"))+
  geom_line(aes(x =fecha, y = adultos_positivos/10, colour = "Adultos"))+
  geom_line(aes(x =fecha, y = acumulado_adultos_mayores_positivos/10, colour = "Adultos Mayores"))+
  scale_color_manual(name = "Casos acumulados por grupo etario", values = c("Menores de Edad" = "aquamarine4", "Adultos" = "cadetblue3", "Adultos Mayores" = "chocolate1"))+
  ggtitle("Evolución temporal de los casos positivos acumulados 
  menores de edad, adultos y adultos mayores en  
  Costa Rica 3/06/2020 al 30/05/2022.") +
  xlab("Fecha") +
  ylab("Casos acumulados") 
grafico_etario
### Gráfico interactivo
ggplotly(grafico_etario) |> 
  config(locale = 'es')
```

### Casos de Covid 19 Positivos

```{r}
# Carga del archivo CSV de entrada en un dataframe
# con la función read_delim() de readr
covid_cantonal_positivos <-
  read_delim(
    file = "C:/Users/Inspiron 15-3558/Desktop//tarea_2/05_30_22_CSV_POSITIVOS.csv",
    locale = locale(encoding = "WINDOWS-1252"), # para procesar las tildes
    col_select = c("provincia", "canton", "30/05/2022")
  )
# Cambio de nombre de columnas
covid_cantonal_positivos <-
  covid_cantonal_positivos |>
  rename(
    positivos = '30/05/2022'
  )
# Borrado de filas con valor NA u "Otros"
# en la columna canton
covid_cantonal_positivos <-
  covid_cantonal_positivos |>
   filter(!is.na(canton) & canton != "Otros")
```

## Tabla 2: Casos positivos de Covid pro provincia y cantón.

```{r}
# Tabla de datos de COVID cantonales positivos
covid_cantonal_positivos |> 
  data.table(options = list (
    pageLength = 10,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
  ))
 
```

## Histograma de distribución de los casos positivos en cantones

```{r}
# Histograma ggplot2 de distribución del PIB per cápita en 2007
histograma_cantonal <-
covid_cantonal_positivos |>
  ggplot(aes(x = positivos, fill = canton)) +
  geom_histogram(aes(y = ..density..))+
  geom_histogram(bins = 5)+
  ggtitle("Frecuencia de casos pro cantón")+
  xlab("Distribución de casos positivos Covid-19 por cantón.") +
  ylab("Frecuencia") +
  labs(caption = "Fuente: https://oges.ministeriodesalud.go.cr/.")
histograma_cantonal 
### Histograma interactivo
ggplotly(histograma_cantonal) |> 
  config(locale = 'es')
  
```
