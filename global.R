
require(treemap)
require(dplyr)
require(shiny)
require(gridBase)
require(RColorBrewer)
require(plotly)
require(leaflet)
require(flifo)
require(lazyeval)
library(d3Tree)
require(crosstalk)
require(shinyWidgets)
require(shinyjs)


ejecucionMes <- read.csv('EjecucionMensual.csv')
entidad <- read.csv('Sabana_de_prueba_TF.csv', sep =  ';')
datos_intitucional <- read.csv('Jerarquia_Entidad.csv', sep=";")
datos_finalidad <- read.csv('Jerarquia_Finalidad.csv', sep= ';')
datos_geografico <- read.csv('Jerarquia_Geografico.csv', sep=';')
datos_objeto_gasto <- read.csv('Jerarquia_ObjetoGasto.csv', sep=';')
datos_economico <- read.csv('Jerarquia_Economico.csv', sep=';')



opciones_filtro_inicio <- list("Sub.Grupo", 
                               "Entidad",
                               "Finalidad",
                               "Región",
                               "Grupo",
                               "Económico.Nivel.1" 
)

mascara_filtro_inicio <- list("Sub grupo (Institucional)",
                              "Institución",
                              "Finalidad",
                              "Clasificación Geográfica",
                              "Objeto del gasto" ,
                              "Económico del gasto" 
)



