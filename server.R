#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

datos_economico <- read.csv('Jerarquia_Economico.csv', sep=';')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Variables para uso en esta sesión ---------------------------------------
  
  mapaJson <-  NULL
  datos_tabla <- NULL
  tabla_temporal <- NULL
  tabla_dinamica <- NULL
  datos_grafica <- SharedData$new(tabla_temporal)
  
  
  # Setting up the environmet for global variables --------------------------
  app.env <- new.env()
  
  app.env$nivelActivo <- ""
  
  jerarquia_institucional_ida = lifo()
  push(jerarquia_institucional_ida, "Unidad.Ejecutora" )
  push(jerarquia_institucional_ida, "Entidad" )
  
  jerarquia_institucional_regreso = lifo()
  
  valores_institucional = list()
  
  jerarquia_finalidad_ida = lifo()
  push(jerarquia_finalidad_ida, "División")
  push(jerarquia_finalidad_ida, "Función")
  push(jerarquia_finalidad_ida, "Finalidad")
  
  
  jerarquia_finalidad_regreso = lifo()
  
  jerarquia_geografico_ida = lifo()
  push(jerarquia_geografico_ida, "Municipio")
  push(jerarquia_geografico_ida, "Departamento")
  push(jerarquia_geografico_ida, "Región")
  
  jerarquia_geografico_regreso = lifo()
  
  jerarquia_objeto_gasto_ida = lifo()
  push(jerarquia_objeto_gasto_ida, "Renglón")
  push(jerarquia_objeto_gasto_ida, "Sub.Grupo.Gasto")
  push(jerarquia_objeto_gasto_ida, "Grupo.Gasto")
  
  jerarquia_objeto_gasto_regreso = lifo()
  
  jerarquia_economico_ida = lifo()
  push(jerarquia_economico_ida, "Clasificación.Económica.Gasto")
  push(jerarquia_economico_ida, "Económico.Nivel.Operativo")
  push(jerarquia_economico_ida, "Económico.Nivel.4")
  push(jerarquia_economico_ida, "Económico.Nivel.3")
  push(jerarquia_economico_ida, "Económico.Nivel.2")
  #push(jerarquia_economico_ida, "Económico.Nivel.1")
  
  
  jerarquia_economico_regreso = lifo()
  
  
  dimension_ida = NULL # candidato a ser eliminado
  valor_dimension = NULL #candidato a ser eliminado
  
  jerarquia_dimension_regreso = list()  #cambio de estructura de datos
  jerarquia_valor_dimension_regreso = list() #antes era lifo, se pasa a lista
  
  
  filaSeleccionada <- 0
  
  output$condition <- renderText({
    condition()
  })
  
  output$tipoVisualizacion <- renderText({
    visualizacion()
  })
  
  output$tipoVisualizacion <- renderUI({
    input$filtro
    vector = c()
    v = visualizacion()
    if(v  == '1'){
      vector = c("Treemap" = "treemap",
                 "Tabla" = "tabla",
                 "Mapa departamental" = "Código.Departamento"
      )
    }else if(v == '0'){ 
      # vector = c("Treemap" = "treemap",
      #            "Tabla" = "tabla")
      vector = c(
                 "Tabla" = "tabla")
    }
    radioButtons("visualizacion","Escoja la forma de ver los datos",
                 choices = vector
    )
    #actionButton("", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
  })
  
  
  condition <- reactive({
    refresh = detalleGasto()
    retornoVisualizacion = input$visualizacion
    input$visualizacion
    result = 0
    if( !is.null(refresh)){
      if( is.null(retornoVisualizacion) || retornoVisualizacion != "Código.Departamento" ){
        if( refresh%%2 == 0 ){
          result = 0
        }else if( refresh%%2 == 1 && retornoVisualizacion == "treemap"  )
          result =1
        else if( refresh%%2 == 1 && retornoVisualizacion == "tabla")
          result = 3
        #gasto_tabla()
      }
      else if( retornoVisualizacion == "Código.Departamento")
        if( refresh%%2 == 0 ){
          result = 0
        }else if( retornoVisualizacion == "tabla" ){
          result = 3
          #gasto_tabla()
        }
      else
        result = 2
    }
    return(result)
  })
  
  
  visualizacion <- reactive({
    clasificacion = input$filtro
    result = 0
    if( !is.null(clasificacion) && clasificacion == "Código.Departamento"){
      result = 1
    }
    return(result)
  })
  
  
  outputOptions(output, 'condition', suspendWhenHidden=FALSE)
  
  porcentajeEjecucion <- reactive({
    ejecucionMes %>%
      select(Ejercicio, Devengado, Vigente ) %>%
      filter(Ejercicio == input$year ) %>%
      mutate(PorcentajeEje =  Devengado /   Vigente  * 100  ) 
  })
  
  
  
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Porcentaje de ejecución", paste( value = round(porcentajeEjecucion()$PorcentajeEje,1), "%" ) , icon = icon("percent"),
      color = "green", width = NULL
    )
  })
  
  
  
  output$devengadoBox <- renderInfoBox({
    infoBox(
      "Ejecución presupuestaria", paste("Q" , value = formatC( ejecucionMes[ejecucionMes$Ejercicio == input$year, ]$Devengado, format = "f", big.mark = ",", digits = 1) )  , icon = icon("money"),
      color = "green", width = NULL
    )
  })
  
  output$detalle <- renderUI({
    actionButton("detalleGasto", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
  })
  
  output$opcionesFiltro <- renderUI({
    radioButtons("filtro","Escoja la forma de ver los datos",
                 choices = c("Institución" = "Entidad",
                             "Finalidad" = "Finalidad",
                             "Clasificación geográfica" = "Código.Departamento",
                             "Objeto del gasto" = "Objeto del gasto",
                             "Económico del gasto" = "Economico"
                 ), selected="Entidad"
    )
    #actionButton("", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
  })
  
  
  
  
  detalleGasto=reactive({
    input$detalleGasto
  }) 
  
  
  gasto <- reactive({
    temp <- data.frame()
    if(!is.null(input$filtro)){
      temp <- entidad %>%
        select(Devengado, input$filtro)%>%
        group_by_(input$filtro) %>%
        summarise(devengado = sum(Devengado))
    }
    return(temp)
  })
  
  mapaAnterior <- reactiveValues(mapa = NULL, variable = NULL)
  tempo <- reactiveValues(temp=NULL)
  
  gastoTreeMap <- function()({
    
    
    temp <- NULL
    nivel <- getRecordFromTreeMap()
    # if( !is.null(isolate(mapaAnterior$variable)) && !is.null(nivel) ){
    #   if (nivel == isolate(mapaAnterior$variable) ){
    #     return(NULL)
    #   }
    # }
    #    
    
      
    if( !is.null(input$filtro) ){
      switch ( input$filtro,
              'Entidad' = {
                if ( !is.empty(jerarquia_institucional_ida) ){
                  filtro = pop(jerarquia_institucional_ida)
                  push(jerarquia_institucional_regreso, paste0("'",filtro, "'") )  
                }else{
                  return(-1)
                }
                
                if( is.null(nivel) || trimws(nivel) == "" ){
                  shinyjs::hide("atrasTM")
                  temp <- datos_intitucional %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  shinyjs::show("atrasTM")
                  va <- app.env$nivelActivo
                  temp <- datos_intitucional %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                valores_institucional <- list(valores_institucional, temp)
                valores_institucional <<- valores_institucional
                if(nrow(temp) >1 ){
                  jerarquia_institucional_ida <<- jerarquia_institucional_ida
                  jerarquia_institucional_regreso <<- jerarquia_institucional_regreso
                  app.env$nivelActivo = filtro  
                }else{
                  return(NULL)
                }
                
              }, 
              'Finalidad' = {
                filtro = pop(jerarquia_finalidad_ida)
                push(jerarquia_finalidad_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_finalidad %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_finalidad %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_finalidad_ida <<- jerarquia_finalidad_ida
                jerarquia_finalidad_regreso <<- jerarquia_finalidad_regreso
                app.env$nivelActivo = filtro
              }, 
              'Código.Departamento' = {
                filtro = pop(jerarquia_geografico_ida)
                push(jerarquia_geografico_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_geografico %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_geografico %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_geografico_ida <<- jerarquia_geografico_ida
                jerarquia_geografico_regreso <<- jerarquia_geografico_regreso
                app.env$nivelActivo = filtro
              }, 
              'Objeto del gasto' = {
                filtro = pop(jerarquia_objeto_gasto_ida)
                push(jerarquia_objeto_gasto_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_objeto_gasto %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_objeto_gasto %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_objeto_gasto_ida <<- jerarquia_objeto_gasto_ida
                jerarquia_objeto_gasto_regreso <<- jerarquia_objeto_gasto_regreso
                app.env$nivelActivo = filtro
              }, 
              'Economico' = {
                filtro = pop(jerarquia_economico_ida)
                push(jerarquia_economico_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_economico %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_economico %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_economico_ida <<- jerarquia_economico_ida
                jerarquia_economico_regreso <<- jerarquia_economico_regreso
                app.env$nivelActivo = filtro
              }
      )
    }
    tempo$temp <- temp
    return(temp)
  })
  

  
  
  observeEvent(input$atrasTM,{ 
               print("Hola")
               print(app.env$nivelActivo)
               print(mapaAnterior$filtro)
               switch ( input$filtro,
                        'Entidad' = {
                          temp = valores_institucional[ length(valores_institucional) -1 ]
                          temp = as.data.frame(temp[[1]][[2]])
                          filtro = pop(jerarquia_institucional_regreso)
                          push(jerarquia_institucional_ida, paste0("'",filtro, "'") )
                          jerarquia_institucional_ida <<- jerarquia_institucional_ida
                          jerarquia_institucional_regreso <<- jerarquia_institucional_regreso
                          # if ( !is.empty(jerarquia_institucional_regreso) ){
                          #   filtro = pop(jerarquia_institucional_regreso)
                          #   push(jerarquia_institucional_ida, paste0("'",filtro, "'") )  
                          # }else{
                          #   return(-1)
                          # }
                          # nivel = valores_institucional[length(valores_institucional)]
                          # if( is.null(nivel) || trimws(nivel) == "" ){
                          #   shinyjs::hide("atrasTM")
                          #   temp <- datos_intitucional %>%
                          #     select_(filtro, "Devengado") %>%
                          #     group_by_(filtro) %>%
                          #     summarise(Devengado = sum(Devengado)) 
                          # }else{
                          #   shinyjs::show("atrasTM")
                          #   va <- app.env$nivelActivo
                          #   temp <- datos_intitucional %>%
                          #     select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                          #     filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                          #     group_by_(filtro) %>%
                          #     summarise(Devengado = sum(Devengado))
                          # }
                          # 
                          # if(nrow(temp) >1 ){
                          #   jerarquia_institucional_ida <<- jerarquia_institucional_ida
                          #   jerarquia_institucional_regreso <<- jerarquia_institucional_regreso
                          #   app.env$nivelActivo = filtro  
                          # }else{
                          #   return(NULL)
                          # }
                          
                        }, 
                        'Finalidad' = {
                          filtro = pop(jerarquia_finalidad_ida)
                          push(jerarquia_finalidad_regreso, paste0("'",filtro, "'") )
                          if( is.null(nivel) || trimws(nivel) == "" ){
                            temp <- datos_finalidad %>%
                              select_(filtro, "Devengado") %>%
                              group_by_(filtro) %>%
                              summarise(Devengado = sum(Devengado)) 
                          }else{
                            va <- app.env$nivelActivo
                            temp <- datos_finalidad %>%
                              select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                              filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                              group_by_(filtro) %>%
                              summarise(Devengado = sum(Devengado))
                          }
                          jerarquia_finalidad_ida <<- jerarquia_finalidad_ida
                          jerarquia_finalidad_regreso <<- jerarquia_finalidad_regreso
                          app.env$nivelActivo = filtro
                        }, 
                        'Código.Departamento' = {
                          filtro = pop(jerarquia_geografico_ida)
                          push(jerarquia_geografico_regreso, paste0("'",filtro, "'") )
                          if( is.null(nivel) || trimws(nivel) == "" ){
                            temp <- datos_geografico %>%
                              select_(filtro, "Devengado") %>%
                              group_by_(filtro) %>%
                              summarise(Devengado = sum(Devengado)) 
                          }else{
                            va <- app.env$nivelActivo
                            temp <- datos_geografico %>%
                              select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                              filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                              group_by_(filtro) %>%
                              summarise(Devengado = sum(Devengado))
                          }
                          jerarquia_geografico_ida <<- jerarquia_geografico_ida
                          jerarquia_geografico_regreso <<- jerarquia_geografico_regreso
                          app.env$nivelActivo = filtro
                        }, 
                        'Objeto del gasto' = {
                          filtro = pop(jerarquia_objeto_gasto_ida)
                          push(jerarquia_objeto_gasto_regreso, paste0("'",filtro, "'") )
                          if( is.null(nivel) || trimws(nivel) == "" ){
                            temp <- datos_objeto_gasto %>%
                              select_(filtro, "Devengado") %>%
                              group_by_(filtro) %>%
                              summarise(Devengado = sum(Devengado)) 
                          }else{
                            va <- app.env$nivelActivo
                            temp <- datos_objeto_gasto %>%
                              select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                              filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                              group_by_(filtro) %>%
                              summarise(Devengado = sum(Devengado))
                          }
                          jerarquia_objeto_gasto_ida <<- jerarquia_objeto_gasto_ida
                          jerarquia_objeto_gasto_regreso <<- jerarquia_objeto_gasto_regreso
                          app.env$nivelActivo = filtro
                        }, 
                        'Economico' = {
                          filtro = pop(jerarquia_economico_ida)
                          push(jerarquia_economico_regreso, paste0("'",filtro, "'") )
                          if( is.null(nivel) || trimws(nivel) == "" ){
                            temp <- datos_economico %>%
                              select_(filtro, "Devengado") %>%
                              group_by_(filtro) %>%
                              summarise(Devengado = sum(Devengado)) 
                          }else{
                            va <- app.env$nivelActivo
                            temp <- datos_economico %>%
                              select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                              filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                              group_by_(filtro) %>%
                              summarise(Devengado = sum(Devengado))
                          }
                          jerarquia_economico_ida <<- jerarquia_economico_ida
                          jerarquia_economico_regreso <<- jerarquia_economico_regreso
                          app.env$nivelActivo = filtro
                        }
               )
               tempo$temp = temp
               a = 5
               
  }
               )
  
  output$treemap1 <- renderPlot({
    gastoTreeMap()
    temp = tempo$temp
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    variable <- names( temp )[1]
    if( !is.null(temp)  ){
      if( temp != -1 ){
        .tm <<- treemap(temp,
                        index= variable,
                        vSize="Devengado",
                        vColor="Devengado",
                        type="value",
                        title = "",
                        palette="Purples",
                        border.col ="white",
                        position.legend="right",
                        fontsize.labels = 16,
                        title.legend="Escala de colores")
        mapaAnterior$mapa <- temp
        mapaAnterior$variable <- variable
      }else{
        t <- mapaAnterior$mapa
        .tm <<- treemap(t,
                        mapaAnterior$variable,
                        vSize="Devengado",
                        vColor="Devengado",
                        type="value",
                        title = "",
                        palette="Purples",
                        border.col ="white",
                        position.legend="right",
                        fontsize.labels = 16,
                        title.legend="Escala de colores")
        if( !is.null(temp) ){
          showModal(modalDialog(
            title = "Seleccione...",
            "Por favor seleccione una fila de la tabla para continuar", footer = modalButton("Continuar"), easyClose = T
          ))  
        }
      }

    }else{
      t <- mapaAnterior$mapa
      .tm <<- treemap(t,
                      mapaAnterior$variable,
                      vSize="Devengado",
                      vColor="Devengado",
                      type="value",
                      title = "",
                      palette="Purples",
                      border.col ="white",
                      position.legend="right",
                      fontsize.labels = 16,
                      title.legend="Escala de colores")
      if( !is.null(temp) ){
        showModal(modalDialog(
          title = "Seleccione...",
          "Por favor seleccione una fila de la tabla para continuar", footer = modalButton("Continuar"), easyClose = T
        ))  
      }    
    }

    
  })
  
  
  treemap_clicked <- reactiveValues(
    center = NULL,
    for_condition=NULL
  )
  
  
  # Handle clicks on treemap by country
  observeEvent(input$click_treemap, {
    x <- input$click_treemap$x
    y <- input$click_treemap$y
    treemap_clicked$center <- c(x,y)
    
    if(is.null(treemap_clicked$for_condition)){
      treemap_clicked$for_condition=c(x,y)
    }
    else{treemap_clicked$for_condition=NULL}
    
  })
  
  getRecordFromTreeMap <- reactive({
    x <- treemap_clicked$center[1]
    y <- treemap_clicked$center[2]
    
    x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
    y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
    
    l <- tmLocate(list(x=x, y=y), .tm)
    z=l[, 1:(ncol(l)-5)]
    
    
    if(is.na(z[,1]))
      return(NULL)
    
    col=as.character(z[,1])
    return(col)
    #filter(pop_data,Country==col)
  })
  
  output$mapa <- renderLeaflet({
    if(is.null(mapaJson)){
      withProgress(message = "Está cargando el mapa", value = 0, {
        mapaJson <<- rgdal::readOGR(dsn ="guatemala.geojson")
      })
    }
    datos <- gasto()
    if( nrow(datos) > 0  ){
      informacionMapa <- sp::merge(mapaJson, datos, by.x="Código.Departamento" )
      qpal <- colorQuantile("YlGn", informacionMapa$devengado, n = 5, na.color = "#bdbdbd")
      
      
      lat <- 16
      lng <- -89.5
      zoom <- 7
      
      mapa <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        #addTiles()%>%
        setView(lat = lat, lng = lng, zoom = zoom)%>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = informacionMapa, fillColor = ~qpal(devengado), fillOpacity = 0.7,
                    color = "white", weight = 2)
    }
    
    
    
    
  })
  
  
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  # {
  #   print(paste("Los botones presionados son:", input$CBK_Button))
  #   datos <- as.data.frame( gasto_tabla() )
  #   tabla_temporal <- tabla_temporal %>%
  #     mutate(Acciones = shinyInput( dropdownButton, 1, 'dropDown_',
  #                                   checkboxGroupButtons(inputId = "btFiltro",label = "Elija un filtro",
  #                                                        choiceNames = mascara_filtro_inicio, 
  #                                                        choiceValues = opciones_filtro_inicio,
  #                                                        direction = 'vertical'
  #                                   ),icon = icon('gear'), status = 'primary') ) 
  #   tabla_inutil <<- DT::datatable( tabla_temporal,escape = F, selection = 'none' ,
  #                                   options = list(orderClasses = TRUE,
  #                                                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),  
  #                                   style = "bootstrap", callback = JS("")   ) %>% 
  #     DT::formatCurrency("Devengado", currency = "Q")
  # }
  
  actualizarFiltro <-  function(filtro="") {
    if( !is.null( tabla_dinamica )  ){
      
      valores_filtros <- sapply(colnames(tabla_dinamica),function( x ){
        y <- tabla_dinamica[,x] 
        if( !is.numeric(y) ){
          y <- as.factor( as.character(y) )
          if( nlevels(y) >  1  ){
            return(x)
          }
        }
      })
      
      
      
      valores_filtros <- plyr::compact(valores_filtros)
      
      if (exists("filtro")) {
        valores_filtros <- valores_filtros[valores_filtros != filtro]
      }else{
        valores_filtros <- valores_filtros[valores_filtros != dimension_ida]
      }
      
      valores_filtros <- obtenerListaFiltros(valores_filtros)
      
      #View(valores_filtros)
      
      
      if(length(valores_filtros) > 0 )
        updateRadioButtons(session,"opcionTabla", choices = valores_filtros, inline = T) 
      return(valores_filtros)
    }else{
      updateRadioButtons(session,"opcionTabla", choiceNames = opciones_filtro_inicio, choiceValues =  opciones_filtro_inicio, inline = T) 
      return(opciones_filtro_inicio)
    }
    
  }  
  
  output$Atras <- renderUI({
    actionButton("retroceder_tabla", icon = icon("glyphicon glyphicon-arrow-up", lib= "glyphicon"), label = "Nivel anterior")
  })
  
  output$comparadorAño <- renderUI({
    selectInput("yearCom","Ejercicio fiscal" ,choices = c(1998:format(Sys.Date(), "%Y")), selected = as.numeric(format(Sys.Date(), "%Y")) -1 )  
  })

  gasto_tabla <- function(filtro = '', variable = ''){
    if( is.null(datos_tabla)){
      withProgress(message ='Leyendo la información', value = 0, {
        datos_tabla <<- read.csv('datos_tabla.csv', sep = ';')  
      }) 
      
    }
    var <-  variable
    temporal <- NULL
    if( is.null(var) || var == '' ){
      temporal <- datos_tabla %>%
        summarise( Devengado = sum(Devengado) )%>%
        mutate(Concepto = 'Gasto Total') 
    }else{
      resultado <- tabla_temporal[var, "Concepto"] 
      if(resultado == "Gasto Total"){
        temporal <- datos_tabla %>%
          group_by_(filtro) %>%
          summarise(Devengado =  sum(Devengado))
        
        tabla_dinamica <<- datos_tabla
        temporal <- temporal %>%
          select_(filtro,"Devengado") %>%
          rename_(Concepto = filtro)
        
        dimension_ida <<- filtro
        #push(jerarquia_dimension_regreso,filtro)
        jerarquia_dimension_regreso <- c(jerarquia_dimension_regreso,filtro)
        #push(jerarquia_valor_dimension_regreso,"")
        jerarquia_valor_dimension_regreso <- "NADA"
        
        
        
        
      }else{
        col <- as.character(tabla_temporal$Concepto[variable] )
        dimension_actual <- dimension_ida
        .dots <- list(interp(~y==x, .values = list( y = as.name(dimension_actual), x = col ) ))
        tabla_dinamica <<- tabla_dinamica %>%
          filter_( .dots = .dots )
        
        temporal <- tabla_dinamica %>%
          group_by_(filtro) %>%
          summarise(Devengado =  sum(Devengado) ) %>%
          rename_( Concepto = filtro )
        dimension_ida <<- filtro # quizás lo correcto es filtro actualmente es dimension_actual
        #push(jerarquia_dimension_regreso, dimension_actual)
        jerarquia_dimension_regreso <- c(jerarquia_dimension_regreso, dimension_actual)
        valor_dimension <<- col
        #push(jerarquia_valor_dimension_regreso, col)
        jerarquia_valor_dimension_regreso <- c(jerarquia_valor_dimension_regreso, col)
      }
      
      
    }
    
    
    
    
    tabla_temporal <<- temporal
    
    #actualizarFiltro(filtro)
    
    jerarquia_valor_dimension_regreso <<- jerarquia_valor_dimension_regreso
    jerarquia_dimension_regreso <<- jerarquia_dimension_regreso
    
    return(temporal)
  }
  
  howManyRows <- 10
  
  values <- reactiveValues(
    datos = gasto_tabla() %>% 
      mutate( 
        Acciones = shinyInput( dropdownButton, 1, 'dropDown_',
                               checkboxGroupButtons(inputId = "btFiltro",label = "Elija un filtro",
                                                    choiceNames = mascara_filtro_inicio,
                                                    choiceValues = opciones_filtro_inicio,
                                                    direction = 'vertical'
                               ),icon = icon('gear'), status = 'primary', right = T)
      )
  )
  
  
  
  output$tabla <- DT::renderDataTable( { DT::datatable(values$datos, escape = F,
                                                       options=list(
                                                         language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                         preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                         drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')  )) %>% DT::formatCurrency("Devengado", currency = "Q")}
                                       , server=FALSE, escape=FALSE )
  
  
  
  columna <- function(filas){
    colum <- list()
    for ( i in seq_len(filas) ){
      colum[i] <- input[[paste0("btFiltroN",i)]]
    }
    colum
  }
  
  
  
  output$filtroTabla <- renderUI({
    radioButtons("opcionTabla","¿Qué filtro desea aplicar?",
                 choiceNames = opciones_filtro_inicio, selected="Entidad", choiceValues = opciones_filtro_inicio , inline = T
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  construirTablaDinamica <- function(){
    tabla_temp <- NULL
    if( length(jerarquia_dimension_regreso  )  == 1 ){
      tabla_temp <- datos_tabla %>%
        summarise( Devengado = sum(Devengado) )%>%
        mutate(Concepto = 'Gasto Total') 
    }else{
      contador <- 2:length(jerarquia_dimension_regreso)
      for(z in  contador ){
        a <- jerarquia_dimension_regreso[[z]]
        b <- jerarquia_valor_dimension_regreso[[z]]
        .dots <- list(interp( ~y==x, .values = list( y = as.name(jerarquia_dimension_regreso[[z]] ), x = jerarquia_valor_dimension_regreso[[z]] ) ) )                      
        if( z == length(jerarquia_dimension_regreso) ) {
          if( z == 2 ){
            tabla_dinamica <<- datos_tabla 
            tabla_temp <- datos_tabla %>%
              group_by_(jerarquia_dimension_regreso[[z]]) %>%
              summarise( Devengado = sum(Devengado) ) %>%
              rename_(Concepto = jerarquia_dimension_regreso[[z]])
            dimension_ida <<- jerarquia_dimension_regreso[[z]]
          }else{
            tabla_dinamica <<- tabla_temp
            tabla_temp <- tabla_temp %>%
              group_by_(jerarquia_dimension_regreso[[z]]) %>%
              summarise( Devengado = sum(Devengado) ) %>%
              rename_(Concepto = jerarquia_dimension_regreso[[z]])
            dimension_ida <<- jerarquia_dimension_regreso[[z]]
          }
        }else if( z == 2 ){
          tabla_temp <- datos_tabla %>%
            filter_(.dots = .dots)
        }else{
          tabla_temp <- tabla_temp %>%
            filter_(.dots = .dots)
        }
      }
    }
    #tomando el valor del último elemento de dimension
    last_filt = jerarquia_dimension_regreso[length(jerarquia_dimension_regreso)]
    #Quitando el ultimo elemento
    jerarquia_dimension_regreso <<- jerarquia_dimension_regreso[c(1:length(jerarquia_dimension_regreso) -1 )]
    jerarquia_valor_dimension_regreso <<- jerarquia_valor_dimension_regreso[c(1:length(jerarquia_valor_dimension_regreso) -1 )]
    #View(tabla_temp)
    tabla_temporal <<- tabla_temp
    return(list(tabla_temp,last_filt))
    #View(tabla_temporal)
    #View(tabla_dinamica)
  }
  
  
  obtenerListaFiltros <- function( valores ){
    listaExclusion <- NULL
    nombres <- names(valores)
    #View(nombres)
    if( "Sub.Grupo" %in%  nombres ){
      listaExclusion <- c(listaExclusion,"Entidad","Unidad.Ejecutora")
      nombres <- subset( nombres, !( nombres %in% c("Entidad","Unidad.Ejecutora") ) )
      
    }
    
    if( "Entidad" %in% nombres ){
      listaExclusion <- c(listaExclusion, "Unidad.Ejecutora" )
    }
    
    if( "Región" %in% nombres ){
      listaExclusion <- c(listaExclusion, c("Departamento", "Municipio") )
      nombres <- nombres[nombres != "Departamento"]
      nombres <- nombres[nombres != "Municipio"]
    }
    
    if( "Departamento" %in% nombres ){
      listaExclusion <- c(listaExclusion,  "Municipio" )
    }
    
    if( "Finalidad" %in% nombres ){
      listaExclusion <- c(listaExclusion, c("Función", "División") )
      nombres <- nombres[nombres != "Función"]
      nombres <- nombres[nombres != "División"]
    }
    
    
    if( "Función" %in% nombres ){
      listaExclusion <- c(listaExclusion,  "División" )
    }
    
    if( "Grupo.Gasto" %in% nombres ){
      listaExclusion <- c(listaExclusion, c("Sub.Grupo.Gasto", "Renglón") )
      nombres <- nombres[nombres != "Sub.Grupo.Gasto"]
      nombres <- nombres[nombres != "Renglón"]
    }
    
    if( "Sub.Grupo.Gasto" %in% nombres ){
      listaExclusion <- c(listaExclusion,  "Renglón" )
    }
    
    
    if( "Económico.Nivel.1" %in% nombres ){
      listaExclusion <- c(listaExclusion, c("Económico.Nivel.2", "Económico.Nivel.3","Económico.Nivel.4","Económico.Nivel.Operativo") )
      nombres <- subset( nombres, !( nombres %in% c("Económico.Nivel.2","Económico.Nivel.3","Económico.Nivel.4","Económico.Nivel.Operativo") ) )
    }
    
    if( "Económico.Nivel.2" %in% nombres ){
      listaExclusion <- c(listaExclusion, c( "Económico.Nivel.3","Económico.Nivel.4","Económico.Nivel.Operativo") )
      nombres <- subset(nombres,  !(nombres %in% c("Económico.Nivel.3","Económico.Nivel.4","Económico.Nivel.Operativo") ) ) 
    }
    
    if( "Económico.Nivel.3" %in% nombres ){
      listaExclusion <- c(listaExclusion, c("Económico.Nivel.4","Económico.Nivel.Operativo") )
      nombres <- subset(nombres,  !( nombres %in% c("Económico.Nivel.4","Económico.Nivel.Operativo") ) )
    }
    
    
    if( "Económico.Nivel.4" %in% nombres ){
      listaExclusion <- c(listaExclusion, "Económico.Nivel.Operativo" )
    }
    
    if( "Programa" %in% nombres ){
      listaExclusion <- c(listaExclusion, c("Sub.Programa", "Proyecto", "Actividad", "Obra") )
      nombres <- subset(nombres,  !( nombres %in% c("Sub.Programa","Proyecto", "Actividad", "Obra") ) )
    }
    
    if( "Sub.Programa" %in% nombres ){
      listaExclusion <- c(listaExclusion, c("Proyecto", "Actividad", "Obra") )
      nombres <- subset(nombres,  !( nombres %in% c("Proyecto", "Actividad", "Obra") ) )
    }
    
    if( "Proyecto" %in% nombres ){
      listaExclusion <- c(listaExclusion, c( "Actividad", "Obra") )
      nombres <- subset(nombres,  !( nombres %in% c("Actividad", "Obra") ) )
    }
    
    if( "Actividad" %in% nombres ){
      listaExclusion <- c(listaExclusion, "Obra" )
    }
    
    
    nombres <- names(valores)[-which( names(valores) %in% listaExclusion )]
    return(nombres)
    
  }
  
  obtenerArbol <- function(){
    arbol <- NULL
    if( length(jerarquia_dimension_regreso) == 0) {
      p= datos_tabla %>% group_by(Entidad) %>% summarize(Devengado = sum(Devengado)) %>% distinct
    }else{
      valores <- sapply(jerarquia_dimension_regreso, as.character)
      p= datos_tabla %>% select_(.dots = valores) %>% mutate(NEWCOL = NA)  %>% distinct
    }
    try( {arbol <- df2tree(struct = p,rootname = 'Gasto Total')
    output$d3 <- renderD3tree({
      d3tree(data = list(root = arbol , layout = 'collapse') , height = 500, width = 500)
    }) })
    
  }
  
  
  output$Avanzar <- renderUI({
    actionButton("avanzar_tabla", "Siguiente nivel")
  })
  
  
  
  # observeEvent(input$avanzar_tabla, {
  #   filtro <- input$opcionTabla
  #   variable <- input$tabla_rows_selected
  #   if( is.null(filtro) || is.null(variable) ){
  #     showModal(modalDialog(
  #       title = "Seleccione...",
  #       "Por favor seleccione una fila de la tabla para continuar", footer = modalButton("Continuar"), easyClose = T
  #     ))
  #   }
  #   else{
  #     tablita <- gasto_tabla(filtro, variable)
  #     output$tabla <- DT::renderDataTable({ 
  #       datos <- as.data.frame( tablita )
  #       tabla_inutil <<- DT::datatable( tabla_temporal, options = list(orderClasses = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')) ) %>%
  #         DT::formatCurrency("Devengado", currency = "Q")
  #     })
  #     actualizarFiltro(filtro) 
  #     obtenerArbol() 
  #   }
  # 
  #   
  #   })
  
  
  observeEvent(input$tabla_rows_selected,{
    if(  !is.null(input$tabla_rows_selected)  )
    {
      filaSeleccionada <<- input$tabla_rows_selected
    }
  }
  )
  
  
  
  
  
  
  numeroReactivos <- reactiveValues(x = 0, y = 0)
  
  observeEvent(input$btFiltro, {
    if(!is.na(input$btFiltro) ){
      filtro <- input$btFiltro
      variable <- filaSeleccionada
      print(input$tabla_rows_selected)
      if( is.null(filtro) || is.null(variable) ){
        showModal(modalDialog(
          title = "Seleccione...",
          "Por favor seleccione una fila de la tabla para continuar", footer = modalButton("Continuar"), easyClose = T
        ))
      }
      else{
        hacerTabla(filtro,variable)
        withProgress(message = "Preparando la información", value = 0,escribirReactivos( lon = numeroReactivos$y ))
        obtenerArbol() 
      }
    }
    
    
  })
  
  observe({
    print(numeroReactivos$x)
    print(numeroReactivos$y)
    if( numeroReactivos$x > 0){
      print("Se hacen los reactivos")
      escribirReactivos(numeroReactivos$x+1, numeroReactivos$y)
    }
  })
  
  hacerTabla <- function( filtro,variable ){
    tablita <- gasto_tabla(filtro, variable)
    val_filtros = actualizarFiltro(filtro)
    inputs <- character( nrow(tablita) )
    for( i in seq_len(nrow(tablita)) ){
      print(  paste("Haciendo el botón:", paste0("btFiltroN",i+numeroReactivos$y)) )
      inputs[i] <- as.character(dropdownButton( icon = icon("gear"), right = T, checkboxGroupButtons(
        inputId = paste0("btFiltroN",i+numeroReactivos$y), 
        choices = val_filtros, direction = "vertical") ) )  
    }
    temp <- isolate( numeroReactivos$y )
    numeroReactivos$y = temp + nrow(tablita)
    numeroReactivos$x <- temp
    datos <- as.data.frame( tablita ) %>%
      mutate( 
        Acciones =  inputs
      )
    output$tabla <- DT::renderDataTable({ 
      tabla_inutil <<- DT::datatable( datos, escape = F,
                                      options = list(orderClasses = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                     preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                                     drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')   ) ) %>%
        DT::formatCurrency("Devengado", currency = "Q")
    })
    
    
    
  }
  
  
  escribirReactivos <- function(ini = 1,lon){
    
    res <- lapply(c(ini:lon),function(x){
      print(paste("Haciendo el observer para el id", "btFiltroN",x))
      observeEvent(input[[paste0("btFiltroN",x)]], {
        print(paste("En el reactivo de", x))
        filtro <- input[[paste0("btFiltroN",x)]]
        variable <- filaSeleccionada
        hacerTabla(filtro,variable)
        obtenerArbol()
      }, once = T)
    } )
    return(res)
  }
  
  
  
  
  observeEvent(input$retroceder_tabla, {
    lista <- construirTablaDinamica()
    tablita <- lista[[1]]
    filtro <- as.character(lista[[2]])
    if(length(jerarquia_dimension_regreso) == 0){
      val_filtros = opciones_filtro_inicio  
    }else{
      val_filtros = actualizarFiltro(filtro)
    }
    
    inputs <- character( nrow(tablita) )
    for( i in seq_len(nrow(tablita)) ){
      print(  paste("Haciendo el botón:", paste0("btFiltroN",i+numeroReactivos$y)) )
      inputs[i] <- as.character(dropdownButton( icon = icon("gear"), right = T, checkboxGroupButtons(
        inputId = paste0("btFiltroN",i+numeroReactivos$y), 
        choices = val_filtros, direction = "vertical") ) )  
    }
    temp <- isolate( numeroReactivos$y )
    numeroReactivos$y = temp + nrow(tablita)
    numeroReactivos$x <- temp
    datos <- as.data.frame( tablita ) %>%
      mutate( 
        Acciones =  inputs
      )
    output$tabla <- DT::renderDataTable({
      tabla_inutil <<- DT::datatable( datos, escape = F, 
                                      list(orderClasses = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                           preDrawCallback=JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                           drawCallback=JS('function() { Shiny.bindAll(this.api().table().node()); } ')   ) )%>%
        DT::formatCurrency("Devengado", currency = "Q")
    })
    obtenerArbol()
    
    
    
    
  })
  
  
  
  observeEvent(input$yearCom,{print(input$yearCom)
    withProgress(message ='Leyendo la información', value = 0, {
      datos_tabla_con <<- read.csv(paste0('datos_tabla',input$yearCom,'.csv'), sep = ';')  
    }) 
    tabla_temp <- NULL
    if( length(jerarquia_dimension_regreso  )  == 1 ){
      tabla_temp <- datos_tabla_con %>%
        summarise( Devengado = sum(Devengado) )%>%
        mutate(Concepto = 'Gasto Total') 
    }else{
      contador <- 2:length(jerarquia_dimension_regreso)
      for(z in  contador ){
        a <- jerarquia_dimension_regreso[[z]]
        b <- jerarquia_valor_dimension_regreso[[z]]
        .dots <- list(interp( ~y==x, .values = list( y = as.name(jerarquia_dimension_regreso[[z]] ), x = jerarquia_valor_dimension_regreso[[z]] ) ) )                      
        if( z == length(jerarquia_dimension_regreso) ) {
          if( z == 2 ){
            tabla_dinamica <<- datos_tabla_con 
            tabla_temp <- datos_tabla_con %>%
              group_by_(jerarquia_dimension_regreso[[z]]) %>%
              summarise( Devengado = sum(Devengado) ) %>%
              rename_(Concepto = jerarquia_dimension_regreso[[z]])
            dimension_ida <<- jerarquia_dimension_regreso[[z]]
          }else{
            tabla_dinamica <<- tabla_temp
            tabla_temp <- tabla_temp %>%
              group_by_(jerarquia_dimension_regreso[[z]]) %>%
              summarise( Devengado = sum(Devengado) ) %>%
              rename_(Concepto = jerarquia_dimension_regreso[[z]])
            dimension_ida <<- jerarquia_dimension_regreso[[z]]
          }
        }else if( z == 2 ){
          tabla_temp <- datos_tabla_con %>%
            filter_(.dots = .dots)
        }else{
          tabla_temp <- tabla_temp %>%
            filter_(.dots = .dots)
        }
      }
    }
    print(tabla_temp)
    
    
    })
  
  
  
  
  
  
  # Data Tree implementation ------------------------------------------------
  d3treeOutput("d3")
  network <- reactiveValues()
  
  observeEvent(input$d3_update,{
    network$nodes <- unlist(input$d3_update$.nodesData)
    activeNode<-input$d3_update$.activeNode
    if(!is.null(activeNode)) network$click <- jsonlite::fromJSON(activeNode)
  })
  
  TreeStruct=eventReactive(network$nodes,{
    df=datos_tabla
    if(is.null(network$nodes)){
      df=datos_tabla
    }else{
      
      x.filter=tree.filter(network$nodes,m)
      df=ddply(x.filter,.(ID),function(a.x){m%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  
  observeEvent(network$nodes,{
    output$results <- renderPrint({
      str.out=''
      if(!is.null(network$nodes)) str.out=tree.filter(network$nodes,m)
      return(str.out)
    })    
  })
  
  output$d3 <- renderD3tree({
    if( length(jerarquia_dimension_regreso) == 0) {
      p= datos_tabla %>% group_by(Entidad) %>% summarize(Devengado = sum(Devengado))
    }else{
      p=tabla_dinamica
    }
    d3tree(data = list(root = df2tree(struct = p,rootname = 'Gasto Total'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 58)
  })
  
  
  
  
  # Gráfica  ----------------------------------------------------------------
  
  # highlight selected rows in the scatterplot
  output$grafica <- renderPlotly({
    numeroReactivos$y
    s <- input$tabla_rows_selected
    tipo = ""
    if(nrow(tabla_temporal) < 10 ){
      tipo = 'bar'
    }else{
      tipo = 'scatter'
    }
    if (is.null(s)) {
      
      p <- tabla_temporal %>%
        plot_ly(x = ~Concepto, y = ~Devengado, mode = "markers", color = I('black'), name = 'Concepto', type = tipo, width = 0.03) %>%
        layout(xaxis = list(showticklabels = FALSE) ,showlegend = T) %>% 
        highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
    } else{
      pp <- tabla_temporal %>%
        plot_ly() %>% 
        add_trace(x = ~Concepto, y = ~Devengado, mode = "markers", color = I('black'), name = 'Concepto', type = tipo, width = 0.3) %>%
        layout( showlegend = T)
      
      # selected data
      pp <- add_trace(pp, data = tabla_temporal[s, , drop = F], x = ~Concepto, y = ~Devengado, mode = "markers",
                      color = I('red'), name = 'Selección',type = 'scatter')
    }
    
  })
  
})
