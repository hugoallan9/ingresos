#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(leaflet)
#require(DT)

dashboardPage(
  dashboardHeader(title = "Gastos"),
  dashboardSidebar(
    selectInput("year",label = HTML('<p style="color:black">Ejercicio Fiscal <p>') ,choices = c(1998:format(Sys.Date(), "%Y")), selected = format(Sys.Date(), "%Y")  ),
    uiOutput("opcionesFiltro"),
    uiOutput("tipoVisualizacion")
  ),
  dashboardBody(
    conditionalPanel(
      condition = "output.condition == 0",
      fluidRow(
        infoBoxOutput("progressBox"),
        infoBoxOutput("devengadoBox"),
        uiOutput("detalle")
      )
    ),
    
    conditionalPanel(
      condition = "output.condition == 1",
      useShinyjs(),
      fluidRow(
        br(),
        br(),
        tags$h1("Detalle del Gasto ",style="text-align:center;color:blue;font-size:200%"),
        tags$p("Click On Any Region To Get The Treemap Of That Region",style="text-align:center;color:purple"),
        actionButton("atrasTM", "Subir Nivel" , icon = icon("glyphicon glyphicon-arrow-up", lib= "glyphicon") ),
        plotOutput("treemap1",height="600px", click = "click_treemap")
      )
    ),
    # ,
    # 
    conditionalPanel(
      condition = "output.condition == 2",
      fluidRow(
        br(),
        br(),
        tags$h1("Mapa de Guatemala"),
        leafletOutput("mapa", height = "600")
      )
    ),
    conditionalPanel(
      condition = "output.condition == 3",
      fluidPage(
        tabBox(      side = "right", width = 12,
                     selected = "Tab1",
                     tabPanel("Comparador de ejercicios", 
                              fluidRow(
                                column(4,uiOutput("comparadorAño"))
                              ),
                              fluidRow(column(8,plotlyOutput("grafCon")))
                     
        ),
        tabPanel("Tab1", 

                 
                 fluidRow(
                   column(12, align= "center",offset = 0,
                          tags$h1("Ejecución presupuestaria")
                   )
                 ),
                 
                 fluidRow(
                   column(12, align= "center",offset = 0,
                          tags$h3("Administración Central")
                   )
                 ),
                 

                 
                 fluidRow(
                   column(12, align = "center", 
                          plotlyOutput("grafica")
                   )
                 ), 
                 
                  fluidRow(
                    column(12, align = "right",
                    uiOutput("Atras"))
                  ), 
                  
                  fluidRow(
                   column(12, align = "left", 
                          DT::dataTableOutput("tabla")
                   ) )
              
                 
                 
                 
                 )
        
        
        
          
          #d3treeOutput(outputId="d3",width = '1200px',height = '800px')
        )
      )
    ), 
    
    conditionalPanel(
      condition =  "output.condition == 100",
      dropdownButton(label = "Hola", icon = icon("gear"))
    ),
    
    tags$head(tags$style(HTML('
        /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #605ca8;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #c6c3ff;

                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: 	#605ca8;
                              }        
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: 	#FFFFFF;
                              color = #000000;
                                  border-bottom-width: 0.01px;
    border-right-color: #D3D3D3;
    border-right-style: solid;

                              }


                              
                              .content-wrapper,
                              .right-side {
                              color: #000000;
                              background-color: #ffffff;
                                                            border-color: #605ca8;

                              }

                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #ff0000;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #605ca8;
                              color: #605ca8;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              }
                              /* toggle button when hovered  */                    
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #c6c3ff;
                              }

                              label{color:black}
                              p.dotted {border-style: dotted;}
                              hr {border-top: 1px solid #000000;}"


                              
                              ' ) ) )
     
    
    
    
  )
  
  
)