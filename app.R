# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(DT)
library(plotly)

# Verificar e instalar paquetes faltantes
if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("DT")) install.packages("DT")
if (!require("plotly")) install.packages("plotly")

# Paleta de colores profesional
COLORES <- list(
  primario = "#1A3A5F",
  secundario = "#2C5F8C", 
  acento = "#FF6B35",
  exito = "#38A169",
  advertencia = "#D69E2E",
  peligro = "#E53E3E",
  malware = c(
    Ransomware = "#E53E3E",
    Spyware = "#3182CE", 
    Trojan = "#D69E2E",
    Virus = "#38A169",
    Worm = "#805AD5"
  )
)

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "SecurityBench Pro",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Análisis", tabName = "analisis", icon = icon("chart-line")),
      menuItem("Datos", tabName = "datos", icon = icon("database"))
    ),
    
    # Panel de control
    box(
      width = 12,
      solidHeader = TRUE,
      background = "black",
      
      numericInput("n_simulaciones", "Nº Simulaciones:", value = 200, min = 50, max = 1000),
      
      sliderInput("tiempo_max", "Horas análisis:", min = 24, max = 168, value = 72),
      
      selectInput("tipo_malware", "Tipo malware:",
                  choices = c("Mixto", "Ransomware", "Spyware", "Troyano"),
                  selected = "Mixto"),
      
      sliderInput("prob_deteccion", "Detección:", min = 0, max = 1, value = 0.4),
      
      actionButton("simular", "Ejecutar", class = "btn-success", icon = icon("play"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo { background-color: #1A3A5F !important; }
        .content-wrapper { background-color: #f8f9fa; }
        .box { border-radius: 8px; margin-bottom: 15px; }
        .small-box { border-radius: 8px; }
      "))
    ),
    
    tabItems(
      # Dashboard
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("vb_incubacion", width = 3),
          valueBoxOutput("vb_recuperacion", width = 3),
          valueBoxOutput("vb_eficiencia", width = 3),
          valueBoxOutput("vb_deteccion", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Distribución Tiempos Incubación",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_incubacion")
          ),
          
          box(
            title = "Incubación vs Recuperación",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_correlacion")
          )
        ),
        
        fluidRow(
          box(
            title = "Evolución Temporal",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_evolucion")
          )
        )
      ),
      
      # Análisis
      tabItem(
        tabName = "analisis",
        fluidRow(
          box(
            title = "Curva de Supervivencia",
            status = "danger",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("plot_supervivencia")
          ),
          
          box(
            title = "Métricas por Malware",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            tableOutput("tabla_metricas")
          )
        )
      ),
      
      # Datos
      tabItem(
        tabName = "datos",
        fluidRow(
          box(
            title = "Datos de Simulación",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("tabla_datos"),
            downloadButton("descargar", "Exportar CSV")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Función de simulación
  simular_datos <- reactive({
    req(input$simular)
    
    set.seed(123)
    n <- input$n_simulaciones
    
    # Tipos de malware
    if (input$tipo_malware == "Mixto") {
      tipos <- c("Ransomware", "Spyware", "Trojan", "Virus", "Worm")
    } else {
      tipos <- rep(input$tipo_malware, n)
    }
    
    datos <- data.frame(
      id = 1:n,
      tipo_malware = sample(tipos, n, replace = TRUE),
      tiempo_incubacion = pmax(1, rnorm(n, 48, 12)),
      tiempo_recuperacion = pmax(1, rnorm(n, 24, 8)),
      vulnerado = TRUE,
      stringsAsFactors = FALSE
    )
    
    # Ajustar por tipo
    ajustes <- list(
      Ransomware = c(incubacion = 1.5, recuperacion = 2.0),
      Spyware = c(incubacion = 2.0, recuperacion = 1.5),
      Trojan = c(incubacion = 1.2, recuperacion = 1.3),
      Virus = c(incubacion = 1.0, recuperacion = 1.0),
      Worm = c(incubacion = 1.3, recuperacion = 1.8)
    )
    
    for (malware in names(ajustes)) {
      idx <- datos$tipo_malware == malware
      datos$tiempo_incubacion[idx] <- datos$tiempo_incubacion[idx] * ajustes[[malware]]["incubacion"]
      datos$tiempo_recuperacion[idx] <- datos$tiempo_recuperacion[idx] * ajustes[[malware]]["recuperacion"]
    }
    
    # Simular detección
    deteccion_temprana <- runif(n) < input$prob_deteccion
    datos$tiempo_deteccion <- ifelse(
      deteccion_temprana,
      datos$tiempo_incubacion * runif(n, 0.1, 0.7),
      datos$tiempo_incubacion * runif(n, 1.1, 2.0)
    )
    
    datos$eficiencia_respuesta <- datos$tiempo_recuperacion / datos$tiempo_incubacion
    datos$deteccion_temprana <- deteccion_temprana
    
    return(datos)
  })
  
  # ValueBoxes
  output$vb_incubacion <- renderValueBox({
    datos <- simular_datos()
    valor <- round(mean(datos$tiempo_incubacion), 1)
    color <- ifelse(valor > 72, "red", ifelse(valor > 48, "yellow", "green"))
    
    valueBox(
      paste(valor, "h"), "Incubación Media", 
      icon = icon("clock"), color = color
    )
  })
  
  output$vb_recuperacion <- renderValueBox({
    datos <- simular_datos()
    valor <- round(mean(datos$tiempo_recuperacion), 1)
    color <- ifelse(valor > 48, "red", ifelse(valor > 24, "yellow", "green"))
    
    valueBox(
      paste(valor, "h"), "Recuperación Media", 
      icon = icon("medkit"), color = color
    )
  })
  
  output$vb_eficiencia <- renderValueBox({
    datos <- simular_datos()
    valor <- round(mean(datos$eficiencia_respuesta), 2)
    color <- ifelse(valor > 1, "red", ifelse(valor > 0.7, "yellow", "green"))
    
    valueBox(
      valor, "Eficiencia", 
      icon = icon("tachometer-alt"), color = color
    )
  })
  
  output$vb_deteccion <- renderValueBox({
    datos <- simular_datos()
    valor <- round(mean(datos$deteccion_temprana) * 100, 1)
    color <- ifelse(valor > 60, "green", ifelse(valor > 40, "yellow", "red"))
    
    valueBox(
      paste(valor, "%"), "Detección Temprana", 
      icon = icon("binoculars"), color = color
    )
  })
  
  # Gráficos
  output$plot_incubacion <- renderPlotly({
    datos <- simular_datos()
    
    p <- ggplot(datos, aes(x = tiempo_incubacion, fill = tipo_malware)) +
      geom_histogram(bins = 20, alpha = 0.7) +
      labs(title = "Distribución de Tiempos de Incubación",
           x = "Horas", y = "Frecuencia") +
      theme_minimal() +
      scale_fill_manual(values = COLORES$malware)
    
    ggplotly(p)
  })
  
  output$plot_correlacion <- renderPlotly({
    datos <- simular_datos()
    
    p <- ggplot(datos, aes(x = tiempo_incubacion, y = tiempo_recuperacion, color = tipo_malware)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Relación Incubación vs Recuperación",
           x = "Incubación (h)", y = "Recuperación (h)") +
      theme_minimal() +
      scale_color_manual(values = COLORES$malware)
    
    ggplotly(p)
  })
  
  output$plot_evolucion <- renderPlotly({
    datos <- simular_datos()
    
    evol_data <- datos %>%
      arrange(tiempo_incubacion) %>%
      mutate(
        acumulado = seq_len(n()),
        porcentaje = acumulado / n() * 100
      )
    
    p <- ggplot(evol_data, aes(x = tiempo_incubacion, y = porcentaje)) +
      geom_line(color = COLORES$peligro, size = 1.2) +
      geom_area(fill = COLORES$peligro, alpha = 0.2) +
      labs(title = "Evolución de Incidentes",
           x = "Horas", y = "% Sistemas Comprometidos") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$plot_supervivencia <- renderPlotly({
    datos <- simular_datos()
    
    survival_data <- datos %>%
      arrange(tiempo_incubacion) %>%
      mutate(
        tiempo = tiempo_incubacion,
        supervivencia = 1 - (seq_len(n()) / n())
      )
    
    p <- ggplot(survival_data, aes(x = tiempo, y = supervivencia)) +
      geom_step(color = COLORES$peligro, size = 1.2) +
      geom_ribbon(aes(ymin = 0, ymax = supervivencia), fill = COLORES$peligro, alpha = 0.1) +
      labs(title = "Curva de Supervivencia",
           x = "Horas", y = "Probabilidad Supervivencia") +
      theme_minimal() +
      ylim(0, 1)
    
    ggplotly(p)
  })
  
  # Tabla de métricas
  output$tabla_metricas <- renderTable({
    datos <- simular_datos()
    
    metricas <- datos %>%
      group_by(tipo_malware) %>%
      summarise(
        Incubación = round(mean(tiempo_incubacion), 1),
        Recuperación = round(mean(tiempo_recuperacion), 1),
        Eficiencia = round(mean(eficiencia_respuesta), 2),
        .groups = 'drop'
      )
    
    metricas
  }, bordered = TRUE, striped = TRUE)
  
  # Tabla de datos
  output$tabla_datos <- renderDT({
    datos <- simular_datos()
    
    datatable(
      datos,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Descarga
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("datos_seguridad_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(simular_datos(), file, row.names = FALSE)
    }
  )
}

# Ejecutar aplicación
shinyApp(ui, server)
