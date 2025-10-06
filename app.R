library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
            .nav-tabs {
                background-color: #f8f9fa;
                border-bottom: 2px solid #dee2e6;
            }
            .nav-tabs > li > a {
                color: #495057;
                font-weight: 500;
            }
            .nav-tabs > li.active > a {
                background-color: #2c3e50;
                color: white;
                border: 1px solid #2c3e50;
            }
            .well {
                background-color: #f8f9fa;
                border: 1px solid #dee2e6;
                border-radius: 5px;
            }
            .btn-primary {
                background-color: #2c3e50;
                border-color: #2c3e50;
                font-weight: 500;
            }
            .btn-primary:hover {
                background-color: #1a252f;
                border-color: #1a252f;
            }
            h1 {
                color: #2c3e50;
                font-weight: 600;
            }
            h2 {
                color: #2c3e50;
                border-bottom: 2px solid #3498db;
                padding-bottom: 10px;
                margin-top: 20px;
            }
            h3 {
                color: #34495e;
                margin-top: 15px;
            }
            h4 {
                color: #7f8c8d;
                margin-top: 12px;
            }
            .shiny-text-output {
                background-color: #ecf0f1;
                padding: 15px;
                border-radius: 5px;
                border-left: 4px solid #3498db;
                font-family: 'Courier New', monospace;
                font-size: 0.9em;
            }
            .footer {
                background-color: #ecf0f1;
                color: #7f8c8d;
                padding: 15px;
                text-align: center;
                border-radius: 5px;
                margin-top: 20px;
                font-size: 0.9em;
            }
            .highlight-box {
                background-color: #d6eaf8;
                padding: 20px;
                border-radius: 5px;
                margin: 10px 0;
                border-left: 4px solid #3498db;
            }
            .stat-box {
                background-color: #f8f9fa;
                padding: 15px;
                border-radius: 5px;
                border: 1px solid #dee2e6;
                height: 100%;
            }
            .content-box {
                background-color: #ffffff;
                padding: 20px;
                border-radius: 5px;
                border: 1px solid #dee2e6;
                margin-bottom: 15px;
            }
        "))
  ),
  
  # Application title
  titlePanel(
    div(
      style = "background-color: #2c3e50; color: white; padding: 25px; border-radius: 5px;",
      h1("Prueba de Normalidad Shapiro-Wilk", style = "margin: 0; font-weight: 600;"),
      h4("Herramienta interactiva para evaluación de normalidad estadística", 
         style = "margin: 5px 0 0 0; font-weight: 300; color: #bdc3c7;")
    )
  ),
  
  br(),
  
  # Navigation tabs
  tabsetPanel(
    type = "tabs",
    id = "mainTabs",
    
    # Introducción tab
    tabPanel(
      "Introducción",
      fluidRow(
        column(12,
               div(class = "highlight-box",
                   h2("Prueba de Normalidad Shapiro-Wilk"),
                   p("Herramienta interactiva para evaluación de normalidad", 
                     style = "font-size: 1.1em; color: #2c3e50;")
               ),
               
               div(class = "content-box",
                   h3("¿Qué es la Prueba de Normalidad Shapiro-Wilk?"),
                   p("Una prueba estadística poderosa para evaluar si los datos siguen una distribución normal."),
                   
                   h4("Definición"),
                   p("La prueba de Shapiro-Wilk es un test estadístico utilizado para evaluar si un conjunto de datos sigue una distribución normal. Es considerada una de las pruebas más potentes para detectar desviaciones de la normalidad, especialmente con tamaños de muestra pequeños."),
                   p("Desarrollada por Samuel Shapiro y Martin Wilk en 1965, esta prueba se ha convertido en el estándar para la evaluación de normalidad en investigación estadística."),
                   
                   h4("Características Principales"),
                   fluidRow(
                     column(6,
                            tags$ul(
                              tags$li("Alta potencia para muestras pequeñas (n < 50)"),
                              tags$li("Basada en correlación entre datos y puntuaciones normales"),
                              tags$li("Evalúa distribuciones unimodales"),
                              tags$li("Ampliamente aceptada en literatura científica")
                            )
                     ),
                     column(6,
                            tags$ul(
                              tags$li("Ideal para verificación de supuestos paramétricos"),
                              tags$li("Implementada en todos los software estadísticos"),
                              tags$li("Rápida ejecución computacional"),
                              tags$li("Sensible a desviaciones de normalidad")
                            )
                     )
                   )
               ),
               
               div(class = "content-box",
                   h3("Fundamento Matemático"),
                   withMathJax("$$W = \\frac{(\\sum_{i=1}^n a_i x_{(i)})^2}{\\sum_{i=1}^n (x_i - \\bar{x})^2}$$"),
                   p("Donde:", style = "font-weight: bold;"),
                   tags$ul(
                     tags$li("x(i) son los valores ordenados de la muestra"),
                     tags$li("ai son constantes derivadas de las medias de estadísticos de orden"),
                     tags$li("W se aproxima a 1 cuando los datos son normales")
                   ),
                   p("Esta fórmula compara la varianza observada con la varianza esperada bajo el supuesto de normalidad, proporcionando una medida robusta de desviación.")
               ),
               
               div(class = "footer",
                   "Desarrolado por Lenin Alfonso Suarez Ccama - 2025"
               )
        )
      )
    ),
    
    # Simulación tab
    tabPanel(
      "Simulación Interactiva",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "background-color: #f8f9fa; border-radius: 5px;",
          h3("Configuración de la Simulación"),
          
          selectInput("dist_type",
                      "Tipo de distribución:",
                      choices = c("Normal" = "norm",
                                  "Uniforme" = "unif",
                                  "Exponencial" = "exp",
                                  "Binomial" = "binom",
                                  "Sesgada a la derecha" = "right_skew",
                                  "Sesgada a la izquierda" = "left_skew")),
          
          sliderInput("sample_size",
                      "Tamaño de muestra:",
                      min = 3,
                      max = 1000,
                      value = 50,
                      step = 1),
          
          # Conditional panels for different distributions
          conditionalPanel(
            condition = "input.dist_type == 'norm'",
            sliderInput("mean_val", "Media:", -5, 5, 0, 0.1),
            sliderInput("sd_val", "Desviación estándar:", 0.1, 5, 1, 0.1)
          ),
          
          conditionalPanel(
            condition = "input.dist_type == 'unif'",
            sliderInput("unif_range", "Rango:", -10, 10, c(0, 5))
          ),
          
          conditionalPanel(
            condition = "input.dist_type == 'exp'",
            sliderInput("exp_rate", "Tasa (λ):", 0.1, 5, 1, 0.1)
          ),
          
          conditionalPanel(
            condition = "input.dist_type == 'binom'",
            sliderInput("binom_size", "Número de ensayos:", 1, 50, 10),
            sliderInput("binom_prob", "Probabilidad de éxito:", 0, 1, 0.5, 0.1)
          ),
          
          conditionalPanel(
            condition = "input.dist_type == 'right_skew'",
            sliderInput("right_skew_param", "Grado de sesgo:", 1, 10, 3, 1)
          ),
          
          conditionalPanel(
            condition = "input.dist_type == 'left_skew'",
            sliderInput("left_skew_param", "Grado de sesgo:", 1, 10, 3, 1)
          ),
          
          actionButton("simulate", "Generar Datos y Ejecutar Prueba", 
                       style = "background-color: #2c3e50; color: white; font-weight: 500; width: 100%;")
        ),
        
        mainPanel(
          width = 9,
          fluidRow(
            column(6,
                   div(class = "content-box",
                       h4("Histograma y Densidad"),
                       plotOutput("distPlot", height = "300px")
                   )
            ),
            column(6,
                   div(class = "content-box",
                       h4("Gráfico Q-Q"),
                       plotOutput("qqPlot", height = "300px")
                   )
            )
          ),
          
          br(),
          
          fluidRow(
            column(12,
                   div(class = "content-box",
                       h3("Resultado de la Prueba Shapiro-Wilk"),
                       verbatimTextOutput("testResult"),
                       
                       div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px;",
                           h4("Interpretación:"),
                           uiOutput("interpretation")
                       )
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   div(class = "content-box",
                       h4("Estadísticas Descriptivas"),
                       tableOutput("descriptiveStats")
                   )
            ),
            column(6,
                   div(class = "content-box",
                       h4("Información de la Muestra"),
                       uiOutput("sampleInfo")
                   )
            )
          )
        )
      )
    ),
    
    # Aplicaciones tab
    tabPanel(
      "Aplicaciones",
      fluidRow(
        column(12,
               div(class = "highlight-box",
                   h2("Aplicaciones en Investigación")
               ),
               
               fluidRow(
                 column(4,
                        div(class = "content-box",
                            h4("Verificación de Supuestos Paramétricos"),
                            tags$ul(
                              tags$li("Pruebas t de Student"),
                              tags$li("Análisis de varianza (ANOVA)"),
                              tags$li("Modelos de regresión lineal"),
                              tags$li("Análisis de correlación paramétrica"),
                              tags$li("Gráficos de control estadísticos")
                            )
                        )
                 ),
                 column(4,
                        div(class = "content-box",
                            h4("Control de Calidad"),
                            tags$ul(
                              tags$li("Validación de procesos industriales"),
                              tags$li("Análisis de capacidad de procesos"),
                              tags$li("Estudios de repetibilidad"),
                              tags$li("Control de instrumentación y medición")
                            )
                        )
                 ),
                 column(4,
                        div(class = "content-box",
                            h4("Investigación Científica"),
                            tags$ul(
                              tags$li("Ciencias de la salud y medicina"),
                              tags$li("Psicología y ciencias educativas"),
                              tags$li("Estudios ambientales y ecológicos"),
                              tags$li("Investigación social y de mercados")
                            )
                        )
                 )
               ),
               
               br(),
               
               fluidRow(
                 column(6,
                        div(class = "content-box",
                            h3("Ventajas"),
                            tags$ul(
                              tags$li("Alta potencia estadística en muestras pequeñas"),
                              tags$li("Basada en propiedades estadísticas robustas"),
                              tags$li("Ampliamente aceptada en la literatura científica"),
                              tags$li("Implementada en la mayoría de software estadístico"),
                              tags$li("Proporciona resultados confiables y reproducibles")
                            )
                        )
                 ),
                 column(6,
                        div(class = "content-box",
                            h3("Limitaciones"),
                            tags$ul(
                              tags$li("Sensible a la presencia de valores atípicos"),
                              tags$li("Puede ser demasiado estricta con muestras grandes"),
                              tags$li("No aplicable a distribuciones multimodales"),
                              tags$li("Requiere datos continuos para su aplicación"),
                              tags$li("No proporciona información sobre el tipo de no-normalidad")
                            )
                        )
                 )
               )
        )
      )
    ),
    
    # Recursos tab
    tabPanel(
      "Recursos",
      fluidRow(
        column(12,
               div(class = "highlight-box",
                   h2("Recursos Adicionales")
               ),
               
               fluidRow(
                 column(6,
                        div(class = "content-box",
                            h3("Referencias Bibliográficas"),
                            tags$ul(
                              tags$li("Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality. Biometrika, 52(3-4), 591-611."),
                              tags$li("Razali, N. M., & Wah, Y. B. (2011). Power comparisons of Shapiro-Wilk, Kolmogorov-Smirnov, Lilliefors and Anderson-Darling tests. Journal of Statistical Modeling and Analytics, 2(1), 21-33."),
                              tags$li("Thode, H. C. (2002). Testing for normality. CRC Press."),
                              tags$li("Ghasemi, A., & Zahediasl, S. (2012). Normality tests for statistical analysis: A guide for non-statisticians. International Journal of Endocrinology and Metabolism, 10(2), 486-489.")
                            )
                        )
                 ),
                 column(6,
                        div(class = "content-box",
                            h3("Implementaciones en Software"),
                            tags$ul(
                              tags$li("R: función shapiro.test() en el paquete stats"),
                              tags$li("Python: scipy.stats.shapiro()"),
                              tags$li("SPSS: Analyze > Descriptive Statistics > Explore"),
                              tags$li("SAS: PROC UNIVARIATE con opción NORMAL"),
                              tags$li("Excel: Complementos de análisis estadístico")
                            )
                        )
                 )
               ),
               
               br(),
               
               div(class = "content-box",
                   h3("Código de Ejemplo en R"),
                   pre(
                     style = "background-color: #2c3e50; color: #ecf0f1; padding: 15px; border-radius: 5px; font-size: 0.85em;",
                     "# Generar datos normalmente distribuidos
set.seed(123)
datos_normales <- rnorm(30, mean = 0, sd = 1)

# Ejecutar prueba Shapiro-Wilk
resultado <- shapiro.test(datos_normales)
print(resultado)

# Generar gráficos de diagnóstico
par(mfrow = c(1, 2))
hist(datos_normales, main = 'Histograma', xlab = 'Valores', col = 'lightblue')
qqnorm(datos_normales, main = 'Gráfico Q-Q')
qqline(datos_normales)

# Interpretación del resultado
nivel_significancia <- 0.05
if(resultado$p.value > nivel_significancia) {
    cat('Conclusión: No se rechaza H0 - Los datos siguen distribución normal')
} else {
    cat('Conclusión: Se rechaza H0 - Los datos no siguen distribución normal')
}"
                   )
               ),
               
               div(class = "content-box",
                   h3("Consideraciones Prácticas"),
                   tags$ul(
                     tags$li("Tamaño de muestra mínimo recomendado: 3 observaciones"),
                     tags$li("Tamaño de muestra máximo: 5000 observaciones"),
                     tags$li("Nivel de significancia típico: α = 0.05"),
                     tags$li("La prueba es más poderosa para n < 50"),
                     tags$li("Siempre complementar con métodos gráficos (Q-Q plot, histograma)")
                   )
               )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data generation
  sample_data <- eventReactive(input$simulate, {
    req(input$sample_size)
    
    set.seed(as.numeric(Sys.time()))
    
    n <- input$sample_size
    
    if(input$dist_type == "norm") {
      data <- rnorm(n, input$mean_val, input$sd_val)
    } else if(input$dist_type == "unif") {
      data <- runif(n, input$unif_range[1], input$unif_range[2])
    } else if(input$dist_type == "exp") {
      data <- rexp(n, rate = input$exp_rate)
    } else if(input$dist_type == "binom") {
      data <- rbinom(n, size = input$binom_size, prob = input$binom_prob)
    } else if(input$dist_type == "right_skew") {
      data <- rgamma(n, shape = input$right_skew_param, scale = 1)
    } else if(input$dist_type == "left_skew") {
      data <- rbeta(n, shape1 = input$left_skew_param, shape2 = 1)
    }
    
    return(data)
  })
  
  # Histogram plot
  output$distPlot <- renderPlot({
    data <- sample_data()
    
    if(length(data) > 0) {
      ggplot(data.frame(x = data), aes(x = x)) +
        geom_histogram(aes(y = ..density..), 
                       bins = 30, 
                       fill = "#3498db", 
                       color = "#2c3e50",
                       alpha = 0.7) +
        geom_density(color = "#e74c3c", size = 1, alpha = 0.6) +
        stat_function(fun = dnorm, 
                      args = list(mean = mean(data), sd = sd(data)),
                      color = "#2c3e50", 
                      size = 0.8, linetype = "dashed") +
        labs(title = paste("Distribución de", length(data), "observaciones"),
             x = "Valores",
             y = "Densidad") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
          axis.title = element_text(color = "#34495e"),
          panel.grid.major = element_line(color = "#ecf0f1"),
          panel.grid.minor = element_blank()
        )
    }
  })
  
  # Q-Q plot
  output$qqPlot <- renderPlot({
    data <- sample_data()
    
    if(length(data) > 0) {
      ggplot(data.frame(x = data), aes(sample = x)) +
        stat_qq(color = "#3498db", size = 1.5, alpha = 0.7) +
        stat_qq_line(color = "#e74c3c", size = 1) +
        labs(title = "Gráfico de Cuantiles-Cuantiles (Q-Q)",
             x = "Cuantiles Teóricos Normales",
             y = "Cuantiles Muestrales") +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
          axis.title = element_text(color = "#34495e"),
          panel.grid.major = element_line(color = "#ecf0f1"),
          panel.grid.minor = element_blank()
        )
    }
  })
  
  # Shapiro-Wilk test results
  output$testResult <- renderPrint({
    data <- sample_data()
    
    if(length(data) >= 3 && length(data) <= 5000) {
      test_result <- shapiro.test(data)
      
      cat("RESULTADO DE LA PRUEBA SHAPIRO-WILK\n")
      cat("=====================================\n\n")
      cat("Estadístico W =", round(test_result$statistic, 4), "\n")
      cat("Valor p =", format.pval(test_result$p.value, digits = 4), "\n")
      cat("Tamaño de muestra: n =", length(data), "\n\n")
      
      cat("Hipótesis Estadísticas:\n")
      cat("  H₀: Los datos siguen una distribución normal\n")
      cat("  H₁: Los datos NO siguen una distribución normal\n\n")
      
      cat("Nivel de significancia: α = 0.05\n")
    } else {
      cat("La prueba requiere entre 3 y 5000 observaciones\n")
      cat("Muestra actual:", length(data), "observaciones\n")
    }
  })
  
  # Interpretation
  output$interpretation <- renderUI({
    data <- sample_data()
    
    if(length(data) >= 3 && length(data) <= 5000) {
      test_result <- shapiro.test(data)
      
      if(test_result$p.value > 0.05) {
        div(style = "color: #27ae60; font-weight: bold; padding: 10px; background-color: #d5f5e3; border-radius: 5px;",
            "No se rechaza la hipótesis nula. Los datos parecen seguir una distribución normal.",
            br(),
            tags$small(paste("(p =", round(test_result$p.value, 4), "> 0.05)"))
        )
      } else {
        div(style = "color: #c0392b; font-weight: bold; padding: 10px; background-color: #fadbd8; border-radius: 5px;",
            "Se rechaza la hipótesis nula. Los datos NO siguen una distribución normal.",
            br(),
            tags$small(paste("(p =", round(test_result$p.value, 4), "≤ 0.05)"))
        )
      }
    } else {
      div(style = "color: #f39c12; font-weight: bold; padding: 10px; background-color: #fef9e7; border-radius: 5px;",
          "Ajuste el tamaño de la muestra (se requieren entre 3 y 5000 observaciones)"
      )
    }
  })
  
  # Descriptive statistics table
  output$descriptiveStats <- renderTable({
    data <- sample_data()
    
    if(length(data) > 0) {
      stats <- data.frame(
        Estadístico = c("Media", "Mediana", "Desviación Estándar", 
                        "Mínimo", "Máximo", "Asimetría"),
        Valor = c(
          round(mean(data), 3),
          round(median(data), 3),
          round(sd(data), 3),
          round(min(data), 3),
          round(max(data), 3),
          round(mean((data - mean(data))^3) / (sd(data)^3), 3)
        )
      )
      stats
    }
  }, bordered = TRUE, striped = TRUE, align = 'c', width = '100%')
  
  # Sample information
  output$sampleInfo <- renderUI({
    data <- sample_data()
    
    if(length(data) > 0) {
      tags$div(
        p(strong("Distribución:"), toupper(input$dist_type)),
        p(strong("Tamaño muestra:"), length(data)),
        p(strong("Rango:"), paste(round(min(data), 2), "-", round(max(data), 2))),
        p(strong("Rango intercuartílico:"), paste(round(quantile(data, 0.25), 2), "-", round(quantile(data, 0.75), 2))),
        p(strong("Recomendación:"), 
          ifelse(length(data) < 20, "Muestra pequeña - Shapiro-Wilk óptimo", 
                 ifelse(length(data) < 100, "Muestra mediana - Buena potencia",
                        "Muestra grande - Alta potencia")))
      )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
