library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Análisis ML - Congestión Santiago"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📊 Comparación Modelos", tabName = "comparacion", icon = icon("chart-bar")),
      menuItem("⏱️ Tiempos Entrenamiento", tabName = "tiempos", icon = icon("clock")),
      menuItem("📈 Regresión Lineal", tabName = "regresion", icon = icon("line-chart")),
      menuItem("🌳 Árbol de Decisión", tabName = "arbol", icon = icon("tree")),
      menuItem("🧠 Red Neuronal", tabName = "red", icon = icon("brain")),
      menuItem("📉 Residuales & Gráficos", tabName = "graficos", icon = icon("chart-area")),
      menuItem("📋 Tabla Validación Test", tabName = "validacion", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f4f4; }
        .box { border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .main-header .logo { font-weight: bold; font-size: 18px; }
      "))
    ),
    
    tabItems(
      # TAB 1: Comparación de Modelos (Slide 10)
      tabItem(
        tabName = "comparacion",
        fluidRow(
          box(
            title = "🏆 Comparación de Modelos - Validación Cruzada",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_comparacion", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "📊 Métricas Detalladas con Hiperparámetros Óptimos",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("tabla_metricas")
          )
        )
      ),
      
      # TAB 2: Tiempos de Entrenamiento (Slide 6)
      tabItem(
        tabName = "tiempos",
        fluidRow(
          valueBoxOutput("tiempo_total", width = 4),
          valueBoxOutput("modelo_rapido", width = 4),
          valueBoxOutput("modelo_lento", width = 4)
        ),
        fluidRow(
          box(
            title = "⏱️ Tiempos de Entrenamiento por Algoritmo",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("plot_tiempos", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "📋 Tabla de Tiempos de Entrenamiento",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("tabla_tiempos"),
            hr(),
            p(strong("Especificaciones del Sistema:")),
            verbatimTextOutput("system_info")
          )
        )
      ),
      
      # TAB 3: Regresión Lineal (Slide 7)
      tabItem(
        tabName = "regresion",
        fluidRow(
          box(
            title = "📊 Tabla de Coeficientes - Regresión Lineal",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("tabla_coeficientes"),
            hr(),
            p(strong("Interpretación:"), "Los coeficientes muestran el impacto de cada variable en la duración de congestión (en horas).")
          )
        )
      ),
      
      # TAB 4: Árbol de Decisión (Slide 8)
      tabItem(
        tabName = "arbol",
        fluidRow(
          box(
            title = "🌳 Árbol de Decisión - Mejor Modelo",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotOutput("plot_arbol", height = "700px"),
            hr(),
            p(strong("Parámetros óptimos:"), "cp (complexity parameter) seleccionado mediante validación cruzada.")
          )
        )
      ),
      
      # TAB 5: Red Neuronal (Slide 9)
      tabItem(
        tabName = "red",
        fluidRow(
          box(
            title = "🧠 Arquitectura de la Red Neuronal",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("plot_red_neuronal", height = "700px"),
            hr(),
            p(strong("Configuración:"), "Red feed-forward con capa oculta de tamaño óptimo (size) y decay para regularización.")
          )
        )
      ),
      
      # TAB 6: Gráficos Adicionales (Slides 12-13)
      tabItem(
        tabName = "graficos",
        fluidRow(
          box(
            title = "📉 Residuales vs Predicción - Modelo Ganador",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("plot_residuales", height = "500px")
          ),
          box(
            title = "📊 Importancia de Variables",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("plot_importancia", height = "500px")
          )
        )
      ),
      
      # TAB 7: Tabla Validación Test (Slide 11)
      tabItem(
        tabName = "validacion",
        fluidRow(
          box(
            title = "📋 Resultados de Validación en Datos de Prueba",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("tabla_validacion_test"),
            hr(),
            p(strong("Nota:"), "Estas métricas fueron calculadas en el conjunto de prueba (20% de los datos), nunca visto durante el entrenamiento.")
          )
        ),
        fluidRow(
          valueBoxOutput("ganador_rmse", width = 3),
          valueBoxOutput("ganador_mae", width = 3),
          valueBoxOutput("ganador_r2", width = 3),
          valueBoxOutput("ganador_mape", width = 3)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Cargar datos
  resultados <- reactive({
    req(file.exists("results.csv"))
    read.csv("results.csv")
  })
  
  tiempos <- reactive({
    req(file.exists("training_times.csv"))
    read.csv("training_times.csv")
  })
  
  modelos_data <- reactive({
    req(file.exists("models_and_preprocessing.rds"))
    readRDS("models_and_preprocessing.rds")
  })
  
  # TAB 1: Gráfico de comparación
  output$plot_comparacion <- renderPlotly({
    df <- resultados()
    
    plot_ly(df, x = ~Model, y = ~RMSE, type = 'bar', 
            marker = list(color = c('#d32f2f', '#1976d2', '#388e3c', '#f57c00', '#7b1fa2'))) %>%
      layout(
        title = "Comparación de Modelos por RMSE (menor es mejor)",
        xaxis = list(title = "Modelo"),
        yaxis = list(title = "RMSE (horas)"),
        hovermode = "closest"
      )
  })
  
  output$tabla_metricas <- renderDT({
    df <- resultados()
    
    # Agregar hiperparámetros óptimos
    df$Hiperparametros <- c(
      "N/A (modelo base)",
      "cp = 0.01 o 0.05",
      "size = 3 o 5, decay = 0.1",
      "sigma = 0.05, C = 1 o 2",
      "k = 5 o 7"
    )
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c('RMSE', 'MAE', 'R2', 'MAPE'), digits = 4)
  })
  
  # TAB 2: Tiempos
  output$tiempo_total <- renderValueBox({
    tiempos_df <- tiempos()
    total <- sum(tiempos_df$Training_Time_Seconds)
    valueBox(
      paste0(round(total, 2), " seg"),
      "Tiempo Total de Entrenamiento",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$modelo_rapido <- renderValueBox({
    tiempos_df <- tiempos()
    min_idx <- which.min(tiempos_df$Training_Time_Seconds)
    valueBox(
      tiempos_df$Model[min_idx],
      paste0("Más Rápido (", round(tiempos_df$Training_Time_Seconds[min_idx], 2), " seg)"),
      icon = icon("bolt"),
      color = "green"
    )
  })
  
  output$modelo_lento <- renderValueBox({
    tiempos_df <- tiempos()
    max_idx <- which.max(tiempos_df$Training_Time_Seconds)
    valueBox(
      tiempos_df$Model[max_idx],
      paste0("Más Lento (", round(tiempos_df$Training_Time_Seconds[max_idx], 2), " seg)"),
      icon = icon("hourglass-half"),
      color = "red"
    )
  })
  
  output$plot_tiempos <- renderPlotly({
    df <- tiempos()
    
    plot_ly(df, x = ~Model, y = ~Training_Time_Seconds, type = 'bar',
            marker = list(color = '#ff9800')) %>%
      layout(
        title = "Tiempo de Entrenamiento por Algoritmo (3-fold CV)",
        xaxis = list(title = "Modelo"),
        yaxis = list(title = "Tiempo (segundos)")
      )
  })
  
  output$tabla_tiempos <- renderDT({
    datatable(
      tiempos(),
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE
    ) %>%
      formatRound(columns = 'Training_Time_Seconds', digits = 3)
  })
  
  output$system_info <- renderText({
    paste(
      "Procesador:", Sys.info()["machine"],
      "\nSistema Operativo:", Sys.info()["sysname"], Sys.info()["release"],
      "\nR Version:", R.version.string,
      "\nDataset:", "10,000 observaciones, 24 features",
      "\nValidación Cruzada:", "3-fold CV"
    )
  })
  
  # TAB 3: Regresión Lineal
  output$tabla_coeficientes <- renderDT({
    modelo_data <- modelos_data()
    
    if ("Linear_Regression" %in% names(modelo_data$models)) {
      modelo_lm <- modelo_data$models[["Linear_Regression"]]
      coef_df <- data.frame(
        Variable = names(coef(modelo_lm$finalModel)),
        Coeficiente = as.numeric(coef(modelo_lm$finalModel)),
        stringsAsFactors = FALSE
      )
      
      coef_df <- coef_df[order(abs(coef_df$Coeficiente), decreasing = TRUE), ]
      
      datatable(
        coef_df,
        options = list(pageLength = 15),
        rownames = FALSE
      ) %>%
        formatRound(columns = 'Coeficiente', digits = 6)
    } else {
      datatable(data.frame(Mensaje = "Modelo de Regresión Lineal no disponible"))
    }
  })
  
  # TAB 4: Árbol de Decisión
  output$plot_arbol <- renderPlot({
    if (file.exists("arbol_decision.png")) {
      img <- png::readPNG("arbol_decision.png")
      grid::grid.raster(img)
    } else {
      plot.new()
      text(0.5, 0.5, "Gráfico del árbol no disponible.\nEjecute analisis_completo.R primero.", cex = 1.5)
    }
  })
  
  # TAB 5: Red Neuronal
  output$plot_red_neuronal <- renderPlot({
    if (file.exists("red_neuronal.png")) {
      img <- png::readPNG("red_neuronal.png")
      grid::grid.raster(img)
    } else {
      plot.new()
      text(0.5, 0.5, "Gráfico de red neuronal no disponible.\nEjecute analisis_completo.R primero.", cex = 1.5)
    }
  })
  
  # TAB 6: Gráficos adicionales
  output$plot_residuales <- renderPlot({
    if (file.exists("roc_residuales.png")) {
      img <- png::readPNG("roc_residuales.png")
      grid::grid.raster(img)
    } else {
      plot.new()
      text(0.5, 0.5, "Gráfico de residuales no disponible", cex = 1.5)
    }
  })
  
  output$plot_importancia <- renderPlot({
    if (file.exists("importancia_variables.png")) {
      img <- png::readPNG("importancia_variables.png")
      grid::grid.raster(img)
    } else {
      plot.new()
      text(0.5, 0.5, "Gráfico de importancia no disponible", cex = 1.5)
    }
  })
  
  # TAB 7: Validación Test
  output$tabla_validacion_test <- renderDT({
    df <- resultados()
    
    datatable(
      df,
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE,
      caption = "Métricas calculadas en conjunto de prueba (20% de los datos)"
    ) %>%
      formatRound(columns = c('RMSE', 'MAE', 'R2', 'MAPE'), digits = 4) %>%
      formatStyle(
        'Model',
        target = 'row',
        backgroundColor = styleEqual(c('KNN'), c('#e8f5e9'))
      )
  })
  
  output$ganador_rmse <- renderValueBox({
    df <- resultados()
    mejor <- df[which.min(df$RMSE), ]
    valueBox(
      round(mejor$RMSE, 4),
      "RMSE - Modelo Ganador (K-NN)",
      icon = icon("trophy"),
      color = "green"
    )
  })
  
  output$ganador_mae <- renderValueBox({
    df <- resultados()
    mejor <- df[which.min(df$RMSE), ]
    valueBox(
      round(mejor$MAE, 4),
      "MAE (≈31 minutos)",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$ganador_r2 <- renderValueBox({
    df <- resultados()
    mejor <- df[which.min(df$RMSE), ]
    valueBox(
      paste0(round(mejor$R2 * 100, 2), "%"),
      "R² (Varianza Explicada)",
      icon = icon("percent"),
      color = "orange"
    )
  })
  
  output$ganador_mape <- renderValueBox({
    df <- resultados()
    mejor <- df[which.min(df$RMSE), ]
    valueBox(
      paste0(round(mejor$MAPE, 2), "%"),
      "MAPE",
      icon = icon("percentage"),
      color = "purple"
    )
  })
}

shinyApp(ui, server)
