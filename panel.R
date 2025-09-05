library(shiny)
library(readxl)
library(ade4)
library(DT)
library(ggplot2)
library(plotly)
library(ggrepel)
library(dplyr)
library(pheatmap)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
library(shinydashboard)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', system-ui, sans-serif;
        background: #f8fafc;
        margin: 0;
      }
      .main-layout {
        display: flex;
        min-height: 100vh;
        background: white;
        box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1);
      }
      .sidebar {
        width: 260px;
        background: linear-gradient(180deg, #1e293b 0%, #334155 100%);
        color: white;
      }
      .sidebar h4 {
        text-align: center;
        padding: 2rem 1rem;
        margin: 0;
        font-weight: 700;
        border-bottom: 1px solid rgba(255,255,255,0.1);
      }
      .nav-pills {
        list-style: none;
        padding: 1rem 0;
        margin: 0;
        display: block;
      }
      .nav-pills li {
        display: block;
        width: 100%;
      }
      .nav-pills li a {
        display: flex;
        align-items: center;
        padding: 1rem 1.5rem;
        color: rgba(255,255,255,0.8);
        text-decoration: none;
        transition: all 0.2s;
        border-left: 4px solid transparent;
        width: 100%;
        box-sizing: border-box;
      }
      .nav-pills li a i {
        width: 20px;
        margin-right: 0.75rem;
        flex-shrink: 0;
      }
      .nav-pills li a:hover, .nav-pills li.active a {
        background: rgba(59,130,246,0.2);
        color: white;
        border-left-color: #3b82f6;
      }
      .main-content {
        flex: 1;
        padding: 2rem;
      }
      .section-title {
        display: flex;
        align-items: center;
        gap: 0.75rem;
        font-size: 1.75rem;
        font-weight: 700;
        color: #1e293b;
        margin-bottom: 2rem;
        padding-bottom: 1rem;
        border-bottom: 2px solid #e2e8f0;
      }
      .section-title i {
        color: #3b82f6;
      }
      .card {
        background: white;
        border-radius: 12px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        border: 1px solid #e2e8f0;
        margin-bottom: 1.5rem;
      }
      .card-header {
        background: #3b82f6;
        color: white;
        padding: 1rem 1.5rem;
        font-weight: 600;
        border-radius: 12px 12px 0 0;
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }
      .card-body {
        padding: 1.5rem;
      }
      .form-control, .form-select {
        border: 2px solid #e2e8f0;
        border-radius: 8px;
        padding: 0.75rem;
      }
      .form-control:focus, .form-select:focus {
        border-color: #3b82f6;
        box-shadow: 0 0 0 3px rgba(59,130,246,0.1);
      }
      .btn-primary {
        background: #3b82f6;
        border: none;
        border-radius: 8px;
        padding: 0.75rem 1.5rem;
        font-weight: 600;
      }
      .btn-primary:hover {
        background: #1d4ed8;
        transform: translateY(-1px);
      }
      .metrics {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 1.5rem;
        margin-bottom: 2rem;
      }
      .metric {
        background: white;
        border-radius: 12px;
        padding: 1.5rem;
        text-align: center;
        border-left: 4px solid #3b82f6;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .metric-value {
        font-size: 2rem;
        font-weight: 800;
        color: #1e293b;
      }
      .metric-label {
        color: #64748b;
        font-size: 0.875rem;
        font-weight: 600;
        text-transform: uppercase;
      }
      .tab-pane {
        display: none;
      }
      .tab-pane.active {
        display: block;
      }
      .plot-wrapper {
        background: white;
        border-radius: 12px;
        border: 1px solid #e2e8f0;
        overflow: hidden;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        margin-bottom: 1.5rem;
      }
      .plot-title {
        background: #1e293b;
        color: white;
        padding: 1rem 1.5rem;
        font-weight: 600;
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }
      .plot-content {
        padding: 1.5rem;
      }
      @media (max-width: 768px) {
        .main-layout { 
          flex-direction: column; 
        }
        .sidebar { 
          width: 100%; 
        }
        .nav-pills { 
          display: flex; 
          flex-wrap: wrap; 
          padding: 0.5rem 0;
        }
        .nav-pills li { 
          flex: 1; 
          min-width: 120px; 
        }
        .nav-pills li a { 
          justify-content: center; 
          padding: 0.75rem 0.5rem; 
          border-left: none; 
          border-bottom: 3px solid transparent; 
          font-size: 0.875rem;
        }
        .nav-pills li a:hover, .nav-pills li.active a { 
          border-left: none; 
          border-bottom-color: #3b82f6; 
        }
        .main-content { 
          padding: 1rem; 
        }
      }
    "))
  ),
  
  div(class = "main-layout",
      div(class = "sidebar",
          h4("STATIS-KFOLD"),
          tags$ul(class = "nav nav-pills", id = "myTabs", role = "tablist",
                  tags$li(role = "presentation", class = "active",
                          tags$a(href = "#configuracion", role = "tab", `data-toggle` = "tab",
                                 HTML('<i class="fas fa-cog"></i> Configuración'))
                  ),
                  tags$li(role = "presentation",
                          tags$a(href = "#datos", role = "tab", `data-toggle` = "tab",
                                 HTML('<i class="fas fa-table"></i> Datos'))
                  ),
                  tags$li(role = "presentation",
                          tags$a(href = "#correlacion", role = "tab", `data-toggle` = "tab",
                                 HTML('<i class="fas fa-project-diagram"></i> Correlación'))
                  ),
                  tags$li(role = "presentation",
                          tags$a(href = "#compromiso", role = "tab", `data-toggle` = "tab",
                                 HTML('<i class="fas fa-balance-scale"></i> Compromiso'))
                  ),
                  tags$li(role = "presentation",
                          tags$a(href = "#intraestructura", role = "tab", `data-toggle` = "tab",
                                 HTML('<i class="fas fa-sitemap"></i> Intra-estructura'))
                  ),
                  tags$li(role = "presentation",
                          tags$a(href = "#cuadrantes", role = "tab", `data-toggle` = "tab",
                                 HTML('<i class="fas fa-th-large"></i> Cuadrantes'))
                  ),
                  tags$li(role = "presentation",
                          tags$a(href = "#validacion", role = "tab", `data-toggle` = "tab",
                                 HTML('<i class="fas fa-check-circle"></i> Validación'))
                  )
          )
      ),
      
      div(class = "main-content",
          div(class = "tab-content",
              
              # Tab 1: Configuración
              div(class = "tab-pane active", id = "configuracion",
                  div(class = "section-title",
                      tags$i(class = "fas fa-cog"),
                      "Configuración"
                  ),
                  
                  div(class = "card",
                      div(class = "card-header",
                          tags$i(class = "fas fa-upload"),
                          "Carga de Datos"
                      ),
                      div(class = "card-body",
                          fileInput("file", NULL, accept = c(".xlsx"),
                                    buttonLabel = HTML('<i class="fas fa-folder-open"></i> Archivo'),
                                    placeholder = "Seleccionar archivo Excel"
                          ),
                          div(style = "color: #64748b; font-size: 0.875rem;",
                              "El archivo debe contener columnas de agrupación y datos numéricos."
                          )
                      )
                  ),
                  
                  fluidRow(
                    column(6,
                           div(class = "card",
                               div(class = "card-header", tags$i(class = "fas fa-sliders-h"), "Parámetros"),
                               div(class = "card-body",
                                   numericInput("num_clusters", "Número de Clusters:", value = 3, min = 2, max = 10),
                                   numericInput("k_folds", "K-folds:", value = 5, min = 2, max = 10)
                               )
                           )
                    ),
                    column(6,
                           div(class = "card",
                               div(class = "card-header", tags$i(class = "fas fa-eye"), "Visualización"),
                               div(class = "card-body",
                                   checkboxInput("mostrar_etiquetas", "Mostrar etiquetas", value = FALSE),
                                   checkboxInput("mostrar_centroides", "Mostrar centroides", value = TRUE),
                                   checkboxInput("mostrar_hulls", "Mostrar envolventes", value = TRUE)
                               )
                           )
                    )
                  )
              ),
              
              # Tab 2: Datos
              div(class = "tab-pane", id = "datos",
                  div(class = "section-title", tags$i(class = "fas fa-table"), "Datos y STATIS"),
                  
                  div(class = "plot-wrapper",
                      div(class = "plot-title", tags$i(class = "fas fa-database"), "Vista de Datos"),
                      div(class = "plot-content", DTOutput("tablaDatos"))
                  ),
                  
                  div(class = "plot-wrapper",
                      div(class = "plot-title", tags$i(class = "fas fa-chart-area"), "Gráfico STATIS"),
                      div(class = "plot-content", plotOutput("graficoStatis", height = "500px"))
                  ),
                  
                  fluidRow(
                    column(6,
                           div(class = "card",
                               div(class = "card-header", tags$i(class = "fas fa-chart-pie"), "Componentes"),
                               div(class = "card-body", verbatimTextOutput("valoresPropios"))
                           )
                    ),
                    column(6,
                           div(class = "plot-wrapper",
                               div(class = "plot-title", tags$i(class = "fas fa-chart-line"), "K-Plot"),
                               div(class = "plot-content", plotOutput("graficoKplot", height = "300px"))
                           )
                    )
                  ),
                  
                  div(class = "card",
                      div(class = "card-header", tags$i(class = "fas fa-calculator"), "Coeficientes RV"),
                      div(class = "card-body", verbatimTextOutput("mensaje"))
                  )
              ),
              
              # Tab 3: Correlación
              div(class = "tab-pane", id = "correlacion",
                  div(class = "section-title", tags$i(class = "fas fa-project-diagram"), "Correlación"),
                  
                  div(class = "card",
                      div(class = "card-body",
                          selectInput("tipo_correlacion", "Tipo de Análisis:",
                                      choices = list(
                                        "Heatmap RV" = "heatmap",
                                        "Weights y Cos2" = "weights_cos2",
                                        "Scatter Plot" = "scatter"
                                      ), width = "100%")
                      )
                  ),
                  
                  conditionalPanel(
                    condition = "input.tipo_correlacion == 'heatmap'",
                    div(class = "plot-wrapper",
                        div(class = "plot-title", tags$i(class = "fas fa-th"), "Matriz RV"),
                        div(class = "plot-content", plotOutput("heatmapRV", height = "600px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.tipo_correlacion == 'weights_cos2'",
                    div(class = "plot-wrapper",
                        div(class = "plot-title", tags$i(class = "fas fa-chart-bar"), "Weights y Cos2"),
                        div(class = "plot-content", plotlyOutput("graficoWeightsCos2", height = "500px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.tipo_correlacion == 'scatter'",
                    div(class = "plot-wrapper",
                        div(class = "plot-title", tags$i(class = "fas fa-scatter-chart"), "Scatter Plot"),
                        div(class = "plot-content", plotlyOutput("scatterWeightsCos2", height = "500px"))
                    )
                  )
              ),
              
              # Tab 4: Compromiso
              div(class = "tab-pane", id = "compromiso",
                  div(class = "section-title", tags$i(class = "fas fa-balance-scale"), "Compromiso"),
                  
                  div(class = "plot-wrapper",
                      div(class = "plot-title", tags$i(class = "fas fa-chart-scatter"), "Biplot"),
                      div(class = "plot-content", plotlyOutput("biplotCompromiso", height = "600px"))
                  ),
                  
                  div(class = "plot-wrapper",
                      div(class = "plot-title", tags$i(class = "fas fa-table"), "Coordenadas"),
                      div(class = "plot-content", DTOutput("tablaCompromiso"))
                  )
              ),
              
              # Tab 5: Intra-estructura
              div(class = "tab-pane", id = "intraestructura",
                  div(class = "section-title", tags$i(class = "fas fa-sitemap"), "Intra-estructura"),
                  
                  div(class = "plot-wrapper",
                      div(class = "plot-title", tags$i(class = "fas fa-layer-group"), "Todos los Grupos"),
                      div(class = "plot-content", plotlyOutput("intraEstructuraTodos", height = "600px"))
                  ),
                  
                  div(class = "plot-wrapper",
                      div(class = "plot-title", tags$i(class = "fas fa-filter"), "Vista Individual"),
                      div(class = "plot-content",
                          fluidRow(
                            column(4, uiOutput("selector_grupo")),
                            column(8, plotlyOutput("intraEstructuraIndividual", height = "500px"))
                          )
                      )
                  )
              ),
              
              # Tab 6: Cuadrantes
              div(class = "tab-pane", id = "cuadrantes",
                  div(class = "section-title", tags$i(class = "fas fa-th-large"), "Cuadrantes"),
                  
                  div(class = "card",
                      div(class = "card-body",
                          fluidRow(
                            column(8,
                                   selectInput("tipo_cuadrante", "Cuadrante:",
                                               choices = list(
                                                 "Vista General" = "general",
                                                 "Cuadrante I" = "cuadrante1",
                                                 "Cuadrante II" = "cuadrante2",
                                                 "Cuadrante III" = "cuadrante3",
                                                 "Cuadrante IV" = "cuadrante4"
                                               ), width = "100%")
                            ),
                            column(4,
                                   checkboxInput("mostrar_grid_cuadrantes", "Mostrar grid", value = TRUE)
                            )
                          )
                      )
                  ),
                  
                  conditionalPanel(
                    condition = "input.tipo_cuadrante == 'general'",
                    div(class = "plot-wrapper",
                        div(class = "plot-title", tags$i(class = "fas fa-expand"), "Vista General"),
                        div(class = "plot-content", plotlyOutput("cuadranteGeneral", height = "700px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.tipo_cuadrante == 'cuadrante1'",
                    div(class = "plot-wrapper",
                        div(class = "plot-title", tags$i(class = "fas fa-square"), "Cuadrante I"),
                        div(class = "plot-content", plotlyOutput("cuadrante1", height = "600px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.tipo_cuadrante == 'cuadrante2'",
                    div(class = "plot-wrapper",
                        div(class = "plot-title", tags$i(class = "fas fa-square"), "Cuadrante II"),
                        div(class = "plot-content", plotlyOutput("cuadrante2", height = "600px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.tipo_cuadrante == 'cuadrante3'",
                    div(class = "plot-wrapper",
                        div(class = "plot-title", tags$i(class = "fas fa-square"), "Cuadrante III"),
                        div(class = "plot-content", plotlyOutput("cuadrante3", height = "600px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.tipo_cuadrante == 'cuadrante4'",
                    div(class = "plot-wrapper",
                        div(class = "plot-title", tags$i(class = "fas fa-square"), "Cuadrante IV"),
                        div(class = "plot-content", plotlyOutput("cuadrante4", height = "600px"))
                    )
                  )
              ),
              
              # Tab 7: Validación
              div(class = "tab-pane", id = "validacion",
                  div(class = "section-title", tags$i(class = "fas fa-check-circle"), "Validación"),
                  
                  div(class = "metrics",
                      div(class = "metric",
                          div(class = "metric-value", textOutput("correlacionMedia")),
                          div(class = "metric-label", "Correlación Promedio")
                      ),
                      div(class = "metric",
                          div(class = "metric-value", textOutput("desviacionEstandar")),
                          div(class = "metric-label", "Desviación Estándar")
                      ),
                      div(class = "metric",
                          div(class = "metric-value", textOutput("intervalosConfianza")),
                          div(class = "metric-label", "IC 95%")
                      )
                  ),
                  
                  div(class = "plot-wrapper",
                      div(class = "plot-title", tags$i(class = "fas fa-chart-bar"), "Resultados por Fold"),
                      div(class = "plot-content", plotlyOutput("graficoBarras", height = "500px"))
                  ),
                  
                  div(class = "card",
                      div(class = "card-header", tags$i(class = "fas fa-info-circle"), "Resumen"),
                      div(class = "card-body", verbatimTextOutput("resumenKfold"))
                  )
              )
          )
      )
  )
)

# El server function permanece exactamente igual
server <- function(input, output, session) {
  valores_statis <- reactiveValues(
    rv = NULL, obj = NULL, correlaciones = NULL, valores_propios_texto = NULL,
    compromiso = NULL, intra_estructura = NULL, datos_procesados = NULL,
    grupos_unicos = NULL, colores_grupos = NULL
  )
  
  datos_react <- reactive({
    req(input$file)
    tryCatch({
      datos <- read_excel(input$file$datapath, sheet = 1)
      return(datos)
    }, error = function(e) {
      showNotification(paste("Error al leer el archivo:", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
  
  observe({
    req(datos_react())
    withCallingHandlers({
      tryCatch({
        datos <- datos_react()
        if (ncol(datos) < 3) {
          showNotification("Se requieren al menos 3 columnas.", type = "error")
          return(NULL)
        }
        
        grupos_unicos <- unique(datos[[1]])
        grupos_unicos <- sort(grupos_unicos)
        valores_statis$grupos_unicos <- grupos_unicos
        
        if (length(grupos_unicos) <= 8) {
          colores <- c("#3b82f6", "#1e40af", "#1d4ed8", "#2563eb", "#1e3a8a", "#475569", "#64748b", "#94a3b8")
          colores <- colores[1:length(grupos_unicos)]
        } else {
          colores <- colorRampPalette(c("#3b82f6", "#1e40af", "#475569"))(length(grupos_unicos))
        }
        names(colores) <- grupos_unicos
        valores_statis$colores_grupos <- colores
        
        col_numericas <- 3:ncol(datos)
        factor_agrupacion <- factor(datos[[1]])
        datos_num <- datos[, col_numericas]
        
        if (!any(sapply(datos_num, is.numeric))) {
          showNotification("No se encontraron columnas numéricas válidas.", type = "error")
          return(NULL)
        }
        
        kta1 <- ktab.within(withinpca(datos_num, factor_agrupacion, scaling = "total", scannf = FALSE))
        statis1 <- statis(kta1, scann = FALSE)
        
        valores_statis$rv <- statis1$RV
        valores_statis$obj <- statis1
        valores_statis$datos_procesados <- datos
        
        medidasGovernanza <- colnames(datos_num)
        compromiso <- data.frame(CP1 = statis1$C.li$C1, CP2 = statis1$C.li$C2, labels = medidasGovernanza)
        valores_statis$compromiso <- compromiso
        
        CP1 <- statis1$C.Co$C1
        CP2 <- statis1$C.Co$C2
        n_grupos <- length(grupos_unicos)
        n_obs_por_grupo <- length(CP1) / n_grupos
        
        Labels <- rep(datos[[3]], n_grupos)
        Tamaño <- rep(datos[[2]], n_grupos)
        Grupos <- rep(grupos_unicos, each = n_obs_por_grupo)
        
        intraEstructura <- data.frame(
          CP1 = CP1, CP2 = CP2, Labels = Labels[1:length(CP1)],
          Tamaño = Tamaño[1:length(CP1)], Grupo = Grupos[1:length(CP1)]
        )
        valores_statis$intra_estructura <- intraEstructura
        
        tryCatch({
          if (is.list(statis1$RV.eig) && !is.null(statis1$RV.eig$values)) {
            eigenvalues_inter <- statis1$RV.eig$values
          } else if (is.numeric(statis1$RV.eig)) {
            eigenvalues_inter <- statis1$RV.eig
          } else {
            eigenvalues_inter <- NULL
          }
          
          if (is.list(statis1$C.eig) && !is.null(statis1$C.eig$values)) {
            eigenvalues_comp <- statis1$C.eig$values
          } else if (is.numeric(statis1$C.eig)) {
            eigenvalues_comp <- statis1$C.eig
          } else {
            eigenvalues_comp <- NULL
          }
          
          texto_resultados <- ""
          
          if (!is.null(eigenvalues_inter) && length(eigenvalues_inter) >= 2) {
            var_explained_inter <- cumsum(eigenvalues_inter / sum(eigenvalues_inter)) * 100
            var_2_comp_inter <- var_explained_inter[2]
            texto_inter <- paste("Inter-estructura: Los 2 primeros componentes explican",
                                 round(var_2_comp_inter, 1), "% de la varianza")
            texto_resultados <- texto_inter
          }
          
          if (!is.null(eigenvalues_comp) && length(eigenvalues_comp) >= 2) {
            var_explained_comp <- cumsum(eigenvalues_comp / sum(eigenvalues_comp)) * 100
            var_2_comp_comp <- var_explained_comp[2]
            texto_comp <- paste("Compromiso: Los 2 primeros componentes explican",
                                round(var_2_comp_comp, 1), "% de varianza")
            if (texto_resultados != "") {
              texto_resultados <- paste(texto_resultados, texto_comp, sep = "\n")
            } else {
              texto_resultados <- texto_comp
            }
          }
          
          if (texto_resultados == "") {
            texto_resultados <- "No se pudieron extraer los valores propios"
          }
          
          valores_statis$valores_propios_texto <- texto_resultados
          
        }, error = function(e) {
          valores_statis$valores_propios_texto <- paste("Error al calcular valores propios:", e$message)
        })
        
        showNotification("Análisis STATIS completado exitosamente.", type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error durante el análisis STATIS:", e$message), type = "error", duration = 10)
      })
    }, warning = function(w) {
      invokeRestart("muffleWarning")
    })
  })
  
  output$selector_grupo <- renderUI({
    req(valores_statis$grupos_unicos)
    selectInput("grupo_seleccionado", "Grupo:", choices = valores_statis$grupos_unicos,
                selected = valores_statis$grupos_unicos[1])
  })
  
  output$tablaDatos <- renderDT({
    req(datos_react())
    datatable(datos_react(), options = list(pageLength = 10, scrollX = TRUE,
                                            dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')), class = 'cell-border stripe hover')
  })
  
  output$graficoStatis <- renderPlot({
    req(valores_statis$obj)
    plot(valores_statis$obj)
  })
  
  output$graficoKplot <- renderPlot({
    req(valores_statis$obj)
    kplot(valores_statis$obj)
  })
  
  output$valoresPropios <- renderPrint({
    req(valores_statis$valores_propios_texto)
    cat(valores_statis$valores_propios_texto)
  })
  
  output$mensaje <- renderPrint({
    req(valores_statis$rv)
    cat("Coeficientes RV del STATIS:\n")
    print(round(valores_statis$rv, 4))
  })
  
  output$heatmapRV <- renderPlot({
    req(valores_statis$rv, valores_statis$grupos_unicos)
    tryCatch({
      cor_data <- valores_statis$rv
      if (is.null(cor_data) || !is.matrix(cor_data)) return(NULL)
      
      grupos <- valores_statis$grupos_unicos
      if (nrow(cor_data) != length(grupos) || ncol(cor_data) != length(grupos)) {
        showNotification("Error: Las dimensiones de la matriz RV no coinciden con los grupos", type = "warning")
        return(NULL)
      }
      
      rownames(cor_data) <- grupos
      colnames(cor_data) <- grupos
      
      annotation_row <- data.frame(Cluster = factor(grupos), row.names = grupos)
      annotation_col <- annotation_row
      
      if (length(grupos) <= 8) {
        cluster_colors <- c("#3b82f6", "#1e40af", "#1d4ed8", "#2563eb", "#1e3a8a", "#475569", "#64748b", "#94a3b8")
        cluster_colors <- cluster_colors[1:length(grupos)]
      } else {
        cluster_colors <- colorRampPalette(c("#3b82f6", "#1e40af", "#475569"))(length(grupos))
      }
      names(cluster_colors) <- levels(annotation_row$Cluster)
      annotation_colors <- list(Cluster = cluster_colors)
      
      pheatmap::pheatmap(cor_data, annotation_row = annotation_row, annotation_col = annotation_col,
                         annotation_colors = annotation_colors, cluster_rows = TRUE, cluster_cols = TRUE,
                         display_numbers = TRUE, number_format = "%.3f", fontsize_number = 10, fontsize = 11,
                         angle_col = 45, color = colorRampPalette(c("#f8fafc", "#3b82f6", "#1e293b"))(100),
                         border_color = "white", cellwidth = 40, cellheight = 40)
      
    }, error = function(e) {
      showNotification(paste("Error al generar heatmap:", e$message), type = "error")
      return(NULL)
    })
  }, height = 600, width = 800)
  
  output$graficoWeightsCos2 <- renderPlotly({
    req(valores_statis$obj, valores_statis$grupos_unicos)
    
    grupos <- valores_statis$grupos_unicos
    rvMatriz <- data.frame(Grupo = grupos, Weights = valores_statis$obj$RV.tabw, Cos2 = valores_statis$obj$cos2)
    data_long <- pivot_longer(rvMatriz, cols = c(Weights, Cos2), names_to = "Variable", values_to = "Valor")
    
    p <- ggplot(data_long, aes(x = Grupo, y = Valor, fill = Variable,
                               text = paste("Grupo:", Grupo, "<br>Variable:", Variable, "<br>Valor:", round(Valor, 3)))) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("Weights" = "#3b82f6", "Cos2" = "#1e40af")) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_blank(),
                              legend.position = "top") + labs(x = "Grupos", y = "Valores", fill = "Métricas")
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = TRUE) %>% config(displayModeBar = TRUE)
  })
  
  output$scatterWeightsCos2 <- renderPlotly({
    req(valores_statis$obj, valores_statis$grupos_unicos)
    
    grupos <- valores_statis$grupos_unicos
    panelMatrices <- data.frame(Cos2 = valores_statis$obj$cos2, Weights = valores_statis$obj$RV.tabw, Grupo = grupos)
    
    p <- ggplot(panelMatrices, aes(x = Cos2, y = Weights, color = Grupo,
                                   text = paste("Grupo:", Grupo, "<br>Cos2:", round(Cos2, 3),
                                                "<br>Weights:", round(Weights, 3)))) +
      geom_point(size = 4, alpha = 0.8) + geom_smooth(method = "lm", se = TRUE, color = "gray", linetype = "dashed", alpha = 0.3) +
      scale_color_manual(values = valores_statis$colores_grupos) + theme_minimal() +
      theme(plot.title = element_blank()) + labs(x = "Cos2", y = "Weights")
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = TRUE) %>% config(displayModeBar = TRUE)
  })
  
  output$biplotCompromiso <- renderPlotly({
    req(valores_statis$compromiso)
    
    compromiso <- valores_statis$compromiso
    p <- ggplot(compromiso, aes(x = CP1, y = CP2, label = labels,
                                text = paste("Variable:", labels, "<br>CP1:", round(CP1, 3), "<br>CP2:", round(CP2, 3)))) +
      geom_segment(aes(x = 0, y = 0, xend = CP1, yend = CP2), arrow = arrow(length = unit(0.3, "cm")),
                   color = "#3b82f6", alpha = 0.8, size = 0.8) +
      geom_point(aes(x = CP1, y = CP2), color = "#1e40af", size = 3, alpha = 0.8) +
      geom_text_repel(aes(x = CP1, y = CP2), size = 3, max.overlaps = 20) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7) +
      theme_minimal() + theme(plot.title = element_blank()) + labs(x = "CP1", y = "CP2")
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE) %>% config(displayModeBar = TRUE)
  })
  
  output$tablaCompromiso <- renderDT({
    req(valores_statis$compromiso)
    compromiso_round <- valores_statis$compromiso
    compromiso_round[,1:2] <- round(compromiso_round[,1:2], 4)
    datatable(compromiso_round, options = list(pageLength = 15, scrollX = TRUE, dom = 'Bfrtip',
                                               buttons = c('copy', 'csv', 'excel')), class = 'cell-border stripe hover')
  })
  
  generar_cuadrante_plotly <- function(datos_intra, condicion_x, condicion_y, titulo) {
    req(valores_statis$grupos_unicos, valores_statis$colores_grupos, input$num_clusters)
    if (is.null(datos_intra) || nrow(datos_intra) == 0) return(NULL)
    
    grupos <- valores_statis$grupos_unicos
    colores <- valores_statis$colores_grupos
    
    if (condicion_x == "positivo" && condicion_y == "positivo") {
      datos_filtrados <- datos_intra %>% filter(CP1 > 0 & CP2 > 0)
    } else if (condicion_x == "negativo" && condicion_y == "positivo") {
      datos_filtrados <- datos_intra %>% filter(CP1 < 0 & CP2 > 0)
    } else if (condicion_x == "negativo" && condicion_y == "negativo") {
      datos_filtrados <- datos_intra %>% filter(CP1 < 0 & CP2 < 0)
    } else {
      datos_filtrados <- datos_intra %>% filter(CP1 > 0 & CP2 < 0)
    }
    
    num_clusters_actual <- if (nrow(datos_filtrados) < input$num_clusters) max(1, nrow(datos_filtrados)) else input$num_clusters
    
    if (nrow(datos_filtrados) == 0) {
      p <- ggplot() + annotate("text", x = 0, y = 0, label = "No hay datos en este cuadrante", size = 6, color = "gray60") +
        theme_minimal() + labs(x = "CP1", y = "CP2")
      return(ggplotly(p))
    }
    
    if (nrow(datos_filtrados) > 1 && num_clusters_actual > 1) {
      set.seed(123)
      kmeans_result <- kmeans(datos_filtrados[, c("CP1", "CP2")], centers = num_clusters_actual)
      datos_filtrados$Cluster <- factor(kmeans_result$cluster)
      centroides <- as.data.frame(kmeans_result$centers)
      centroides$Cluster <- factor(1:nrow(centroides))
      
      p <- ggplot() + geom_point(data = datos_filtrados,
                                 aes(x = CP1, y = CP2, color = Grupo, shape = Cluster,
                                     text = paste("Grupo:", Grupo, "<br>Cluster:", Cluster, "<br>CP1:", round(CP1, 3), "<br>CP2:", round(CP2, 3))),
                                 size = 4, alpha = 0.8) + scale_color_manual(values = colores) + theme_minimal() +
        theme(plot.title = element_blank()) + labs(x = "CP1", y = "CP2")
      
      if (input$mostrar_centroides) {
        p <- p + geom_point(data = centroides, aes(x = CP1, y = CP2), color = "black", size = 6, shape = 3, stroke = 2)
      }
      if (input$mostrar_grid_cuadrantes) {
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7)
      }
    } else {
      p <- ggplot(datos_filtrados, aes(x = CP1, y = CP2, color = Grupo,
                                       text = paste("Grupo:", Grupo, "<br>CP1:", round(CP1, 3), "<br>CP2:", round(CP2, 3)))) +
        geom_point(size = 4, alpha = 0.8) + scale_color_manual(values = colores) + theme_minimal() +
        theme(plot.title = element_blank()) + labs(x = "CP1", y = "CP2")
      
      if (input$mostrar_grid_cuadrantes) {
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7) +
          geom_vline(xintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7)
      }
    }
    
    if (input$mostrar_etiquetas && "Labels" %in% colnames(datos_filtrados)) {
      p <- p + geom_text_repel(data = datos_filtrados, aes(x = CP1, y = CP2, label = Labels), size = 2)
    }
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = TRUE) %>% config(displayModeBar = TRUE)
  }
  
  output$cuadranteGeneral <- renderPlotly({
    req(valores_statis$intra_estructura, valores_statis$colores_grupos)
    
    datos_intra <- valores_statis$intra_estructura
    colores <- valores_statis$colores_grupos
    
    p <- ggplot(datos_intra, aes(x = CP1, y = CP2, color = Grupo,
                                 text = paste("Grupo:", Grupo, "<br>CP1:", round(CP1, 3), "<br>CP2:", round(CP2, 3)))) +
      geom_point(size = 3, alpha = 0.8) + scale_color_manual(values = colores) +
      theme_minimal() + theme(plot.title = element_blank()) + labs(x = "CP1", y = "CP2")
    
    if (input$mostrar_grid_cuadrantes) {
      p <- p + geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.8) +
        geom_vline(xintercept = 0, linetype = "solid", color = "black", alpha = 0.8) +
        annotate("text", x = max(datos_intra$CP1) * 0.8, y = max(datos_intra$CP2) * 0.8, label = "I", size = 8, color = "gray50", alpha = 0.7) +
        annotate("text", x = min(datos_intra$CP1) * 0.8, y = max(datos_intra$CP2) * 0.8, label = "II", size = 8, color = "gray50", alpha = 0.7) +
        annotate("text", x = min(datos_intra$CP1) * 0.8, y = min(datos_intra$CP2) * 0.8, label = "III", size = 8, color = "gray50", alpha = 0.7) +
        annotate("text", x = max(datos_intra$CP1) * 0.8, y = min(datos_intra$CP2) * 0.8, label = "IV", size = 8, color = "gray50", alpha = 0.7)
    }
    
    if (input$mostrar_etiquetas && "Labels" %in% colnames(datos_intra)) {
      p <- p + geom_text_repel(aes(label = Labels), size = 2, max.overlaps = 15)
    }
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = TRUE) %>% config(displayModeBar = TRUE)
  })
  
  output$cuadrante1 <- renderPlotly({
    req(valores_statis$intra_estructura, input$num_clusters)
    generar_cuadrante_plotly(valores_statis$intra_estructura, "positivo", "positivo", "Cuadrante I")
  })
  
  output$cuadrante2 <- renderPlotly({
    req(valores_statis$intra_estructura, input$num_clusters)
    generar_cuadrante_plotly(valores_statis$intra_estructura, "negativo", "positivo", "Cuadrante II")
  })
  
  output$cuadrante3 <- renderPlotly({
    req(valores_statis$intra_estructura, input$num_clusters)
    generar_cuadrante_plotly(valores_statis$intra_estructura, "negativo", "negativo", "Cuadrante III")
  })
  
  output$cuadrante4 <- renderPlotly({
    req(valores_statis$intra_estructura, input$num_clusters)
    generar_cuadrante_plotly(valores_statis$intra_estructura, "positivo", "negativo", "Cuadrante IV")
  })
  
  output$intraEstructuraTodos <- renderPlotly({
    req(valores_statis$intra_estructura, valores_statis$colores_grupos)
    
    datos_intra <- valores_statis$intra_estructura
    colores <- valores_statis$colores_grupos
    
    p <- ggplot(datos_intra, aes(x = CP1, y = CP2, color = Grupo,
                                 text = paste("Grupo:", Grupo, "<br>CP1:", round(CP1, 3), "<br>CP2:", round(CP2, 3)))) +
      geom_point(size = 3, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", alpha = 0.7) +
      scale_color_manual(values = colores) + theme_minimal() +
      theme(plot.title = element_blank()) + labs(x = "CP1", y = "CP2")
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = TRUE) %>% config(displayModeBar = TRUE)
  })
  
  output$intraEstructuraIndividual <- renderPlotly({
    req(valores_statis$intra_estructura, input$grupo_seleccionado, valores_statis$colores_grupos)
    
    datos_intra <- valores_statis$intra_estructura
    grupo_seleccionado <- input$grupo_seleccionado
    colores <- valores_statis$colores_grupos
    
    datos_grupo <- datos_intra %>% filter(Grupo == grupo_seleccionado)
    color_grupo <- colores[grupo_seleccionado]
    
    p <- ggplot(datos_grupo, aes(x = CP1, y = CP2,
                                 text = paste("Grupo:", Grupo, "<br>CP1:", round(CP1, 3), "<br>CP2:", round(CP2, 3)))) +
      geom_point(size = 4, color = color_grupo, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", alpha = 0.7) +
      theme_minimal() + theme(plot.title = element_blank()) + labs(x = "CP1", y = "CP2")
    
    if (input$mostrar_etiquetas && "Labels" %in% colnames(datos_grupo)) {
      p <- p + geom_text_repel(aes(label = Labels), size = 3, max.overlaps = 15)
    }
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE) %>% config(displayModeBar = TRUE)
  })
  
  # Validación cruzada
  observe({
    req(datos_react(), input$k_folds)
    withCallingHandlers({
      tryCatch({
        datos <- datos_react()
        col_numericas <- 3:ncol(datos)
        lista_statis <- split(datos[, col_numericas], datos[[1]])
        lista_statis <- lapply(lista_statis, function(x) as.data.frame(lapply(x, as.numeric)))
        
        lista_tablas <- lista_statis
        k <- input$k_folds
        individuos <- rownames(lista_statis[[1]])
        
        if (length(individuos) < k) {
          k <- max(2, length(individuos))
          showNotification(paste("Ajustando k-folds a", k, "debido a datos insuficientes"), type = "warning")
        }
        
        folds <- cut(seq_along(individuos), breaks = k, labels = FALSE)
        correlaciones <- c()
        
        for (i in 1:k) {
          test_idx <- which(folds == i)
          train_idx <- setdiff(1:length(individuos), test_idx)
          indiv_train <- individuos[train_idx]
          indiv_test <- individuos[test_idx]
          
          train_tablas <- lapply(lista_tablas, function(tabla) tabla[indiv_train, ])
          train_combined <- data.frame()
          nombres_tablas <- names(train_tablas)
          
          for (j in seq_along(train_tablas)) {
            temp_tabla <- train_tablas[[j]]
            temp_tabla$tabla <- nombres_tablas[j]
            train_combined <- rbind(train_combined, temp_tabla)
          }
          
          col_numericas_train <- 1:(ncol(train_combined) - 1)
          kta1_train <- ktab.within(withinpca(train_combined[, col_numericas_train],
                                              factor(train_combined$tabla), scaling = "total", scannf = FALSE))
          statis1_train <- statis(kta1_train, scann = FALSE)
          test_tablas <- lapply(lista_tablas, function(tabla) tabla[indiv_test, ])
          
          cor_fold <- c()
          for (j in seq_along(test_tablas)) {
            tab_test <- test_tablas[[j]]
            tab_mean_train <- colMeans(train_tablas[[j]], na.rm = TRUE)
            
            cor_vals <- apply(tab_test, 1, function(x) {
              if (sd(x, na.rm = TRUE) == 0 || sd(tab_mean_train, na.rm = TRUE) == 0) {
                return(0)
              } else {
                cor(x, tab_mean_train, use = "complete.obs")
              }
            })
            cor_fold <- c(cor_fold, mean(cor_vals, na.rm = TRUE))
          }
          correlaciones <- c(correlaciones, mean(cor_fold, na.rm = TRUE))
        }
        
        valores_statis$correlaciones <- correlaciones
        
      }, error = function(e) {
        showNotification(paste("Error durante la validación cruzada:", e$message), type = "error")
      })
    }, warning = function(w) { invokeRestart("muffleWarning") })
  })
  
  output$correlacionMedia <- renderText({
    req(valores_statis$correlaciones)
    round(mean(valores_statis$correlaciones, na.rm = TRUE), 3)
  })
  
  output$desviacionEstandar <- renderText({
    req(valores_statis$correlaciones)
    round(sd(valores_statis$correlaciones, na.rm = TRUE), 3)
  })
  
  output$intervalosConfianza <- renderText({
    req(valores_statis$correlaciones)
    correlaciones <- valores_statis$correlaciones
    media <- mean(correlaciones, na.rm = TRUE)
    se <- sd(correlaciones, na.rm = TRUE) / sqrt(length(correlaciones))
    ci_lower <- media - 1.96 * se
    ci_upper <- media + 1.96 * se
    paste0("[", round(ci_lower, 3), ", ", round(ci_upper, 3), "]")
  })
  
  output$resumenKfold <- renderPrint({
    req(datos_react(), valores_statis$correlaciones)
    
    datos <- datos_react()
    correlaciones <- valores_statis$correlaciones
    grupos <- unique(datos[[1]])
    col_numericas <- 3:ncol(datos)
    
    cat("=== RESUMEN DE VALIDACIÓN CRUZADA ===\n\n")
    cat("Configuración:\n")
    cat("• Número de folds:", length(correlaciones), "\n")
    cat("• Grupos analizados:", length(grupos), "(", paste(grupos, collapse = ", "), ")\n")
    cat("• Variables numéricas:", length(col_numericas), "\n")
    cat("• Dimensiones dataset:", nrow(datos), "×", ncol(datos), "\n\n")
    
    cat("Resultados por fold:\n")
    for (i in 1:length(correlaciones)) {
      cat("• Fold", i, ":", round(correlaciones[i], 4), "\n")
    }
    
    cat("\nEstadísticas resumen:\n")
    cat("• Media:", round(mean(correlaciones, na.rm = TRUE), 4), "\n")
    cat("• Mediana:", round(median(correlaciones, na.rm = TRUE), 4), "\n")
    cat("• Desviación estándar:", round(sd(correlaciones, na.rm = TRUE), 4), "\n")
    cat("• Mín:", round(min(correlaciones, na.rm = TRUE), 4), "\n")
    cat("• Máx:", round(max(correlaciones, na.rm = TRUE), 4), "\n")
  })
  
  output$graficoBarras <- renderPlotly({
    req(valores_statis$correlaciones)
    
    correlaciones <- valores_statis$correlaciones
    df_barras <- data.frame(
      Fold = paste("Fold", 1:length(correlaciones)),
      Correlacion = correlaciones,
      Color = ifelse(correlaciones >= mean(correlaciones), "Por encima", "Por debajo")
    )
    
    p <- ggplot(df_barras, aes(x = Fold, y = Correlacion, fill = Color,
                               text = paste("Fold:", Fold, "<br>Correlación:", round(Correlacion, 4)))) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_hline(yintercept = mean(correlaciones), color = "#dc3545", linetype = "dashed", size = 1) +
      scale_fill_manual(values = c("Por encima" = "#3b82f6", "Por debajo" = "#64748b")) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_blank(),
                              legend.position = "top") + labs(x = "Folds", y = "Correlación", fill = "Rendimiento")
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = TRUE) %>% config(displayModeBar = TRUE)
  })
}

shinyApp(ui, server)