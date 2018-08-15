Sys.setenv(TZ = "UTC") 
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(dygraphs))


load("RData/euro_output.RData")
x$serial <- NULL
x$micro <- filter(x$micro, shore != "mindelo")
x$shore <- filter(x$shore, shore != "mindelo")

#input <- list(shores = c("biarritz", "moledo"), micros = "tsu", type = "water")
# ranges
dateRange <- range(c(x$micro$start, x$micro$end))
tempRange <- range(purrr::map(x$micro$data, ~range(.x$avg)))

xx <- x$shore$daily[[1]]
xx <- xts(xx[,2:3], xx$time)
dygraph(xx) %>% dyRangeSelector

# shores and micros for ui
shores <- x$shore$sh
micros <- unique(x$micro$micro)
sh_mic <- x$micro$sh_mic

# colors
col <- list()
# water
col$water  <- set_names(x$shore$col_shores, shores)
# shores
col$shores <- set_names(x$micro$col_micros, x$micro$sh_mic)
# micros
col$micros <- set_names(x$micro$col_shores, x$micro$sh_mic)

# UI ####
ui <- fluidPage(theme = shinytheme("lumen"),
                fluidRow(
                  column(2,
                         # Select type of plot
                         selectInput(inputId  = "type", label = strong("plot type"),
                                     choices  = c("shore", "micro", "water"), selected = "water"),
                         
                         hr(),
                         
                         # Select shores
                         column(6,
                                checkboxGroupInput(inputId  = "shores", label = strong("shores"),
                                                   choices  = shores, selected = shores[1])),
                         
                         # Select micros
                         column(6,
                                checkboxGroupInput(inputId  = "micros", label = strong("micros"),
                                                   choices  = micros,
                                                   selected = micros[1]))),
                  
                  # Plot
                  mainPanel(
                    plotOutput(outputId = "plotALL", height = "200px",
                               brush = brushOpts(
                                 id = "plot_brush", direction = "xy", resetOnNew = FALSE)),
                    
                    plotOutput(outputId = "plotZOOM", height = "500px"))))

# SERVER ####
server <- function(input, output) {
# input <- list(type = "shore", shores = c("cc"), micros = c("tsu", "msu", "ssu")); ranges <- list(x = as.POSIXct(c("2017-01-01", "2018-03-01")))

  # Subset data
  DATA <- reactive({
    if(input$type == "water") {
      dat <- x$shore$water[shores %in% input$shores]
      rng <- x$shore$daily[shores %in% input$shores]
      col <- x$shore$col_shores[shores %in% input$shores]
    }
    if(input$type != "water") {
      if(input$type == "shore") {
        ind <- expand.grid(input$shores[1], input$micros)
      }else{
        ind <- expand.grid(input$shores, input$micros[1])
      }
      ind <- cbind(ind, x$shore$shore[match(ind[,1], shores)])
      ind <- tibble(sh = ind[,3], micro = ind[,2]) %>%
        mutate(sh_mic = str_c(sh, "_", micro))
      ind <- sh_mic %in% ind$sh_mic
      dat <- x$micro$data[ind]
      rng <- x$micro$daily[ind]
      if(input$type == "shore") {
        col <- x$micro$col_micros[ind]
      }else{
        col <- x$micro$col_shores[ind]
      }
    }
    d <- tibble(dat = dat, rng = rng, col = col)
    d
  })
  
  DATA_TIME <- reactive({
    d <- DATA()
    d$dat <- purrr::map(d$dat, ~.x[.x$time >= ranges$x[1] & .x$time <= ranges$x[2], ])
    d
  })
  
  output$plotALL <- renderPlot({
    d  <- DATA()
    p <- ggplot() +
      theme_classic() +
      xlab("") + ylab("") +
      guides(color = guide_legend(title = "")) +
      theme(legend.text = element_text(size = 15), axis.text = element_text(size = 15)) +
      coord_cartesian(ylim = tempRange)
    p <- p + scale_color_manual(values = d$col)
    for(i in 1:nrow(d)) p <- p + geom_linerange(data = d$rng[[i]], aes(time, ymin = min, ymax = max), col = d$col[i], size = 1)
    p
  })
  
  output$plotZOOM <- renderPlot({
    d  <- DATA_TIME()
    p <- ggplot() +
      theme_classic() +
      xlab("") + ylab("") +
      guides(color = guide_legend(title = "")) +
      theme(legend.text = element_text(size = 15), axis.text = element_text(size = 15)) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
    p <- p + scale_color_manual(values = d$col)
    for(i in 1:nrow(d)) p <- p + geom_line(data = d$dat[[i]], aes(time, avg), col = d$col[i], size = 1)
    p
  })
  
  # when a double-click happens, check if there's a brush on the plot
  # if so, zoom to the brush bounds, if not, reset the zoom
  ranges <- reactiveValues(x = NULL, y = NULL)
  observe({
    brush <- input$plot_brush
    if(!is.null(brush)) {
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax) * 24 * 3600, origin = origin)
      ranges$y <- c(brush$ymin, brush$ymax)
    }else{
      ranges$x <- dateRange
      ranges$y <- tempRange
    }
  })
}

# APP ####
shinyApp(ui = ui, server = server)

