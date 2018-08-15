library(tidyverse)
library(stringr)
library(lubridate)
library(shiny)
Sys.setenv(TZ = "UTC")

source("functions.R")

folder <- file.choose() %>% dirname
files  <- dir(folder, full.names = TRUE, recursive = TRUE)
files  <- files[grepl(".txt", files, ignore.case = TRUE) | grepl(".csv", files, ignore.case = TRUE)]

# read data
if (length(files)) x <- read.logger(files)
x$id2 <- x$id %>% str_sub(9, 14)
x$hi  <- FALSE

dateRange <- range(x$time)
tempRange <- range(x$val)
serials <- unique(x$id2)

# UI ####
ui <- fluidPage(
	fluidRow(
		column(1,
					 # Select loggers to plot
					 checkboxGroupInput(inputId  = "Lplot", label = "plot",
					 									 choices  = serials,
					 									 selected = serials)),
		column(1,
					 # Select loggers to highlight
					 checkboxGroupInput(inputId  = "Lhigh", label = "highlight",
					 									 choices  = serials)),

		# Plot
		mainPanel(
			plotOutput(outputId = "plot1", height = "200px",
								 brush = brushOpts(id = "plotBrush", resetOnNew = FALSE, direction = "x")),
			plotOutput(outputId = "plot2", height = "500px")
			)))

# SERVER ####
# input <- list(Lplot = serials[1:6], Lhigh = serials[1]); ranges <- list(x = ymd_h(c("2018-02-01 02", "2018-03-30 10")))
server <- function(input, output) {

	ranges <- reactiveValues(x = NULL)

	VALS <- reactive({
		v <- filter(x, id2 %in% unique(c(input$Lplot, input$Lhigh)))
		v$hi[v$id2 %in% input$Lhigh] <- TRUE
		v
	})

	YLIM <- reactive({
		y <- VALS()
		if (!is.null(ranges$x[1])) y <- filter(y, between(time, ranges$x[1], ranges$x[2]))
		range(y$val)
	})

	PLOT <- reactive({
		v <- VALS()

		P <- ggplot(v) +
			coord_cartesian(xlim = dateRange, ylim = tempRange) +
			geom_line(aes(x = time, y = val, group = id), size = 1, col = "grey") +
			geom_line(data = filter(v, hi), aes(x = time, y = val, col = id), size = 1) +
			guides(color = FALSE) +
			labs(x = "", y = "") +
			theme_classic() +
			theme(axis.text = element_text(size = 15),
						panel.border = element_rect(colour = "black", fill = NA))

		P
	})

	output$plot1 <- renderPlot({
		P <- PLOT()
		P
	})

	output$plot2 <- renderPlot({
		P <- PLOT()
		P <- P +
			coord_cartesian(xlim = ranges$x, ylim = YLIM())
		P
	})

	# when a double-click happens, check if there's a brush on the plot
	# if so, zoom to the brush bounds, if not, reset the zoom
	observe({
		brush <- input$plotBrush
		if (is.null(brush)) {
			ranges$x <- dateRange
		}else{
			ranges$x <- as_datetime(c(brush$xmin, brush$xmax))
		}
	})
}

# APP ####
shinyApp(ui = ui, server = server)
