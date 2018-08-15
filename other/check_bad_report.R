i <- 681

# ------- load stuff
library(tidyverse)
library(stringr)
library(shiny)
library(shinythemes)
library(lubridate)
Sys.setenv(TZ = "UTC") 
path <- dirname(getwd())

folders     <- lapply(list(
  base       = "", 
  reports    = "robolimpet_reports/", 
  loggerInfo = "logger_info/robolimpet_info.RData"
), function(x) paste(path, x, sep="/"))

load(folders$loggerInfo)
REPORTS <- x

# ------- select all reports from the same shore and servicing date
tmp <- REPORTS[i,]
ALL <- filter(REPORTS, dirname(file) == dirname(tmp$file))
iii <- which(ALL$file == tmp$file)

# ------- read reports
DATA <- list()
for(ii in 1:nrow(ALL)){
  x <- ALL[ii,]
  path2   <- str_c(folders$reports, x$file)
  tdelta <- read_lines(path2, skip = 1, n_max = 1)
  tdelta <- if(tdelta == "NA") 0 else as.numeric(tdelta)
  dat    <- suppressMessages(read_csv(path2, col_names = FALSE, skip = 2))
  colnames(dat) <- c("time", "temp")
  if(first(dat$time) > last(dat$time)) dat <- dat[nrow(dat):1,]
  DATA[[ii]] <- list(dat = dat, tdelta = tdelta, details = as.list(x))
}

# ------- calibrate data
calibrate <- function(report) {
  dat <- report$dat
  x   <- report$details
  
  if(x$contacts) {
    missing_cal <- !x$factory_calib & length(unlist(x$calib)) == 1
    if(missing_cal) stop("file missing calib data")
    if(!missing_cal) {
      lo_ref  <- x$lo_ref
      md_ref  <- x$md_ref
      hi_ref  <- x$hi_ref
      
      lo_read <- x$lo_read
      md_read <- x$md_read
      hi_read <- x$hi_read
      
      # calculate differences between chamber and logger measurements (from calibration data)
      D_lo  <- lo_read - lo_ref
      D_md  <- md_read - md_ref
      D_hi  <- hi_read - hi_ref
      
      # calculate quadratic function parameters
      Alpha <- (D_lo - D_hi) + ((D_md - D_lo) * (hi_ref - lo_ref)) / (md_ref - lo_ref)
      Beta  <- hi_ref^2 - (md_ref * hi_ref) + (md_ref * lo_ref) - (hi_ref * lo_ref)
      C     <- -Alpha / Beta
      B     <- (D_md - D_lo - C * ((md_ref^2) - (lo_ref^2))) / (md_ref - lo_ref)  
      A     <- D_lo - B * lo_ref - C * lo_ref^2
      
      # adjust readings with quadratic function
      dat$temp <- round(dat$temp - A - B * dat$temp - C * dat$temp^2, 3)
    }
  }
  dat
}
for(ii in 1:length(DATA)) DATA[[ii]]$dat <- calibrate(DATA[[ii]])

# ------- make a 2nd version with the clock drift corrected using the available tdelta
for(ii in 1:length(DATA)) {
  dat    <- DATA[[ii]]$dat
  tdelta <- DATA[[ii]]$tdelta
  
  if(tdelta != 0) {
    n      <- nrow(dat)
    tdelta <- -(tdelta / (n-1))
    tdelta <- c(0, tdelta * (1:(n-1)))
    dates2 <- dat$time + tdelta
    DATA[[ii]]$dat2 <- dat
    DATA[[ii]]$dat2$time <- dates2
  }
}

# ------- store data in tibble
ALL$tdelta <- map_dbl(DATA, "tdelta")
ALL$data   <- purrr::map(DATA, "dat")
ALL$data2  <- purrr::map(DATA, "dat2")

# ------- shiny
# UI ####
ui <- fluidPage(theme = shinytheme("lumen"),
                plotOutput('plot1', height = 300, brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = FALSE)),
                plotOutput('plot2'),
                hr(),
                fluidRow(column(10,sliderInput("tdelta", label = "tdelta", min = -500000, max = 500000, value = 1, step = 1000))))

# SERVER ####
server <- function(input, output) {
  # ranges <- list(x = map(ALL$data, ~range(.x$time)) %>% range %>% as.POSIXct(origin = origin)); input <- list(tdelta = 10000)
  
  # when a double-click happens, check if there's a brush on the plot
  # if so, zoom to the brush bounds, if not, reset the zoom
  ranges <- reactiveValues(x = NULL)
  observe({
    brush <- input$plot_brush
    if(!is.null(brush)) {
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
    }else{
      ranges$x <- NULL
    }
  })
  
  prepareData <- reactive({
    dat_all    <- ALL$data2
    dat_uncorr <- ALL$data[[iii]]
    dat_corr   <- ALL$data2[[iii]]
    dat_react  <- dat_uncorr
    
    dat     <- dat_uncorr
    n       <- nrow(dat)
    tdelta  <- -(input$tdelta / (n-1))
    tdelta <- c(0, tdelta * (1:(n-1)))
    dates2  <- dat$time + tdelta
    dat$time <- dates2
    dat_react <- dat
    
    dat <- dat_all
    col <- c(rep("black", length(dat)), "red", "blue", "green")
    dat[[length(dat)+1]] <- dat_uncorr
    dat[[length(dat)+1]] <- dat_corr
    dat[[length(dat)+1]] <- dat_react
    
    DAT <- dat
    if(!is.null(ranges$x)) dat <- map(dat, ~.x[.x$time >= ranges$x[1] & .x$time <= ranges$x[2], ])
    list(DAT = DAT, dat = dat, col = col)
  })
  
  output$plot1 <- renderPlot({
    x <- prepareData()
    p <- ggplot() + xlab("") + ylab("") + theme_minimal() +
      ggtitle(str_c("micro: ", ALL$micro[iii], "\ntdelta: ", ALL$tdelta[[iii]], "\nfile: ", tmp$file))
    
    for(ii in 1:(length(x$DAT)-3)){
      p <- p + geom_line(data = x$DAT[[ii]], aes(time, temp))
    }
    ii <- length(x$DAT)
    p <- p + geom_line(data = x$DAT[[ii]], aes(time, temp), col = x$col[ii])
    p
  })
  
  output$plot2 <- renderPlot({
    x <- prepareData()
    p <- ggplot() + xlab("") + ylab("") + theme_minimal() +
      ggtitle(str_c("tdelta: ", input$tdelta))
    
    for(ii in 1:length(x$dat)){
      p <- p + geom_line(data = x$dat[[ii]], aes(time, temp), col = x$col[ii])
    }
    p
  })
}

# APP ####
shinyApp(ui = ui, server = server)

