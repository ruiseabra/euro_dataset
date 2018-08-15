# use 'browse_data.R' to extract the data subset to be ploted
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(lubridate))
Sys.setenv(TZ = "UTC") 

load("tmp/plot_data.RData")
D <- list()
n <- 31
d <- d[-(2:3),]
for(i in 1:nrow(d)) {
  tmp1 <- d$rng[[i]]
  tmp2 <- apply.daily(with(d$dat[[i]], xts(avg, time)), mean)
  time(tmp2) <- as.Date(time(tmp2))
  tmp1$avg   <- tmp2[tmp1$time] %>% as.numeric %>% round(2)
  tmp1$jday  <- tmp1$time %>% yday
  tmp1 <- group_by(tmp1, jday) %>% summarise(min = mean(min), max = mean(max), avg = mean(avg))
  tmp2 <- zoo::rollmean(select(tmp1, -jday), n) %>% round(2)
  tmp1 <- tail(tmp1, -floor(n/2))
  tmp1 <- head(tmp1, -floor(n/2))
  tmp1[,2:4] <- tmp2
  tmp1$id <- as.character(i)
  D[[i]] <- tmp1
}

dat <- do.call(rbind, D)

ggplot(dat) + 
  theme_minimal() + 
  geom_ribbon(aes(jday, ymin = min, ymax = max, fill = id), alpha = 0.5) +
  geom_line(aes(jday, min, col = id)) +
  geom_line(aes(jday, max, col = id)) +
  geom_line(aes(jday, avg, col = id)) +
  xlab("julian day") + ylab("temp (ºC)") +
  guides(fill = FALSE, color = FALSE)
ggsave("plot.pdf", device = "pdf")  
  
