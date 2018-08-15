read.logger <- function(FILES) {
	Sys.setenv(TZ = "UTC")
	DATA <- list()
	pb <- txtProgressBar(max = length(FILES), style = 3)
	for (i in 1:length(FILES)) {
		setTxtProgressBar(pb, i)

		f       <- FILES[i]
		isNS71  <- str_detect(basename(f), "REL_")

		# NS71 and PC files are processed differently
		if (isNS71) {
			# read the file, extract serial, tdelta, dates and temps, and prepare the filenames
			x <- readLines(f, warn = FALSE, encoding = "latin1")

			tmp <- str_detect(x, "iButton Serial Number:") %>% which
			if (!tmp) stop(str_c("could not find the serial number for file:\n\n", f))
			serial <- str_replace(x[tmp], "iButton Serial Number:", "")

			tmp <- str_detect(x, "Difference between iButton and Reference:") %>% which
			if (!tmp) stop(str_c("could not find tdelta for file:\n\n", f))
			tdelta <- str_replace_all(x[tmp], "[^0-9]", "") %>% as.numeric

			x <- x[str_sub(x, 1, 1) == "2"]

			dates <- str_sub(x, 1, 19) %>%
				as.POSIXct(format = "%Y-%m-%d at %H:%M")

			temps <- str_sub(x, 20, 30) %>%
				str_replace_all("[^0-9.]", "") %>%
				as.numeric
		}else{
			# read the file, extract serial, tdelta, dates and temps, and prepare the filenames
			x  <- readLines(f, warn = FALSE)
			x  <- x[x != ""]

			serial <- x[1]

			logs <- grep(",", x)[1] - 1
			tdelta <- x[logs] %>% as.numeric

			x <- tail(x, -logs)
			x <- str_replace_all(x, ",C,", ", ") %>%
				str_split(", ")

			dates <- map_chr(x, 1)
			temps <- map_chr(x, 2) %>%
				str_replace_all(",", ".") %>%
				as.numeric


			d1 <- dates[1]
			# PC files have dates stored in different formats, find which one is being used in the current file
			Format <- if (str_detect(d1, "-")) {
				if (str_detect(substr(d1, 1, 3), "-")) "%d-%m-%Y %T" else "%Y-%m-%d %H:%M"
			}else{
				if (str_detect(d1, "M")) {
					"%m/%d/%y %I:%M:%S %p"
				}else if (nchar(str_split(d1, " ")[[1]][1]) == 8) {
					"%d/%m/%y %H:%M:%S"
				}else{
					"%m/%d/%Y %H:%M"
				}
			}

			dates <- as.POSIXct(dates, format = Format)
		}
		DATA[[i]] <- tibble(time = dates, val = temps, id = serial)
	}
	close(pb)
	do.call(rbind, DATA) %>% arrange(id, time)
}
