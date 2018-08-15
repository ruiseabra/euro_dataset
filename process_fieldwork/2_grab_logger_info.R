# grab info from the "robolimpets@work" file
# only keeping info for logger-dates for which there's data available
#------------------------------------------------------------------------------#

cat("------- GRABBING LOGGER INFO ---- START\n")

# compile a list of all serials for which we have data (serials from actual robolimpet reports)
# also store each file path
filesF <- dir(paths$files, full.names = TRUE, recursive = TRUE)
filesF <- filesF[!str_detect(filesF, "OTHER")]
serials <- basename(filesF) %>%
	str_replace(".txt", "") %>%
	str_sub(4, 30)
filesF  <- tibble(serial = serials, path = filesF)

# grab info from the "robolimpets@work" file
info   <- list()
cal1   <- suppressMessages(read_csv(paths$calibReadings))
cal2   <- suppressMessages(read_csv(paths$calibTemperatures))
shores <- tibble(
	short = c("ev", "al", "sl", "mi", "mo", "to", "lc", "sv", "bi", "rn", "cc", "lz", "wy", "mc", "an", "em", "sc"),
	long  = c("evaristo", "alteirinhos", "slourenco", "mindelo", "moledo", "tourinan", "lacaridad", "svbarquera", "biarritz", "royan", "lecroisic", "landunvez", "wembury", "mcastle", "anglesey", "emlagh", "scairn"),
	lat = c(37.0742, 37.5192, 39.0135, 41.3104, 41.8399, 43.0429, 43.5654, 43.3936, 43.4844, 45.6256, 47.2899, 48.5431, 50.3134, 52.1249, 53.3194, 53.7511, 54.9721),
	lon = c(-8.3037, -8.8126, -9.4224, -8.7424, -8.8751, -9.2900, -6.8281, -4.4403, -1.5632, -1.0626, -2.5424, -4.7514, -4.1071, -10.1100, -4.6616, -9.9067, -5.1796))

sheets <- excel_sheets(paths$atWork)
if(any(nchar(sheets) != 2)) stop("check sheet names")
sheets <- sheets[nchar(sheets) == 2]

shores$servDates <- ""

# extract info from each sheet
cat("file:", basename(paths$atWork), "\n")
pb <- txtProgressBar(max = length(sheets), style = 3)
cat("\n")
for(i in 1:length(sheets)) {
	sh <- sheets[i]
	# i <- 11; sh <- "lc"
	# i <- 3

	# load the file
	x <- read_excel(path = paths$atWork, sheet = sh, col_names = FALSE, col_types = "text")

	# shore
	shore <- tolower(x[1,1])
	ii <- which(shores$long == shore)
	cat(shore, "\n")

	## the layout of columns, here saved for later use in interpreting data columns in x
	columns <- x[1,] %>%
		t %>%
		as.vector %>%
		tail(-1)
	columns <- c(0, cumsum(na.fill(!is.na(columns), FALSE)))

	# servicing dates
	D <- x[1,-1] %>% as.numeric
	D <- D[!is.na(D)]
	D  <- as.Date(D-2, origin = "1900-01-01")
	shores$servDates[ii] <- list(D)

	# microhabitat info
	micro <- x[-(1:3), columns == 0]
	linesWithData <- apply(micro, 1, function(x) !all(is.na(x) | x == ""))
	micro <- micro[linesWithData,] ## removes empty lines
	micro <- micro[,!is.na(micro[1,]) %>% as.logical] ## removes columns with no titles
	colnames(micro) <- micro[1,]
	micro <- micro[-1,]
	micro <- suppressMessages(type_convert(micro))

	# serials, their start and end dates, and all other associated info
	# discard unwanted lines
	x <- x[-(1:4),]
	x <- x[linesWithData[-1],]
	# break by columns
	X <- list()
	for(j in 0:max(columns)) X[[j+1]] <- x[, columns == j]
	# discard unwanted columns
	x <- X[-1] # the 1st column has micro info
	if(any(sapply(x, ncol) < 7)) stop("some columns are incomplete")
	# we are only interested in the serials and the presence/absence of contacts
	x  <- map(x, ~tibble(serials = .x[[6]], contacts = .x[[5]]))
	# replace NAs with ""
	nafill <- function(x, fill = "") {x[is.na(x)] <- fill; x}
	x <- map(x, nafill)
	# save row numbers
	x <- map(x, ~tibble(row = 1:nrow(.x), serials = .x$serials, contacts = .x$contacts))
	# make sure all entries of a certain serial have information about the presence/absence of contacts (currently only the 1st instance has that info)
	serCon <- do.call(rbind, x) %>% filter(serials != "" & contacts != "")
	if((serCon$serials %>% unique %>% length) != nrow(serCon)) stop(str_c("(F2_ERR1) fix contacts info for shore", toupper(shore)))
	for(j in 1:length(x)) x[[j]]$contacts <- match(x[[j]]$serials, serCon$serials) %>% serCon$contacts[.] %>% nafill
	x <- map(x, ~.x[apply(.x, 1, function(y) all(y != "")),])
	# at this point we have all the serials grouped by servicing trip
	# now we need to separate each serial, and identify all relevant pieces of information that relate to that deployment
	# the same serial will appear once per deployment, meaning that many instances of the same serial may be present; however, the information associated with each of those instances will be different, namely regarding the start and end date, and the file path to the report file

	X <- list() # will hold all info related to each robolimpet file (same serials are not collapsed, there is a separate entry for repeated files of the same serial)
	counter <- 1
	# for each servicing trip (except the last one, since for that trip we still don't have reports)
	# for each serial found in that servicing trip
	for(j in 1:(length(x)-1)) for(k in 1:nrow(x[[j]])) {
		# j <- 1; k <- 1
		ser1 <- x[[j]]$serials[k]  # serial short
		# if(ser1 == "1A625B") stop() # use this to check the 1st entry for a given serial
		cont <- str_sub(x[[j]]$contacts[k], 1, 1)  # contact
		# a few robolimpets have been re-used (removed from one shore because the shell was broken, re-built as new loggers and re-deployed); this practice was later abandoned because of all the mess it generates in this database, but still we have to deal with the ones that were nevertheless used before
		# this is done by finding the re-used ones (serials in the file 'robolimpets@work' end with '_') and modifying the serial number accordingly
		if(nchar(ser1) == 5) ser1 <- str_c("0", ser1)
		REUSED <- str_detect(ser1, "_")
		if(REUSED) {
			ser3 <- str_replace(ser1, "_", "41B")
			ser2 <- filesF$serial[str_detect(filesF$serial, ser3)] %>% unique
			ser1 <- str_replace(ser1, "_", "_B")
		}else{
			ser2 <- filesF$serial[str_detect(filesF$serial, ser1)] %>% unique
			ser2 <- ser2[nchar(ser2) == 16] # this step has to be done because if a logger was later re-used, there will be a second version of the same serial ending in 'B' that will be caught by the grep, resulting in 2 serials being selected for ser2
		}
		if(length(ser2) > 1) stop(str_c("(F2_ERR2) more than one file matching serial...\n   ", ser1))

		if(length(ser2)) { # serials for which there are no data are immediately discarded

			file <- filesF$path[str_detect(filesF$path, ser2) & str_detect(filesF$path, str_sub(D[j+1], 1, 10))]
			if(length(file) > 1) stop(str_c("(F2_ERR3) more than one file matching serial...\n   ", ser1, " in ", D[j+1]))
			if(length(file)) { # serials for which there are no data are immediately discarded
				m <- micro[x[[j]]$row[k],]
				X[[counter]] <- tibble(
					serial1  = ser1,
					serial2  = ser2,
					file     = str_split(file, "robolimpet_reports/")[[1]][2],
					start    = D[j],
					end      = D[j+1],
					contacts = as.logical(as.numeric(cont)),
					shore1   = shores$short[match(shore, shores$long)],
					shore2   = shore,
					slope    = m$slope,
					N        = m$N,
					ref1     = m$ref1,
					ref2     = m$ref2,
					micro    = tolower(m$micro),
					height   = switch(str_sub(m$micro, 1, 1), "T" = "top", "M" = "mid", "L" = "low", "S" = "supra"),
					exposure = switch(str_sub(m$micro, 3, 3), "U" = "sun", "H" = "shade"))
				counter <- counter + 1
			}
		}
	}
	X <- do.call(rbind, X)
	if(X$contacts %>% is.na %>% any) stop(str_c("(F2_ERR4) information about presence/absence of contacts is missing for some loggers in shore:", toupper(shore)))
	info[[sh]] <- X

	setTxtProgressBar(pb, i)
	cat("\n")
}

info <- do.call(rbind, info)
cat("------- GRABBING LOGGER INFO ---- END\n\n")

# add calibration data
# (loggers without contacts will remain NA, because they retain factory calibration)
cat("------- ADDING CALIBRATION DETAILS\n")

info$factory_calib <- FALSE
info$run <- info$lo_read <- info$lo_ref <- info$md_read <- info$md_ref <- info$hi_read <- info$hi_ref <- NA
pb <- txtProgressBar(max = nrow(info), style = 3)
for(i in 1:nrow(info)) {
	# i <- 1
	x <- info[i,]
	if(x$contacts) {
		ind  <- match(x$serial2, cal1$serial)
		if(is.na(ind)) stop(str_c("(F2_ERR5) serial missing from the calibration file:\n   ", x$serial2))
		x1 <- cal1[ind, ]
		x2 <- cal2[match(x1$run, cal2$run), ]
		# the factory calibration status is unknown for some loggers (it is stored as NA)
		# in those cases factory calibration is treated as FALSE, thus forcing calibration to be applied
		info$factory_calib[i] <- if(is.na(x1$cal)) FALSE else as.logical(x1$cal)
		info$run[i] <- x1$run
		info$lo_read[i] <- x1$low_read
		info$lo_ref[i]  <- x2$low_ref
		info$md_read[i] <- x1$med_read
		info$md_ref[i]  <- x2$med_ref
		info$hi_read[i] <- x1$high_read
		info$hi_ref[i]  <- x2$high_ref
	}else{
		info$factory_calib[i] <- TRUE
	}
	setTxtProgressBar(pb, i)
}

# save stuff
x <- info
save(x, file = paths$roboInfo)

print("...updating robolimpet servicing dates file")
x <- shores
x$last <- map(x$servDates, last) %>% unlist %>% as.POSIXct(origin = origin)

# add info about whether there are any low shore loggers left in each shore
# this will impact the threshold for good/bad tide classification during fieldwork planning
# this has to be updated manually
x$has_lows <- TRUE
x$has_lows[x$short == "lz"] <- FALSE
x$has_lows[x$short == "to"] <- FALSE
x$has_lows[x$short == "mo"] <- FALSE
x$has_lows[x$short == "al"] <- FALSE

save(x, file = paths$servDates)
print("------- PRE-PROCESSING OF ROBOLIMPER REPORTS COMPLETED")
