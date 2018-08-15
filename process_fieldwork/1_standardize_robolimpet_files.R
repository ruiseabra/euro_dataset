# standardize robolimpet files
# both ns71 and pc files are saved with the same format
# originals are kept for backup
#------------------------------------------------------------------------------#

# list all robolimpet files
filesF <- dir(paths$files, full.names = TRUE, recursive = TRUE)
# but exclude all that have been moved into subfolders (thus keeping only those that data to be imported)
filesF <- filesF[!str_detect(filesF, "OTHER")]
# flag lowercase NS71 files filenames (must manually make them UPPERCASE)
low <- grepl("rel_", basename(filesF))
if(any(low)) {
	newPaths <- str_c(dirname(filesF[low]), "/", basename(filesF[low]) %>% toupper)
	invisible(file.rename(filesF[low], newPaths))
	filesF[low] <- newPaths
}

# exclude from the list all files that have already been processed
alreadyProcessed <- str_detect(basename(filesF), pattern = glob2rx("NS_*.txt")) | str_detect(basename(filesF), pattern = glob2rx("PC_*.txt"))
filesF <- filesF[!alreadyProcessed]

# if there are no files left for processing, the work is done
cat("------- PROCESSING", length(filesF), "FILES ---- START\n")

# otherwise, process them
if(length(filesF)) {
	pb <- txtProgressBar(max = length(filesF), style = 3)
	for(i in 1:length(filesF)) {
		setTxtProgressBar(pb, i)

		f       <- filesF[i]
		isNS71  <- str_detect(basename(f), "REL_")

		# NS71 and PC files are processed differently
		if(isNS71) {
			# read the file, extract serial, tdelta, dates and temps, and prepare the filenames
			x <- readLines(f, warn=F, encoding = "latin1")

			tmp <- str_detect(x, "iButton Serial Number:") %>% which
			if(!tmp) stop(str_c("could not find the serial number for file:\n\n", f))
			serial <- str_replace(x[tmp], "iButton Serial Number:", "")

			tmp <- str_detect(x, "Difference between iButton and Reference:") %>% which
			if(!tmp) stop(str_c("could not find tdelta for file:\n\n", f))
			tdelta <- str_replace_all(x[tmp], "[^0-9]", "") %>% as.numeric

			x <- x[str_sub(x, 1, 1) == "2"]

			dates <- str_sub(x, 1, 19) %>%
				as.POSIXct(format = "%Y-%m-%d at %H:%M") %>%
				str_sub(1, 16)

			temps <- str_sub(x, 20, 30) %>%
				str_replace_all("[^0-9.]", "") %>%
				as.numeric %>%
				formatC(digits = 2, format = "f")

			f2 <- str_c(dirname(f), "/NS_", serial, ".txt")
			f3 <- str_c(dirname(f), "/OTHER/NS_original/")
		}else{
			# read the file, extract serial, tdelta, dates and temps, and prepare the filenames
			x  <- readLines(f, warn = FALSE)
			x  <- x[x != ""]

			serial <- x[1]
			tdelta <- x[2] %>% as.numeric

			x <- tail(x, -2)
			x <- str_replace_all(x, ",", ".")

			line <- x[1]
			# PC files have dates stored in different formats, find which one is being used in the current file
			Format <- if(str_detect(line, "-")) {
				if(str_detect(substr(line, 1, 3), "-")) "%d-%m-%Y %T" else "%Y-%m-%d %H:%M"
			}else{
				if(str_detect(line, "M")) {
					"%m/%d/%y %I:%M:%S %p"
					}else if(nchar(str_split(line, " ")[[1]][1]) == 8) {
						"%d/%m/%y %H:%M:%S"
					}else{
						"%m/%d/%Y %H:%M"
					}
			}

			dates <- str_split(x, ".C.") %>%
				map_chr(1) %>%
				as.POSIXct(format = Format) %>%
				str_sub(1, 16)

			temps <- str_split(x, ".C.") %>%
				map_chr(2) %>%
				as.numeric %>%
				formatC(digits = 2, format = "f")

			f2 <- str_c(dirname(f), "/PC_", serial, ".txt")
			f3 <- str_c(dirname(f), "/OTHER/PC_original/")
		}
		f4 <- str_c(f3, basename(f))
		dir.create(f3, recursive = TRUE, showWarnings = FALSE)

		# write the new file and move the orixginal one into a subfolder
		x <- cbind(dates, temps)
		write.table(rbind(serial, tdelta), file = f2, row.names = FALSE, col.names = FALSE, quote = FALSE)
		write.table(x, file = f2, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ", ", append = TRUE)
		file.rename(f, f4)
	}
	cat("\n")
}
cat("------- PROCESSING", length(filesF), "FILES ---- END\n")
