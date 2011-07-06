# 
# 
# Author: Emilio
###############################################################################


.onLoad <- function(library, pkg){
	description <- readLines(system.file("DESCRIPTION", package = "SixSigma"))
	vers <- grep("Version:", description, ignore.case = TRUE, value = TRUE)
	vers <- gsub(pattern = "Version:", replacement = "", vers, ignore.case = TRUE)
	vers <- gsub(pattern = " ", replacement = "", vers)
	ss.message <- paste("SixSigma package, version",vers,"\n",
			"Type 'citation(\"SixSigma\")' for citing in publications.\n")
	packageStartupMessage(ss.message)
}
