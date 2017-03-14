source("getMetaFromFile.R")
fls <- dir("../delivery1", full.names = TRUE)
f <- fls[25]
mt <- getMetaDataList(f)
