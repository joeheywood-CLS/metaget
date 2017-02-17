library(haven)
library(rjson)


getMetaDataList <- function(f) {
    fl <- read_sav(f)
    out <- lapply(colnames(fl), extractMetaFromVar, df = fl)
    names(out) <- colnames(fl)
    out
}

extractMetaFromVar <- function(nm, df) {
    att <- attributes(df[[nm]])
    frq <- getVarFreq(df[[nm]])
    ctg <- data.frame(code = att$labels, label = names(att$labels),
                      stringsAsFactors=FALSE, row.names=NULL)
    if(nrow(ctg) == 0) {
        ctg <- data.frame(code = -99, label = "no labels",
                          stringsAsFactors = FALSE, row.names = NULL)
    }
    if("code" %in% colnames(ctg) & nrow(frq$tbl) > 0) {
        frq$tbl$code <- as.character(frq$tbl$code)
        ctg$code <- as.character(ctg$code)
        ctg <- full_join(ctg, frq$tbl, by = "code")
    } else {
        print(paste0("Nowt for ", nm))
    }
    frq$tbl <- NULL
    list(
        vName = nm,
        varLabel = att$label,
        format = att$format.spss,
        ctg = ctg,
        frq = frq
    )
}

getVarFreq <- function(v) {
    tbl <- data.frame()
    summary <- c()
    unq  <- unique(v)
    numVals <- length(unq)
    numPos <- length(which(unq > 0))
    if(numPos < 15) {
        varType <- "categorical"
        tbl <- as.data.frame(table(v), stringsAsFactors = FALSE)
        colnames(tbl)[1] <- "code"
        if(any(is.na(as.numeric(tbl$code)))) {
            varType = "character"
        } else {
            tbl$code <- as.numeric(tbl$code)
        }
    } else {
        varType <- "discrete"
        summary <- unlist(summary(v))
    }
    numNeg <- length(which(unq < 0))
    list(numberVals = numVals, varType = varType, numNeg = numNeg,
         numPos = numPos, tbl = tbl, summary = summary)
}


mapToSpec <- function(flmt) {
    ## assign list of variable names to a vector
    ## loop through modules
    ## add new node with module name, and spec data
    ## basis for shiny app?
}

mapSpecModuleToMeta <- function(mn, mod) {

}

