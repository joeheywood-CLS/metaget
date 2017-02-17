library(haven)
library(rjson)
library(dplyr)

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


mapToSpec <- function() {
    ## assign list of variable names to a vector
    ## loop through modules
    ## add new node with module name, and spec data
    ## basis for shiny app?
    mn <- fromJSON(file = "../json/main.json")
    spc <- fromJSON(file = "../json/spec.json")
    for(mod in names(spc$modules)) {
        mn <- mapSpecModuleToMeta(mn, spc$modules[[mod]], mod)
    }
    cat(toJSON(mn), file = "../json/main_mapped.json")

}

mapSpecModuleToMeta <- function(mn, mod, modName) {
    modVars <- names(mod$vars)
    inMain <- names(mn)[which(names(mn) %in% modVars)]
    for(v in inMain) {
        cat(paste0(v, " "))
        mn[[v]]$spec <- mod$vars[[v]]
        mn[[v]]$mapped <- TRUE
        mn$module = modName
    }
    mn
}

getComparisonObject <- function(a) {
    spc <- data.frame(code = as.character(a$spec$ctg$code), spcLabel = a$spec$ctg$label,
                      stringsAsFactors=F)
    ma <- data.frame(code = as.character(a$ctg$code), mnLabel = a$ctg$label,
                     frq = as.numeric(unlist(a$ctg$Freq)), stringsAsFactors=F)
    frqTab = full_join(ma, spc)
    list(vName = a$vName, varLabel = a$varLabel, format = a$format, frqTab = frqTab)
}



## This shoule be replicated and adjusted to other clusters of variables. But
## for now it's just loops with an ordinal number at the end.

assignToLoop <- function(root, jsn) {
    loopNode <- list(root = root)
    indVars <- names(jsn)[grepl(paste(root, "\\d$"), names(jsn))]
    loopNode$labels <- rep("", length(indVars))
    names(loopNode$labels) <- indVars
    loopNode$ctg <- data.frame(code = jsn[[indVars[1]]]$ctg$code, stringsAsFactors = FALSE)
    for(vr in indVars) {
        ind <- jsn[[vr]]
        ctg <- ctgToDF(ind$ctg) %>% select(code, Freq)
        colnames(ctg)[2] <- vr
        loopNode$ctg <- full_join(loopNode$ctg, ctg)
        loopNode$labels[vr] <- ind$varLabel
    }
    loopNode
}

ctgToDF <- function(ctg) {
    out <- data.frame(code = ctg$code, label = ctg$label, stringsAsFactors = FALSE)
    if("missing" %in% names(ctg)) out$missing <- ctg$missing
    if("Freq" %in% names(ctg)) out$Freq <- ctg$Freq
    out
}


## ok. next task. Get all names from a module. Include two (at the moment) functions:
## 1. Add loop to json list
## 2. Add regular variable
## -> come up with function to aggregate variable names and separate out loops:
## eg. gsub("\\d+$", "_loop", vrbs); vrbs <- unique(vrbs)
## start thinking about shiny app



### Different sub-types of formats ###
## Categorical
## - Yes/No
## - Loop Yes/No
##
## Other Numeric
## - Date subvars: y/m/d (Day of week?)
