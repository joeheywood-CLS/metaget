library(jqr)
library(readxl)
library(dplyr)
library(RJSONIO)
library(haven)

### I'm having a go here at producing an s4  class ####
### http://adv-r.had.co.nz/S4.html ###
## Here - we have two classes: the more general metaList class
## and the ctg class for categorical variables

setClass("ctg",
         representation(
             code = "numeric",
             label = "character",
             missing = "logical",
             freq = "numeric")
         )

setClass("var",
         representation(
             vName = "character",
             varLabel = "character")
         )

setClass("ctgVar",
         representation(
             ctg = "ctg"),
         contains = "var"
         )

setClass("multiVarList",
         representation(
             root = "character",
             opt = "data.frame",
             ctg = "ctg",
             fq = "data.frame")
         )



## extend metalist with frequencies for categorical variables

## look at discrete variables too

############################################################
############################################################

labelledToCtgVar <- function(x, n) {
    ctg <- NULL
    code = as.numeric(attr(x, "labels"))
    y <- as.data.frame(table(x))
    yy <- left_join(
        data.frame(x = as.character(code), stringsAsFactors = FALSE),
        y)
    yy$Freq[is.na(yy$Freq)] <- 0
    if("labels" %in% names(attributes(x))) {
        ctg <- new("ctg",
                   code = code,
                   label = trimws(names(attr(x, "labels"))),
                   missing = rep(FALSE, length(attr(x, "labels"))),
                   freq = yy$Freq
                   )
    }
    lb <- attr(x, "label")
    if(is.na(lb)) lb <- ""
    new("ctgVar",
        vName = trimws(n),
        varLabel = trimws(attr(x, "label")),
        ctg = ctg)
}


labelledToMulti <- function(x, lblObj) {
    ctg <- NULL
    a <- attributes(lblObj[[ x[1] ]])
    if("labels" %in% names(a)) {
        ctg <- new("ctg",
                   code = as.numeric(a$labels),
                   label = trimws(names(a$labels)),
                   missing = rep(FALSE, length(a))
                   )
    }
    opts <- do.call(rbind, lapply(x, function(y) {
        z <- attributes(lblObj[[y]])
        if("label" %in% names(z)) {
            lbl <- z$label
        } else {
            lbl <- ""
        }
        cbind(data.frame(vName = y, varLabel = lbl, stringsAsFactors = FALSE),
              createMultiFreq(ctg, table(lblObj[[y]])))
    }))
    opts$shortLabel <- gsub(commonString(opts$varLabel), "", opts$varLabel)
    new("multiVarList",
        root = commonString(x), opt = opts, ctg = ctg)
}


createMultiFreq <- function(ctg, tbl) {
    save(ctg, tbl, file = "debug.Rda")
    j <- data.frame(Var1 = ctg@code, lbls = gsub(" ", "_", ctg@label),
                    stringsAsFactors = FALSE)
    v <- as.data.frame(tbl, stringsAsFactors = FALSE)
    v$Var1 <- as.numeric(v$Var1)
    j <- left_join(j, v)
    jj <- as.data.frame(t(j$Freq))
    colnames(jj) <- j$lbls
    jj
}

getSpecInfo <- function(f) {
    vrbs <- read_excel(f, sheet = 2)
    lbls <- read_excel(f, sheet = 3) %>%
	select(VARNAME, CODE, CODELABEL)
    list(v = vrbs, l = lbls)
}

specToCtg <- function(v, sp) {
    ctg <- NULL
    lb <- sp$l[which(sp$l$VARNAME == v), ]
    vrb <- sp$v[which(sp$v$VARNAME == v), ]
    ctg <- NULL
    if(nrow(lb) > 0) {
	ctg <- new("ctg",
		   code = lb$CODE,
		   label = lb$CODELABEL,
		   missing = rep(FALSE, nrow(lb))
		   )
    } else {
        ctg <- new("ctg")
    }
    new("ctgVar",
	vName = trimws(v),
	varLabel = trimws(vrb$LABEL),
	ctg = ctg)
}

commonString <- function(x) {
    x <- x[which(nchar(x) > 0)]
    ix <- 1
    cnt <- TRUE
    while(all(substr(x, 1, ix) == substr(x[1], 1, ix)) ) {
        ix <- ix + 1
    }
    substr(x[1], 1, (ix - 1))
}



## some kind of map module variables list of funcs:
## createModuleList
## addMultiList
## viewNumeric
## finishModuleList #--> where/how to save?


## how to save/reload from json
## comparisons/unit tests for each module
## separate functions/program for each

## use previous sweep data if possible

startMapping <- function(f) {
    sp <- getSpecInfo(f)
}

collapseToMulti <- function(root, lst, patt = "%s\\d+$") {
    multi <- which(grepl(sprintf(patt, root), lst))
    print(length(multi))
    if(length(multi) < 2) {
        print("Check root, less than two.")
        return(list)
    }
    lst[multi[1]] <- sprintf("%s__%s", root, patt)
    multi <- multi[-1]
    lst[-multi]
}
