library(readxl)
library(rjson)

currentSpecFile <- "n:/bcs70/sweep10/BCS46_DataDocumentation-Substantive_Interim_NatCen_03_20170215.xlsx"

######
vars <- read_excel(currentSpecFile, sheet = 2)
vls <- read_excel(currentSpecFile, sheet = 3)
##################
## start constructing a list object

spec <- list(lastupdated = Sys.time())

getModules <- function(vars, vls) {
    lapply(split(vars, vars$Module), function(x) {
        list(vars = getVarsFromModule(x, vls))
    })
}

getVarsFromModule <- function(mdl, vls) {
    vars = apply(mdl, 1, getIndVar, vls = vls)
    names(vars) <- mdl$VARNAME
    vars
}

getIndVar <- function(row, vls) {
    vals <- vls[which(vls$VARNAME == row["VARNAME"]), c("CODE", "CODELABEL")]
    lbls <- vals$CODE
    names(lbls) <- vals$CODELABEL
    list(
        vName = row["VARNAME"],
        varLabel = row["LABEL"],
        format = row["FORMAT"],
        valLabels = lbls,
        missings = c()
    )
}
spec$vars <- getModules(vars, vls)

cat(toJSON(spec ), file = "spec.json")






