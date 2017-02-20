library(readxl)
library(rjson)

## get some args from config file
conf <- config::get(file =  "~/bcsconfig.yaml")


currentSpecFile <- paste0(conf$root,
    "/spec/BCS46_DataDocumentation-Substantive_Interim_NatCen_03_20170215.xlsx")

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
    ctg <- data.frame(code = vals$CODE, label = vals$CODELABEL,
                      stringsAsFactors = FALSE, row.names = NULL)
    if(nrow(ctg) == 0) {
        ctg <- data.frame(code = -99, label = "no labels", stringsAsFactors = FALSE, row.names = NULL)
    }
    ctg$missing = FALSE
    list(
        vName = row["VARNAME"],
        varLabel = row["LABEL"],
        format = row["FORMAT"],
        ctg = ctg
    )
}
spec$modules <- getModules(vars, vls)

cat(toJSON(spec ), file = paste0(conf$root, "/json/spec.json"))






