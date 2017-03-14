## testing the json view tests file.
## chooseCRANmirror(81)
## install.packages("jqr")
## install.packages("readxl")
## install.packages("xlsx")
## install.packages("RJSONIO")
## install.packages("readxl")
source("jsonViewTests.R")
getwd()

mn <- read_sav("../delivery1/BCS46_Mainstage_Questionnaire_Main_NatCen_20170215.sav")
## an example of ml
m <- labelledToCtgVar(x = mn[[765]], names(mn)[765])

m@ctg@label
dir("..")
aspc <- readxl::read_excel(sp, sheet = 2)
aspc2 <- readxl::read_excel(sp, sheet = 3)
sp <- dir("../spec/", full.names = TRUE)[1]
aa <- getSpecInfo(sp)
aa$l[which(aa$l$VARNAME == names(mn)[765]),]
sml <- specToCtg(names(mn)[765], aa)
sml
setClass("bs", representation(a = "numeric", b = "numeric"))
b <- new("bs", a=2, b=3)
setClass("bss", contains = "bs", representation(x = "numeric"))
bb <- new("bss", x = 44)
## --- 2017-9-3 --> mapping vars list <-- --- ##

sp <- getSpecInfo("../spec/BCS46_DataDocumentation-Substantive_Interim_NatCen_03_20170215.xlsx")

str(sp)
mods <- unique(sp$v$Module)
writeLines(mods, "modules.txt")
sp$v$VARNAME[1:10]
vv <- sp$v$VARNAME[which(sp$v$Module == mods[11] & sp$v$LOOP == 0 & sp$v$CODED == 0 & sp$v$DELIVER_MAIN == 1)]
vv_loops <- sp$v$VARNAME[which(sp$v$Module == mods[11] & sp$v$LOOP == 1)]
vv_coded <- sp$v$VARNAME[which(sp$v$Module == mods[11] & sp$v$CODED == 1)]
length(vv)
vv[1:80]
vv <- collapseToMulti("KHLPRB", vv)
vv <- collapseToMulti("BACKPRB", vv)
vv <- collapseToMulti("CANCTYPE", vv)
vv <- collapseToMulti("HEARPRB", vv)
vv <- collapseToMulti("SPEECHPRB", vv)
vv <- collapseToMulti("EYEPRBT", vv)
vv <- collapseToMulti("HEARTPRB", vv)
vv <- collapseToMulti("SKINCOND", vv)
vv <- collapseToMulti("SBGBPROB", vv)
vv <- collapseToMulti("BKPROB", vv)
vv <- collapseToMulti("HEANI", vv)
vv <- collapseToMulti("DRINKS", vv)
vv <- collapseToMulti("WINEGS", vv)
vv



joinLists <- function(x) {
    qn <- out[[x]]
    nm <- qn$vName
    print(nm)
    m <- s <- list()
    if(qn$multi == TRUE) {
        aa <- names(mn)[grep(paste0(nm, "\\d+$"), names(mn))]
        m <- labelledToMulti(aa, mn)
        ## TODO spectTOMulti()
        s <- specToCtg(aa[1], sp)
    } else if(nm %in% names(mn)){
        m <- labelledToCtgVar(mn[[nm]], nm)
        s <- specToCtg(nm, sp)
    } else {
        print("VARIABLE NOT THERE!")
    }
    list(q = qn, m = m, s = s)
}

