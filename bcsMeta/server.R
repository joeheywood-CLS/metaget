#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTree)
## get some json to play with
mp <- fromJSON(file = "../../json/main_mapped.json")
spc <- fromJSON(file = "../../json/spec.json")
changeModule <- function(mod) {
    print("-------- changing module ----------")
    a <- spc$modules[[mod]]$vars
    md <- modToShiny(names(a), mp)
    save(md, a, file = "md.Rda")
    print("-----------------------------------")
    list(md = md, a = a)
}

loadModule <- function() {
    load("md.Rda")
    list(md = md, a = a)
}
y <- loadModule()

source("../getMetaFromFile.R")
#md <- modToShiny(names(a), mp)

shinyServer(function(input, output, session) {

    updateSelectizeInput(session, "mods", choices = names(spc$modules))
    output$tx <- renderText({
        if(input$mods %in% names(spc$modules)) {
            y <- changeModule(input$mods)
            updateSelectizeInput(session, "qns", choices = names(y$md$dat))
            ##dd <- data.frame(qns = names(y$md$dat), stringsAsFactors = FALSE)
            ##output$qnTab <- renderDataTable(dd )
            updateRadioButtons(session, "qnRad", choices = names(y$md$dat), selected = 1)
            "Module selected"
        } else {
            "None selected"
        }
    })

    output$lbl <- renderText({input$qnTab_rows_selected})

    output$tst <- renderText({
        y <- loadModule()
        print(input$qnRad)
        aa <- as.numeric(input$qnRad) ##input$qns
        a <- input$qnRad ##names(y$md$dat)[aa]
        save(aa, a, y, file = "debug.Rda")
        lbl <- unlist(y$md$dat[[a]]$labels)[1]
        ##output$lbl <- renderText({lbl})
        output$vrbNm <- renderText({a})
        output$vrbLbl <- renderText({lbl})
        output$vrbType <- renderText({y$md$dat[[a]]$type})
        ctg <- y$md$dat[[a]]$ctg
        if(y$md$dat[[a]]$type == "loop") {
            mss <- data.frame(mss=ctg$NotApplicable[1])
            ctg <- ctg[, which(colnames(ctg) != "NotApplicable")] #
        } else {
            mss <- ctg[which(as.numeric(ctg$code) < 0),]
            ctg <- ctg[which(as.numeric(ctg$code) >= 0),]
        }
        output$ctg <- renderDataTable(ctg, options = list(paging = FALSE, searching = FALSE))
        output$mss <- renderDataTable(mss, options = list(paging = FALSE, searching = FALSE))
        a
    })
})
