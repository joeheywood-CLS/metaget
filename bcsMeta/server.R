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
            dd <- data.frame(qns = names(y$md$dat), stringsAsFactors = FALSE)
            output$qnTab <- renderDataTable(dd )
            "Module selected"
        } else {
            "None selected"
        }
    })

    output$lbl <- renderText({input$qnTab_rows_selected})

    output$tst <- renderText({
        y <- loadModule()
        aa <- as.numeric(input$qnTab_rows_selected[1]) ##input$qns
        a <- names(y$md$dat)[aa]
        save(aa, a, y, file = "debug.Rda")
        lbl <- unlist(y$md$dat[[a]]$labels)[1]
        ##output$lbl <- renderText({lbl})
        output$vrbNm <- renderText({a})
        output$vrbLbl <- renderText({lbl})
        output$vrbType <- renderText({y$md$dat[[a]]$type})
        output$ctg <- renderDataTable({y$md$dat[[a]]$ctg})
        a
    })
})
