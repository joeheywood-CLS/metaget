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
}

loadModule <- function() {
    load("md.Rda")
    list(md = md, a = a)
}


source("../getMetaFromFile.R")
#md <- modToShiny(names(a), mp)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    y <- loadModule()
    output$tree <- renderTree({y$md$tree})
    updateSelectizeInput(session, "mods", choices = names(spc$modules))
    output$tx <- renderText({
        if(input$mods %in% names(spc$modules)) {
            changeModule(input$mods)
            y <- loadModule()
            output$tree <- renderTree({y$md$tree})
            input$mods
        } else {
            "None selected"
        }

    })

    output$tst <- renderText({
        print(".. [getting selection] ..")
        tre <- input$tree
        if(is.null(tre)) {
            "None"
        } else {
                #a <- unlist(get_selected(tre))
                #lbl <- unlist(md$dat[[a]]$labels)[1]
                #output$lbl <- renderText({lbl})
                #output$vrbNm <- renderText({a})
                #output$vrbLbl <- renderText({lbl})
                #output$vrbType <- renderText({md$dat[[a]]$type})
                #output$ctg <- renderDataTable({md$dat[[a]]$ctg})
                unlist(get_selected(tre))
        }
    })
})
