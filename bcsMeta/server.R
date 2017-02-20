#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## get some json to play with
mp <- fromJSON(file = "n:/bcs70/sweep10/json/main_mapped.json")
spc <- fromJSON(file = "n:/bcs70/sweep10/json/spec.json")
changeModule <- function(mod) {
    a <- spc$modules[[mod]]$vars
    md <- modToShiny(names(a), mp)
    save(md, a, file = "md.Rda")
}

loadModule <- function() {
    load("md.Rda")
    print(names(a))
    list(md = md, a = a)
}


source("n:/bcs70/sweep10/metaget/getMetaFromFile.R")
#md <- modToShiny(names(a), mp)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    y <- loadModule()
    output$tree <- renderTree({y$md$tree})
    updateSelectizeInput(session, "mods", choices = names(spc$modules))
    output$tx <- renderText({
        print(input$mods)
        if(input$mods %in% names(spc$modules)) {
            changeModule(input$mods)
            print("here?")
            y <- loadModule()
            output$tree <- renderTree({y$md$tree})
            input$mods
        } else {
            "None selected"
        }

    })

    output$tst <- renderText({
        tre <- input$tree
        if(is.null(tre)) {
            "None"
        } else {
            yy <- get_selected(tre)
            a <- unlist(get_selected(tre))
            lbl <- unlist(md$dat[[a]]$labels)[1]
            #output$lbl <- renderText({lbl})
            #output$vrbNm <- renderText({a})
            #output$vrbLbl <- renderText({lbl})
            #output$vrbType <- renderText({md$dat[[a]]$type})
            #output$ctg <- renderDataTable({md$dat[[a]]$ctg})
            a
        }
    })
})
