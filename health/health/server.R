#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

getLabel <- function(x) {
    if(class(x$m) == "ctgVar") {
        x$m@varLabel
    } else if(class(x$m) == "multiVarList") {
        "No variable label - multi options"
    } else {
        "Unknown data type"
    }
}

getFqTable <- function(x) {
    if(class(x$m) == "ctgVar") {
        ct <- x$m@ctg
        a <- data.frame(code = ct@code, label = ct@label, missing = ct@missing,
                        freq = ct@freq)
        a %>% arrange(desc(code))
    } else if(class(x$m) == "multiVarList") {
        x$m@opt 
    } else {
        data.frame()
    }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
    kk <- 1:length(ko)
    names(kk) <- ko
    output$j <- renderText({
        ix <- kk[input$variables]
        output$vname <- renderText({getLabel(j[[ix]])})
        output$qstn <- renderText({j[[ix]]$q$qn})
        output$optCtg <- renderDataTable({getFqTable(j[[ix]])})
        output$prompts <- renderText({j[[ix]]$q$prompts})
        output$scope <- renderText({j[[ix]]$q$scope})
        paste0(input$variables, " (", ix, ")")
        })

})
