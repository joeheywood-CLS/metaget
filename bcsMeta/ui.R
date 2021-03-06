#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTree)
library(DT)
library(rjson)



# Define UI for application that draws a histogram
shinyUI(navbarPage(
    "BCS70 Sweep 10 Metadata",
    tabPanel("Module Search",
             h3("Search Modules"),
             fluidRow(
                 column(3,
                        div(
                            h3("sideBar"),
                            selectizeInput("mods", choices = NULL, label = "Modules"),
                            ## shinyTree("tree"), ## got rid of this. didn't work
                            selectizeInput("qns", choices = NULL, label = "Questions"),
                            p(textOutput("tst")),
                            p(textOutput("lbl")),
                            p(textOutput("tx")),
                            radioButtons("qnRad", "lbl", 1:3, selected = 1)#dataTableOutput("qnTab")
                        )
                        ),
                 column(4,
                        div(
                            h2("mainPage"),
                            h4(textOutput("vrbNm")),
                            h5(textOutput("vrbLbl")),
                            p(textOutput("vrbType")),
                            dataTableOutput("ctg")
                        )
                        ),
                 column(3,
                        div(
                            dataTableOutput("mss")
                        )
                        )
                 )
             )
))
