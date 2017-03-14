#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
load("N:/bcs70/sweep10/metaget/forShiny.Rda")

shinyUI(navbarPage(
    "BCS70 Sweep 10 Metadata",
    tabPanel("Module Search",
             h3("Health"),
             selectizeInput("variables", choices = ko, label = "Sections"),
             fluidRow(
                 column(3,
                        div(
                            h3("sideBar"),
                            p(textOutput("j")),
                            p(textOutput("prompts")),
                            p(textOutput("scope"))
                            

                        )
                 ),
                 column(7,
                        div(
                            h2(textOutput("vname")),
                            h4(textOutput("varLabel")),
                            p(textOutput("qstn")),
                            dataTableOutput("optCtg")
                        )
                 )
             )
    )
))