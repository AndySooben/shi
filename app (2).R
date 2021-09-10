library(shiny)
library(shinymanager)
library(shinydashboard)
library(png)
library(htmltools)
library(htmltidy)
library(htmlwidgets)
library(readxl)
library(ggplot2)


credentials <- data.frame(
    user = c("1", "leo"),
    password = c("1", "thelem"),
    # comment = c("alsace", "auvergne", "bretagne"), %>% 
    stringsAsFactors = FALSE
)


adminUsers <- c("leo")

data <- read_excel("C:/Users/soobe/OneDrive/Documents/appstage/newtable.xlsx")


ui<-secure_app(dashboardPage(skin="red",
                             
                             dashboardHeader(title = tags$a(href='https://www.thelem-assurances.fr/',
                                                            tags$img(src="thelem.png", width = 200))),
                             
                             dashboardSidebar(
                                 radioButtons(inputId = 'nbcontrats',label="Nombre de contrats d'assurance que possède le foyer",
                                              choiceNames=list(("1"),("2"),("3"),("4"),("5"),("6"),("7")),
                                              choiceValues=list("c1","c2","c3","c4","c5","c6","c7"),selected="c1"),
                                 tags$a(href='http://www.leo-univ-orleans.fr/fr/',
                                        tags$img(src='labo.jpg',width=200))
                                 
                             ),
                             
                             
                             dashboardBody(
                                 # foyers avec 1 contrat
                                 conditionalPanel("input.nbcontrats=='c1'",
                                                  tags$p("Le foyer possède 1 contrat",style="font-weight: bold; font-size: 30px"),br(),
                                                  # box pour renseigner produit
                                                  box(status="warning",title="Veuillez renseigner le produit dont il dispose",
                                                      fluidRow(column(6,selectInput("assurance", "Assurance",
                                                                                    c("Auto" = "a",
                                                                                      "Habitation" = "h",
                                                                                      "Santé"="s",
                                                                                      "Accidents de la vie"="g",
                                                                                      "Protection juridique"="pj"
                                                                                    ))),
                                                               
                                                               column(6,conditionalPanel("input.assurance=='a'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance=='h'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance=='s'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance=='g'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance=='pj'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               
                                                               column(6,radioButtons(inputId = 'gamme',label="Choix de la gamme",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_1","_2","_3"),selected="_1")),
                                                      )
                                                  ),
                                                  
                                                  #box pour renseigner produit recherché
                                                  box(status="warning",title = "Veuillez choisir le produit recherché",
                                                      fluidRow(column(6,selectInput("assurance_p", "Produit recherché",
                                                                                    c("Auto" = "a",
                                                                                      "Habitation" = "h",
                                                                                      "Santé"="s",
                                                                                      "Accidents de la vie"="g",
                                                                                      "Protection juridique"="pj"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance_p=='a'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance_p=='h'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance_p=='s'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance_p=='g'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance_p=='pj'",tags$img(src="pj.png",height="20%", width="20%"))))),
                                                  # box résultats
                                                  box(status="warning",title = "Résultats",
                                                      verbatimTextOutput("p1"))
                                                  
                                 ), 
                                 
                                 # foyers avec 2 contrats
                                 
                                 conditionalPanel("input.nbcontrats=='c2'",
                                                  tags$p("Le foyer possède 2 contrats",style="font-weight: bold; font-size: 30px"),br(),
                                                  # box pour renseigner produit
                                                  box(status="warning",title="Veuillez renseigner les produits dont il dispose",
                                                      fluidRow(column(6,selectInput("assurance2", "1ère Assurance",
                                                                                    c("Auto" = "a2",
                                                                                      "Habitation" = "h2",
                                                                                      "Santé"="s2",
                                                                                      "Accidents de la vie"="g2",
                                                                                      "Protection juridique"="pj2"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance2=='a2'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance2=='h2'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance2=='s2'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance2=='g2'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance2=='pj2'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               column(6,radioButtons(inputId = 'gamme2',label="Choix de la gamme pour la 1ère assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_11","_22","_33"),selected="_11"))
                                                      ),
                                                      
                                                      fluidRow(column(6,selectInput("assurance22", "2e Assurance",
                                                                                    c("Auto" = "a2",
                                                                                      "Habitation" = "h2",
                                                                                      "Santé"="s2",
                                                                                      "Accidents de la vie"="g2",
                                                                                      "Protection juridique"="pj2"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance22=='a2'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance22=='h2'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance22=='s2'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance22=='g2'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance22=='pj2'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               column(6,radioButtons(inputId = 'gamme22',label="Choix de la gamme pour la 2e assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_11","_22","_33"),selected="_11"))
                                                      )),br(),
                                                  
                                                  # box pour choisir le produit recherché
                                                  box(status="warning",title="Veuillez choisir le produit recherché",
                                                      fluidRow(column(6,selectInput("assurance_p2", "Produit recherché",
                                                                                    c("Auto" = "a2",
                                                                                      "Habitation" = "h2",
                                                                                      "Santé"="s2",
                                                                                      "Accidents de la vie"="g2",
                                                                                      "Protection juridique"="pj2"
                                                                                    )))
                                                               ,column(6,conditionalPanel("input.assurance_p2=='a2'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                       conditionalPanel("input.assurance_p2=='h2'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                       conditionalPanel("input.assurance_p2=='s2'",tags$img(src="san.png",height="20%", width="20%")),
                                                                       conditionalPanel("input.assurance_p2=='g2'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                       conditionalPanel("input.assurance_p2=='pj2'",tags$img(src="pj.png",height="20%", width="20%"))))),
                                                  box(status="warning",title="Résultats",verbatimTextOutput("p2"))
                                                  
                                 ),
                                 
                                 
                                 
                                 # foyers avec 3 contrats
                                 
                                 conditionalPanel("input.nbcontrats=='c3'",
                                                  tags$p("Le foyer possède 3 contrats",style="font-weight: bold; font-size: 30px"),br(),
                                                  # box produits 
                                                  box(status="warning",title="Veuillez renseigner les produits dont il dispose",
                                                      fluidRow(column(6,selectInput("assurance3", "1ère Assurance",
                                                                                    c("Auto" = "a3",
                                                                                      "Habitation" = "h3",
                                                                                      "Santé"="s3",
                                                                                      "Accidents de la vie"="g3",
                                                                                      "Protection juridique"="pj3"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance3=='a3'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance3=='h3'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance3=='s3'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance3=='g3'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance3=='pj3'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               column(6,radioButtons(inputId = 'gamme3',label="Choix de la gamme pour la 1ère assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_111","_222","_333"),selected="_111"))
                                                      ),
                                                      
                                                      fluidRow(column(6,selectInput("assurance33", "2e Assurance",
                                                                                    c("Auto" = "a3",
                                                                                      "Habitation" = "h3",
                                                                                      "Santé"="s3",
                                                                                      "Accidents de la vie"="g3",
                                                                                      "Protection juridique"="pj3"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance33=='a3'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance33=='h3'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance33=='s3'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance33=='g3'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance33=='pj3'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               column(6,radioButtons(inputId = 'gamme33',label="Choix de la gamme pour la 2e assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_111","_222","_333"),selected="_111"))
                                                      ),
                                                      
                                                      fluidRow(column(6,selectInput("assurance333", "3e Assurance",
                                                                                    c("Auto" = "a3",
                                                                                      "Habitation" = "h3",
                                                                                      "Santé"="s3",
                                                                                      "Accidents de la vie"="g3",
                                                                                      "Protection juridique"="pj3"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance333=='a3'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance333=='h3'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance333=='s3'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance333=='g3'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance333=='pj3'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               column(6,radioButtons(inputId = 'gamme333',label="Choix de la gamme pour la 3e assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_111","_222","_333"),selected="_111"))
                                                      )),
                                                  
                                                  #box produits recherches
                                                  box(status="warning",title="Veuillez choisir le produit recherché",fluidRow(column(6,
                                                                                                                                      selectInput("assurance_p3", "Produit recherché",
                                                                                                                                                  c("Auto" = "a3",
                                                                                                                                                    "Habitation" = "h3",
                                                                                                                                                    "Santé"="s3",
                                                                                                                                                    "Accidents de la vie"="g3",
                                                                                                                                                    "Protection juridique"="pj3"
                                                                                                                                                  ))),
                                                                                                                               column(6,conditionalPanel("input.assurance_p3=='a3'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p3=='h3'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p3=='s3'",tags$img(src="san.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p3=='g3'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p3=='pj3'",tags$img(src="pj.png",height="20%", width="20%"))))),
                                                  # box resultats
                                                  box(status="warning",title="Résultats",verbatimTextOutput("p3"))
                                                  
                                                  
                                 ),
                                 
                                 
                                 # foyers avec 4 contrats
                                 
                                 conditionalPanel("input.nbcontrats=='c4'",
                                                  tags$p("Le foyer possède 4 contrats",style="font-weight: bold; font-size: 30px"),br(),
                                                  # box produits
                                                  box(status="warning",title="Veuillez renseigner les produits dont il dispose",
                                                      fluidRow(column(6,selectInput("assurance4", "1ère Assurance",
                                                                                    c("Auto" = "a4",
                                                                                      "Habitation" = "h4",
                                                                                      "Santé"="s4",
                                                                                      "Accidents de la vie"="g4",
                                                                                      "Protection juridique"="pj4"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance4=='a4'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance4=='h4'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance4=='s4'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance4=='g4'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance4=='pj4'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               
                                                               column(6,radioButtons(inputId = 'gamme4',label="Choix de la gamme pour la 1ère assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_1111","_2222","_3333"),selected="_1111"))
                                                      ),
                                                      
                                                      fluidRow(column(6,selectInput("assurance44", "2e Assurance",
                                                                                    c("Auto" = "a4",
                                                                                      "Habitation" = "h4",
                                                                                      "Santé"="s4",
                                                                                      "Accidents de la vie"="g4",
                                                                                      "Protection juridique"="pj4"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance44=='a4'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance44=='h4'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance44=='s4'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance44=='g4'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance44=='pj4'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               
                                                               column(6,radioButtons(inputId = 'gamme44',label="Choix de la gamme pour la 2e assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_1111","_2222","_3333"),selected="_1111"))
                                                      ),
                                                      
                                                      fluidRow(column(6,selectInput("assurance444", "3e Assurance",
                                                                                    c("Auto" = "a4",
                                                                                      "Habitation" = "h4",
                                                                                      "Santé"="s4",
                                                                                      "Accidents de la vie"="g4",
                                                                                      "Protection juridique"="pj4"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance444=='a4'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance444=='h4'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance444=='s4'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance444=='g4'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance444=='pj4'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               
                                                               column(6,radioButtons(inputId = 'gamme444',label="Choix de la gamme pour la 3e assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_1111","_2222","_3343"),selected="_1111"))
                                                      ),
                                                      
                                                      fluidRow(column(6,selectInput("assurance4444", "4e Assurance",
                                                                                    c("Auto" = "a4",
                                                                                      "Habitation" = "h4",
                                                                                      "Santé"="s4",
                                                                                      "Accidents de la vie"="g4",
                                                                                      "Protection juridique"="pj4"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance4444=='a4'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance4444=='h4'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance4444=='s4'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance4444=='g4'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance4444=='pj4'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               column(6,radioButtons(inputId = 'gamme4444',label="Choix de la gamme pour la 4e assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_1111","_2222","_3343"),selected="_1111"))
                                                      )),
                                                  
                                                  # box produits recherches
                                                  box(status="warning",title="Veuillez choisir le produit recherché",fluidRow(column(6,
                                                                                                                                      selectInput("assurance_p4", "Produit recherché",
                                                                                                                                                  c("Auto" = "a4",
                                                                                                                                                    "Habitation" = "h4",
                                                                                                                                                    "Santé"="s4",
                                                                                                                                                    "Accidents de la vie"="g4",
                                                                                                                                                    "Protection juridique"="pj4"
                                                                                                                                                  ))),
                                                                                                                               column(6,conditionalPanel("input.assurance_p4=='a4'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p4=='h4'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p4=='s4'",tags$img(src="san.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p4=='g4'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p4=='pj4'",tags$img(src="pj.png",height="20%", width="20%"))))),
                                                  
                                                  # box resultats
                                                  box(status="warning",title="Résultats",verbatimTextOutput("p4"))
                                                  
                                 ),
                                 
                                 # foyers avec 5 contrats
                                 
                                 conditionalPanel("input.nbcontrats=='c5'",
                                                  tags$p("Le foyer possède 5 contrats",style="font-weight: bold; font-size: 30px"),br(),
                                                  # box produits 1
                                                  box(status="warning",title="Veuillez renseigner les contrats dont il dispose",
                                                      fluidRow(column(6,selectInput("assurance5", "1ère Assurance",
                                                                                    c("Auto" = "a5",
                                                                                      "Habitation" = "h5",
                                                                                      "Santé"="s5",
                                                                                      "Accidents de la vie"="g5",
                                                                                      "Protection juridique"="pj5"
                                                                                    ))),
                                                               column(6,conditionalPanel("input.assurance5=='a5'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance5=='h5'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance5=='s5'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance5=='g5'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance5=='pj5'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               
                                                               column(6,radioButtons(inputId = 'gamme5',label="Choix de la gamme pour la 1ère assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_11111","_22222","_33333"),selected="_11111"))
                                                      ),
                                                      
                                                      fluidRow(column(6,selectInput("assurance55", "2e Assurance",
                                                                                    c("Auto" = "a5",
                                                                                      "Habitation" = "h5",
                                                                                      "Santé"="s5",
                                                                                      "Accidents de la vie"="g5",
                                                                                      "Protection juridique"="pj5"
                                                                                    ))),
                                                               
                                                               column(6,conditionalPanel("input.assurance55=='a5'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance55=='h5'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance55=='s5'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance55=='g5'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance55=='pj5'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               
                                                               column(6,radioButtons(inputId = 'gamme55',label="Choix de la gamme pour la 2e assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_11111","_22222","_33333"),selected="_11111"))
                                                      ),
                                                      
                                                      fluidRow(column(6,selectInput("assurance555", "3e Assurance",
                                                                                    c("Auto" = "a5",
                                                                                      "Habitation" = "h5",
                                                                                      "Santé"="s5",
                                                                                      "Accidents de la vie"="g5",
                                                                                      "Protection juridique"="pj5"
                                                                                    ))),
                                                               
                                                               column(6,conditionalPanel("input.assurance555=='a5'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance555=='h5'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance555=='s5'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance555=='g5'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance555=='pj5'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               
                                                               column(6,radioButtons(inputId = 'gamme555',label="Choix de la gamme pour la 3e assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_11111","_22222","_33333"),selected="_11111"))),
                                                      
                                                      fluidRow(column(6,selectInput("assurance5555", "4e Assurance",
                                                                                    c("Auto" = "a5",
                                                                                      "Habitation" = "h5",
                                                                                      "Santé"="s5",
                                                                                      "Accidents de la vie"="g5",
                                                                                      "Protection juridique"="pj5"
                                                                                    ))),
                                                               
                                                               column(6,conditionalPanel("input.assurance5555=='a5'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance5555=='h5'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance5555=='s5'",tags$img(src="san.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance5555=='g5'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                      conditionalPanel("input.assurance5555=='pj5'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                               
                                                               column(6,radioButtons(inputId = 'gamme5555',label="Choix de la gamme pour la 4e assurance",
                                                                                     choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                     choiceValues=list("_11111","_22222","_33333"),selected="_11111")))),
                                                  
                                                  # box produit 2
                                                  box(status="warning",fluidRow(column(6,selectInput("assurance55555", "5e Assurance",
                                                                                                     c("Auto" = "a5",
                                                                                                       "Habitation" = "h5",
                                                                                                       "Santé"="s5",
                                                                                                       "Accidents de la vie"="g5",
                                                                                                       "Protection juridique"="pj5"
                                                                                                     ))),
                                                                                column(6,conditionalPanel("input.assurance55555=='a5'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                                       conditionalPanel("input.assurance55555=='h5'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                                       conditionalPanel("input.assurance55555=='s5'",tags$img(src="san.png",height="20%", width="20%")),
                                                                                       conditionalPanel("input.assurance55555=='g5'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                                       conditionalPanel("input.assurance55555=='pj5'",tags$img(src="pj.png",height="20%", width="20%"))),
                                                                                
                                                                                column(6,radioButtons(inputId = 'gamme55555',label="Choix de la gamme pour la 4e assurance",
                                                                                                      choiceNames=list(("Bas de gamme"),("Milieu de gamme"),("Haut de gamme")),
                                                                                                      choiceValues=list("_11111","_22222","_33333"),selected="_11111")))),
                                                  
                                                  # box produits recherches
                                                  box(status="warning",title="Veuillez choisir le produit recherché",fluidRow(column(6,
                                                                                                                                      selectInput("assurance_p5", "Produit recherché",
                                                                                                                                                  c("Auto" = "a5",
                                                                                                                                                    "Habitation" = "h5",
                                                                                                                                                    "Santé"="s5",
                                                                                                                                                    "Accidents de la vie"="g5",
                                                                                                                                                    "Protection juridique"="pj5"
                                                                                                                                                  ))),
                                                                                                                               column(6,conditionalPanel("input.assurance_p5=='a5'",tags$img(src="auto.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p5=='h5'",tags$img(src="hab.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p5=='s5'",tags$img(src="san.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p5=='g5'",tags$img(src="gav.png",height="20%", width="20%")),
                                                                                                                                      conditionalPanel("input.assurance_p5=='pj5'",tags$img(src="pj.png",height="20%", width="20%"))))),
                                                  # box titre
                                                  box(status="warning",title="Résultats",verbatimTextOutput("p5"))),
                                 
                                 
                                 
                                 
                                 
                             ),
                             
                             box(plotOutput("distPlot"))
                             
                             
                             
)
)

# Define server logic required to draw a histogram
server<-shinyServer(function(input, output) {
    
    #déf des fonctions pour afficher probas
    
    p1=function(var,proba){
        effectif1=nrow(data[data[,var]==1 & data[,proba[1]]==1,])
        effectif2=nrow(data[data[,var]==1 & data[,proba[2]]==1,])
        effectif3=nrow(data[data[,var]==1 & data[,proba[3]]==1,])
        tot=nrow(data[data[,var]==1,])
        result1=effectif1/tot
        result2=effectif2/tot
        result3=effectif3/tot
        cat("Probabilité de souscription bas de gamme",result1,'\n')
        cat("Probabilité de souscription milieu de gamme",result2,'\n')
        cat("Probabilité de souscription haut de gamme",result3,'\n')
        
    }
    
    pj1=function(var,proba){
        effectif1=nrow(data[data[,var]==1 & data[,proba]==1,])
        tot=nrow(data[data[,var]==1,])
        result1=effectif1/tot
        cat("Probabilité de souscription",result1,'\n')
        
    }
    
    p2=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba[1]]==1 & data[,var[2]]==1,])
        effectif2=nrow(data[data[,var[1]]==1 & data[,proba[2]]==1 & data[,var[2]]==1,])
        effectif3=nrow(data[data[,var[1]]==1 & data[,proba[3]]==1 & data[,var[2]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1,])
        result1=effectif1/tot
        result2=effectif2/tot
        result3=effectif3/tot
        cat("Probabilité de souscription bas de gamme",result1,'\n')
        cat("Probabilité de souscription milieu de gamme",result2,'\n')
        cat("Probabilité de souscription haut de gamme",result3,'\n')
    }
    
    pj2=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba]==1 & data[,var[2]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1,])
        result1=effectif1/tot
        cat("Probabilité de souscription",result1,'\n')
    }
    
    p3=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        effectif2=nrow(data[data[,var[1]]==1 & data[,proba[2]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        effectif3=nrow(data[data[,var[1]]==1 & data[,proba[3]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        result1=effectif1/tot
        result2=effectif2/tot
        result3=effectif3/tot
        cat("Probabilité de souscription bas de gamme",result1,'\n')
        cat("Probabilité de souscription milieu de gamme",result2,'\n')
        cat("Probabilité de souscription haut de gamme",result3,'\n')
    }
    
    pj3=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        result1=effectif1/tot
        cat("Probabilité de souscription",result1,'\n')
    }
    
    p4=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        effectif2=nrow(data[data[,var[1]]==1 & data[,proba[2]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        effectif3=nrow(data[data[,var[1]]==1 & data[,proba[3]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        result1=effectif1/tot
        result2=effectif2/tot
        result3=effectif3/tot
        cat("Probabilité de souscription bas de gamme",result1,'\n')
        cat("Probabilité de souscription milieu de gamme",result2,'\n')
        cat("Probabilité de souscription haut de gamme",result3,'\n')
    }
    
    pj4=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        result1=effectif1/tot
        cat("Probabilité de souscription",result1,'\n')
    }
    
    # déf des fonctions pour afficher les graphs
    
    p11=function(var,proba){
        effectif1=nrow(data[data[,var]==1 & data[,proba[1]]==1,])
        effectif2=nrow(data[data[,var]==1 & data[,proba[2]]==1,])
        effectif3=nrow(data[data[,var]==1 & data[,proba[3]]==1,])
        tot=nrow(data[data[,var]==1,])
        result1=effectif1/tot
        result2=effectif2/tot
        result3=effectif3/tot
        result=cbind(result1,result2,result3)
        return(result)
        
    }
    
    pj11=function(var,proba){
        effectif1=nrow(data[data[,var]==1 & data[,proba]==1,])
        tot=nrow(data[data[,var]==1,])
        result1=effectif1/tot
        return(result1)
        
    }
    
    
    
    p22=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba[1]]==1 & data[,var[2]]==1,])
        effectif2=nrow(data[data[,var[1]]==1 & data[,proba[2]]==1 & data[,var[2]]==1,])
        effectif3=nrow(data[data[,var[1]]==1 & data[,proba[3]]==1 & data[,var[2]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1,])
        result1=effectif1/tot
        result2=effectif2/tot
        result3=effectif3/tot
        result=cbind(result1,result2,result3)
        return(result)
    }
    
    pj22=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba]==1 & data[,var[2]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1,])
        result1=effectif1/tot
        return(result1)
    }
    
    p33=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        effectif2=nrow(data[data[,var[1]]==1 & data[,proba[2]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        effectif3=nrow(data[data[,var[1]]==1 & data[,proba[3]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        result1=effectif1/tot
        result2=effectif2/tot
        result3=effectif3/tot
        result=cbind(result1,result2,result3)
        return(result)
    }
    
    pj33=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1,])
        result1=effectif1/tot
        return(result1)
    }
    
    p44=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        effectif2=nrow(data[data[,var[1]]==1 & data[,proba[2]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        effectif3=nrow(data[data[,var[1]]==1 & data[,proba[3]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        result1=effectif1/tot
        result2=effectif2/tot
        result3=effectif3/tot
        result=cbind(result1,result2,result3)
        return(result)
    }
    
    pj44=function(var,proba){
        effectif1=nrow(data[data[,var[1]]==1 & data[,proba[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        tot=nrow(data[data[,var[1]]==1 & data[,var[2]]==1 & data[,var[3]]==1 & data[,var[4]]==1,])
        result1=effectif1/tot
        return(result1)
    }
    
    
    # fonctions pour afficher probas
    
    output$p1 <- renderPrint({
        
        
        if (input$assurance_p=="pj"){
            if (input$assurance=="a" & input$gamme=="_1"){i=3}
            if (input$assurance=="a" & input$gamme=="_2"){i=4}
            if (input$assurance=="a" & input$gamme=="_3"){i=5}
            
            if (input$assurance=="h" & input$gamme=="_1"){i=12}
            if (input$assurance=="h" & input$gamme=="_2"){i=13}
            if (input$assurance=="h" & input$gamme=="_3"){i=14}
            
            if (input$assurance=="s" & input$gamme=="_1"){i=18}
            if (input$assurance=="s" & input$gamme=="_2"){i=19}
            if (input$assurance=="s" & input$gamme=="_3"){i=20}
            
            if (input$assurance=="g" & input$gamme=="_1"){i=21}
            if (input$assurance=="g" & input$gamme=="_2"){i=22}
            if (input$assurance=="g" & input$gamme=="_3"){i=23}
            pj1(i,14)
        }
        
        else{
            
            if (input$assurance=="a" & input$gamme=="_1"){i=3}
            if (input$assurance=="a" & input$gamme=="_2"){i=4}
            if (input$assurance=="a" & input$gamme=="_3"){i=5}
            
            if (input$assurance=="h" & input$gamme=="_1"){i=12}
            if (input$assurance=="h" & input$gamme=="_2"){i=13}
            if (input$assurance=="h" & input$gamme=="_3"){i=14}
            
            if (input$assurance=="s" & input$gamme=="_1"){i=18}
            if (input$assurance=="s" & input$gamme=="_2"){i=19}
            if (input$assurance=="s" & input$gamme=="_3"){i=20}
            
            if (input$assurance=="g" & input$gamme=="_1"){i=21}
            if (input$assurance=="g" & input$gamme=="_2"){i=22}
            if (input$assurance=="g" & input$gamme=="_3"){i=23}
            
            if (input$assurance=="pj"){i=2}
            
            if (input$assurance_p=="a"){
                if (input$assurance=="a"){
                    i=6
                    j=7
                    k=8}
                
                else { 
                    
                    j=2
                    k=3
                    l=4}
                
                
            }
            
            if (input$assurance_p=="h"){
                if (input$assurance=="h") { 
                    j=15
                    k=16
                    l=17
                }
                
                else { 
                    j=12
                    k=13
                    l=14
                    
                }
                
                
                
            }
            
            if (input$assurance_p=="s"){
                j=18
                k=19
                l=20}
            
            if (input$assurance_p=="g"){
                j=21
                k=22
                l=23}
            
            p1(i,c(j,k,l))
        }
    })
    
    output$p2 <- renderPrint({
        
        
        if (input$assurance_p2=="pj2"){
            if (input$assurance2=="a2" & input$gamme2=="_11"){i1=3}
            if (input$assurance2=="a2" & input$gamme2=="_22"){i1=4}
            if (input$assurance2=="a2" & input$gamme2=="_33"){i1=5}
            
            if (input$assurance2=="h2" & input$gamme2=="_11"){i1=12}
            if (input$assurance2=="h2" & input$gamme2=="_22"){i1=13}
            if (input$assurance2=="h2" & input$gamme2=="_33"){i1=14}
            
            if (input$assurance2=="s2" & input$gamme2=="_11"){i1=18}
            if (input$assurance2=="s2" & input$gamme2=="_22"){i1=19}
            if (input$assurance2=="s2" & input$gamme2=="_33"){i1=20}
            
            if (input$assurance2=="g2" & input$gamme2=="_11"){i1=21}
            if (input$assurance2=="g2" & input$gamme2=="_22"){i1=22}
            if (input$assurance2=="g2" & input$gamme2=="_33"){i1=23}
            
            
            if (input$assurance22=="a2" & input$gamme22=="_11"){i2=3}
            if (input$assurance22=="a2" & input$gamme22=="_22"){i2=4}
            if (input$assurance22=="a2" & input$gamme22=="_33"){i2=5}
            
            if (input$assurance22=="h2" & input$gamme22=="_11"){i2=12}
            if (input$assurance22=="h2" & input$gamme22=="_22"){i2=13}
            if (input$assurance22=="h2" & input$gamme22=="_33"){i2=14}
            
            if (input$assurance22=="s2" & input$gamme22=="_11"){i2=18}
            if (input$assurance22=="s2" & input$gamme22=="_22"){i2=19}
            if (input$assurance22=="s2" & input$gamme22=="_33"){i2=20}
            
            if (input$assurance22=="g2" & input$gamme22=="_11"){i2=21}
            if (input$assurance22=="g2" & input$gamme22=="_22"){i2=22}
            if (input$assurance22=="g2" & input$gamme22=="_33"){i2=23}
            
            pj2(c(i1,i2),2)
        }
        
        else{
            
            if (input$assurance2=="a2" & input$gamme2=="_11"){i1=3}
            if (input$assurance2=="a2" & input$gamme2=="_22"){i1=4}
            if (input$assurance2=="a2" & input$gamme2=="_33"){i1=5}
            
            if (input$assurance2=="h2" & input$gamme2=="_11"){i1=12}
            if (input$assurance2=="h2" & input$gamme2=="_22"){i1=13}
            if (input$assurance2=="h2" & input$gamme2=="_33"){i1=14}
            
            if (input$assurance2=="s2" & input$gamme2=="_11"){i1=18}
            if (input$assurance2=="s2" & input$gamme2=="_22"){i1=19}
            if (input$assurance2=="s2" & input$gamme2=="_33"){i1=20}
            
            if (input$assurance2=="g2" & input$gamme2=="_11"){i1=21}
            if (input$assurance2=="g2" & input$gamme2=="_22"){i1=22}
            if (input$assurance2=="g2" & input$gamme2=="_33"){i1=23}
            
            if (input$assurance2=="pj2"){i1=2}
            
            
            if (input$assurance22=="a2" & input$gamme22=="_11"){i2=3}
            if (input$assurance22=="a2" & input$gamme22=="_22"){i2=4}
            if (input$assurance22=="a2" & input$gamme22=="_33"){i2=5}
            
            if (input$assurance22=="h2" & input$gamme22=="_11"){i2=12}
            if (input$assurance22=="h2" & input$gamme22=="_22"){i2=13}
            if (input$assurance22=="h2" & input$gamme22=="_33"){i2=14}
            
            if (input$assurance22=="s2" & input$gamme22=="_11"){i2=18}
            if (input$assurance22=="s2" & input$gamme22=="_22"){i2=19}
            if (input$assurance22=="s2" & input$gamme22=="_33"){i2=20}
            
            if (input$assurance22=="g2" & input$gamme22=="_11"){i2=21}
            if (input$assurance22=="g2" & input$gamme22=="_22"){i2=22}
            if (input$assurance22=="g2" & input$gamme22=="_33"){i2=23}
            
            if (input$assurance22=="pj2"){i2=2}
            
            if (input$assurance_p2=="a2"){
                
                if(input$assurance2=="a2" & input$assurance22=="a2"){ 
                    j=9
                    k=10
                    l=11
                    
                }
                
                else if (input$assurance2=="a2"  | input$assurance22=="a2") { 
                    j=6
                    k=7
                    l=8
                    
                    
                }
            }
            
            if (input$assurance_p2=="h2"){
                
                
                if (input$assurance2=="h2" | input$assurance22=="h2"){   
                    
                    j=15
                    k=16
                    l=17
                    
                }
                
                else{ 
                    j=12
                    k=13
                    l=14
                    
                }
            }
            
            if (input$assurance_p2=="s2"){
                j=18
                k=19
                l=20}
            
            if (input$assurance_p2=="g2"){
                j=21
                k=22
                l=23}
            
            p2(c(i1,i2),c(j,k,l))
        }
    })
    
    output$p3 <- renderPrint({
        
        
        
        if (input$assurance_p3=="pj3"){
            if (input$assurance3=="a3" & input$gamme3=="_111"){i1=3}
            if (input$assurance3=="a3" & input$gamme3=="_222"){i1=4}
            if (input$assurance3=="a3" & input$gamme3=="_333"){i1=5}
            
            if (input$assurance3=="h3" & input$gamme3=="_111"){i1=12}
            if (input$assurance3=="h3" & input$gamme3=="_222"){i1=13}
            if (input$assurance3=="h3" & input$gamme3=="_333"){i1=14}
            
            if (input$assurance3=="s3" & input$gamme3=="_111"){i1=18}
            if (input$assurance3=="s3" & input$gamme3=="_222"){i1=19}
            if (input$assurance3=="s3" & input$gamme3=="_333"){i1=20}
            
            if (input$assurance3=="g3" & input$gamme3=="_111"){i1=21}
            if (input$assurance3=="g3" & input$gamme3=="_222"){i1=22}
            if (input$assurance3=="g3" & input$gamme3=="_333"){i1=23}
            
            
            if (input$assurance33=="a3" & input$gamme33=="_111"){i2=6}
            if (input$assurance33=="a3" & input$gamme33=="_222"){i2=7}
            if (input$assurance33=="a3" & input$gamme33=="_333"){i2=8}
            
            if (input$assurance33=="h3" & input$gamme33=="_111"){i2=15}
            if (input$assurance33=="h3" & input$gamme33=="_222"){i2=16}
            if (input$assurance33=="h3" & input$gamme33=="_333"){i2=17}
            
            if (input$assurance33=="s3" & input$gamme33=="_111"){i2=18}
            if (input$assurance33=="s3" & input$gamme33=="_222"){i2=19}
            if (input$assurance33=="s3" & input$gamme33=="_333"){i2=20}
            
            if (input$assurance33=="g3" & input$gamme33=="_111"){i2=21}
            if (input$assurance33=="g3" & input$gamme33=="_222"){i2=22}
            if (input$assurance33=="g3" & input$gamme33=="_333"){i2=23}
            
            
            
            if (input$assurance333=="a3" & input$gamme333=="_111"){i3=9}
            if (input$assurance333=="a3" & input$gamme333=="_222"){i3=10}
            if (input$assurance333=="a3" & input$gamme333=="_333"){i3=11}
            
            if (input$assurance333=="h3" & input$gamme333=="_111"){i3=15}
            if (input$assurance333=="h3" & input$gamme333=="_222"){i3=16}
            if (input$assurance333=="h3" & input$gamme333=="_333"){i3=17}
            
            if (input$assurance333=="s3" & input$gamme333=="_111"){i3=18}
            if (input$assurance333=="s3" & input$gamme333=="_222"){i3=19}
            if (input$assurance333=="s3" & input$gamme333=="_333"){i3=20}
            
            if (input$assurance333=="g3" & input$gamme333=="_111"){i3=21}
            if (input$assurance333=="g3" & input$gamme333=="_222"){i3=22}
            if (input$assurance333=="g3" & input$gamme333=="_333"){i3=23}
            
            
            pj3(c(i1,i2,i3),2)
            
        }
        
        else if (input$assurance_p3!="pj3"){
            if (input$assurance3=="a3" & input$gamme3=="_111"){i1=2}
            if (input$assurance3=="a3" & input$gamme3=="_222"){i1=3}
            if (input$assurance3=="a3" & input$gamme3=="_333"){i1=4}
            
            if (input$assurance3=="h3" & input$gamme3=="_111"){i1=5}
            if (input$assurance3=="h3" & input$gamme3=="_222"){i1=6}
            if (input$assurance3=="h3" & input$gamme3=="_333"){i1=7}
            
            if (input$assurance3=="s3" & input$gamme3=="_111"){i1=8}
            if (input$assurance3=="s3" & input$gamme3=="_222"){i1=9}
            if (input$assurance3=="s3" & input$gamme3=="_333"){i1=10}
            
            if (input$assurance3=="g3" & input$gamme3=="_111"){i1=11}
            if (input$assurance3=="g3" & input$gamme3=="_222"){i1=12}
            if (input$assurance3=="g3" & input$gamme3=="_333"){i1=13}
            
            if (input$assurance3=="pj3"){i1=14}
            
            
            if (input$assurance33=="a3" & input$gamme33=="_111"){i2=2}
            if (input$assurance33=="a3" & input$gamme33=="_222"){i2=3}
            if (input$assurance33=="a3" & input$gamme33=="_333"){i2=4}
            
            if (input$assurance33=="h3" & input$gamme33=="_111"){i2=5}
            if (input$assurance33=="h3" & input$gamme33=="_222"){i2=6}
            if (input$assurance33=="h3" & input$gamme33=="_333"){i2=7}
            
            if (input$assurance33=="s3" & input$gamme33=="_111"){i2=8}
            if (input$assurance33=="s3" & input$gamme33=="_222"){i2=9}
            if (input$assurance33=="s3" & input$gamme33=="_333"){i2=10}
            
            if (input$assurance33=="g3" & input$gamme33=="_111"){i2=11}
            if (input$assurance33=="g3" & input$gamme33=="_222"){i2=12}
            if (input$assurance33=="g3" & input$gamme33=="_333"){i2=13}
            
            if (input$assurance33=="pj3"){i2=14}
            
            
            if (input$assurance333=="a3" & input$gamme333=="_111"){i3=2}
            if (input$assurance333=="a3" & input$gamme333=="_222"){i3=3}
            if (input$assurance333=="a3" & input$gamme333=="_333"){i3=4}
            
            if (input$assurance333=="h3" & input$gamme333=="_111"){i3=5}
            if (input$assurance333=="h3" & input$gamme333=="_222"){i3=6}
            if (input$assurance333=="h3" & input$gamme333=="_333"){i3=7}
            
            if (input$assurance333=="s3" & input$gamme333=="_111"){i3=8}
            if (input$assurance333=="s3" & input$gamme333=="_222"){i3=9}
            if (input$assurance333=="s3" & input$gamme333=="_333"){i3=10}
            
            if (input$assurance333=="g3" & input$gamme333=="_111"){i3=11}
            if (input$assurance333=="g3" & input$gamme333=="_222"){i3=12}
            if (input$assurance333=="g3" & input$gamme333=="_333"){i3=13}
            
            if (input$assurance333=="pj3"){i3=14}
            
            
            
            if (input$assurance_p3=="a3"){
                j=2
                k=3
                l=4}
            
            if (input$assurance_p3=="h3"){
                j=5
                k=6
                l=7}
            
            if (input$assurance_p3=="s3"){
                j=8
                k=9
                l=10}
            
            if (input$assurance_p3=="g3"){
                j=11
                k=12
                l=13}
            
            p3(c(i1,i2,i3),c(j,k,l))
        }
        
        
    })
    
    output$p4 <- renderPrint({
        if (input$assurance4==input$assurance44 | input$assurance4==input$assurance444 | input$assurance4==input$assurance4444 | input$assurance44==input$assurance444 | input$assurance44==input$assurance4444 | input$assurance444==input$assurance4444){
            cat("Veuillez choisir des produits différents")
        }
        else if(input$assurance4==input$assurance_p4 | input$assurance44==input$assurance_p4 | input$assurance444==input$assurance_p4 | input$assurance4444==input$assurance_p4){
            cat("Veuillez choisir un produit d'assurance différent de celui qu'il possède")
        }
        else{
            if (input$assurance_p4=="pj4"){
                if (input$assurance4=="a4" & input$gamme4=="_1111"){i1=2}
                if (input$assurance4=="a4" & input$gamme4=="_2222"){i1=3}
                if (input$assurance4=="a4" & input$gamme4=="_3333"){i1=4}
                
                if (input$assurance4=="h4" & input$gamme4=="_1111"){i1=5}
                if (input$assurance4=="h4" & input$gamme4=="_2222"){i1=6}
                if (input$assurance4=="h4" & input$gamme4=="_3333"){i1=7}
                
                if (input$assurance4=="s4" & input$gamme4=="_1111"){i1=8}
                if (input$assurance4=="s4" & input$gamme4=="_2222"){i1=9}
                if (input$assurance4=="s4" & input$gamme4=="_3333"){i1=10}
                
                if (input$assurance4=="g4" & input$gamme4=="_1111"){i1=11}
                if (input$assurance4=="g4" & input$gamme4=="_2222"){i1=12}
                if (input$assurance4=="g4" & input$gamme4=="_3333"){i1=13}
                
                
                if (input$assurance44=="a4" & input$gamme44=="_1111"){i2=2}
                if (input$assurance44=="a4" & input$gamme44=="_2222"){i2=3}
                if (input$assurance44=="a4" & input$gamme44=="_3333"){i2=4}
                
                if (input$assurance44=="h4" & input$gamme44=="_1111"){i2=5}
                if (input$assurance44=="h4" & input$gamme44=="_2222"){i2=6}
                if (input$assurance44=="h4" & input$gamme44=="_3333"){i2=7}
                
                if (input$assurance44=="s4" & input$gamme44=="_1111"){i2=8}
                if (input$assurance44=="s4" & input$gamme44=="_2222"){i2=9}
                if (input$assurance44=="s4" & input$gamme44=="_3333"){i2=10}
                
                if (input$assurance44=="g4" & input$gamme44=="_1111"){i2=11}
                if (input$assurance44=="g4" & input$gamme44=="_2222"){i2=12}
                if (input$assurance44=="g4" & input$gamme44=="_3333"){i2=13}
                
                
                
                if (input$assurance444=="a4" & input$gamme444=="_1111"){i3=2}
                if (input$assurance444=="a4" & input$gamme444=="_2222"){i3=3}
                if (input$assurance444=="a4" & input$gamme444=="_3333"){i3=4}
                
                if (input$assurance444=="h4" & input$gamme444=="_1111"){i3=5}
                if (input$assurance444=="h4" & input$gamme444=="_2222"){i3=6}
                if (input$assurance444=="h4" & input$gamme444=="_3333"){i3=7}
                
                if (input$assurance444=="s4" & input$gamme444=="_1111"){i3=8}
                if (input$assurance444=="s4" & input$gamme444=="_2222"){i3=9}
                if (input$assurance444=="s4" & input$gamme444=="_3333"){i3=10}
                
                if (input$assurance444=="g4" & input$gamme444=="_1111"){i3=11}
                if (input$assurance444=="g4" & input$gamme444=="_2222"){i3=12}
                if (input$assurance444=="g4" & input$gamme444=="_3333"){i3=13}
                
                
                
                if (input$assurance4444=="a4" & input$gamme4444=="_1111"){i4=2}
                if (input$assurance4444=="a4" & input$gamme4444=="_2222"){i4=3}
                if (input$assurance4444=="a4" & input$gamme4444=="_3333"){i4=4}
                
                if (input$assurance4444=="h4" & input$gamme4444=="_1111"){i4=5}
                if (input$assurance4444=="h4" & input$gamme4444=="_2222"){i4=6}
                if (input$assurance4444=="h4" & input$gamme4444=="_3333"){i4=7}
                
                if (input$assurance4444=="s4" & input$gamme4444=="_1111"){i4=8}
                if (input$assurance4444=="s4" & input$gamme4444=="_2222"){i4=9}
                if (input$assurance4444=="s4" & input$gamme4444=="_3333"){i4=10}
                
                if (input$assurance4444=="g4" & input$gamme4444=="_1111"){i4=11}
                if (input$assurance4444=="g4" & input$gamme4444=="_2222"){i4=12}
                if (input$assurance4444=="g4" & input$gamme4444=="_3333"){i4=13}
                
                pj4(c(i1,i2,i3,i4),14)
            }
            else if (input$assurance_p4!="pj4"){
                if (input$assurance4=="a4" & input$gamme4=="_1111"){i1=2}
                if (input$assurance4=="a4" & input$gamme4=="_2222"){i1=3}
                if (input$assurance4=="a4" & input$gamme4=="_3333"){i1=4}
                
                if (input$assurance4=="h4" & input$gamme4=="_1111"){i1=5}
                if (input$assurance4=="h4" & input$gamme4=="_2222"){i1=6}
                if (input$assurance4=="h4" & input$gamme4=="_3333"){i1=7}
                
                if (input$assurance4=="s4" & input$gamme4=="_1111"){i1=8}
                if (input$assurance4=="s4" & input$gamme4=="_2222"){i1=9}
                if (input$assurance4=="s4" & input$gamme4=="_3333"){i1=10}
                
                if (input$assurance4=="g4" & input$gamme4=="_1111"){i1=11}
                if (input$assurance4=="g4" & input$gamme4=="_2222"){i1=12}
                if (input$assurance4=="g4" & input$gamme4=="_3333"){i1=13}
                
                if (input$assurance4=="pj4"){i1=14}
                
                
                if (input$assurance44=="a4" & input$gamme44=="_1111"){i2=2}
                if (input$assurance44=="a4" & input$gamme44=="_2222"){i2=3}
                if (input$assurance44=="a4" & input$gamme44=="_3333"){i2=4}
                
                if (input$assurance44=="h4" & input$gamme44=="_1111"){i2=5}
                if (input$assurance44=="h4" & input$gamme44=="_2222"){i2=6}
                if (input$assurance44=="h4" & input$gamme44=="_3333"){i2=7}
                
                if (input$assurance44=="s4" & input$gamme44=="_1111"){i2=8}
                if (input$assurance44=="s4" & input$gamme44=="_2222"){i2=9}
                if (input$assurance44=="s4" & input$gamme44=="_3333"){i2=10}
                
                if (input$assurance44=="g4" & input$gamme44=="_1111"){i2=11}
                if (input$assurance44=="g4" & input$gamme44=="_2222"){i2=12}
                if (input$assurance44=="g4" & input$gamme44=="_3333"){i2=13}
                
                if (input$assurance44=="pj4"){i2=14}
                
                
                if (input$assurance444=="a4" & input$gamme444=="_1111"){i3=2}
                if (input$assurance444=="a4" & input$gamme444=="_2222"){i3=3}
                if (input$assurance444=="a4" & input$gamme444=="_3333"){i3=4}
                
                if (input$assurance444=="h4" & input$gamme444=="_1111"){i3=5}
                if (input$assurance444=="h4" & input$gamme444=="_2222"){i3=6}
                if (input$assurance444=="h4" & input$gamme444=="_3333"){i3=7}
                
                if (input$assurance444=="s4" & input$gamme444=="_1111"){i3=8}
                if (input$assurance444=="s4" & input$gamme444=="_2222"){i3=9}
                if (input$assurance444=="s4" & input$gamme444=="_3333"){i3=10}
                
                if (input$assurance444=="g4" & input$gamme444=="_1111"){i3=11}
                if (input$assurance444=="g4" & input$gamme444=="_2222"){i3=12}
                if (input$assurance444=="g4" & input$gamme444=="_3333"){i3=13}
                
                if (input$assurance444=="pj4"){i3=14}
                
                
                if (input$assurance4444=="a4" & input$gamme4444=="_1111"){i4=2}
                if (input$assurance4444=="a4" & input$gamme4444=="_2222"){i4=3}
                if (input$assurance4444=="a4" & input$gamme4444=="_3333"){i4=4}
                
                if (input$assurance4444=="h4" & input$gamme4444=="_1111"){i4=5}
                if (input$assurance4444=="h4" & input$gamme4444=="_2222"){i4=6}
                if (input$assurance4444=="h4" & input$gamme4444=="_3333"){i4=7}
                
                if (input$assurance4444=="s4" & input$gamme4444=="_1111"){i4=8}
                if (input$assurance4444=="s4" & input$gamme4444=="_2222"){i4=9}
                if (input$assurance4444=="s4" & input$gamme4444=="_3333"){i4=10}
                
                if (input$assurance4444=="g4" & input$gamme4444=="_1111"){i4=11}
                if (input$assurance4444=="g4" & input$gamme4444=="_2222"){i4=12}
                if (input$assurance4444=="g4" & input$gamme4444=="_3333"){i4=13}
                
                if (input$assurance4444=="pj4"){i4=14}
                
                
                
                if (input$assurance_p4=="a4"){
                    j=2
                    k=3
                    l=4}
                
                if (input$assurance_p4=="h4"){
                    j=5
                    k=6
                    l=7}
                
                if (input$assurance_p4=="s4"){
                    j=8
                    k=9
                    l=10}
                
                if (input$assurance_p4=="g4"){
                    j=11
                    k=12
                    l=13}
                
                
                p4(c(i1,i2,i3,i4),c(j,k,l))
            }
            
            
        }
    })
    
    
    # mot de passe 
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
        reactiveValuesToList(result_auth)
    })
    
    
    #graphs
    output$distPlot <- renderPlot({
        
        if (input$nbcontrats=="c1") {
            
            
            
            if (input$assurance==input$assurance_p){
                cat("Veuillez choisir un produit différent de celui que possède déjà le foyer")
            }
            else if (input$assurance_p=="pj"){
                if (input$assurance=="a" & input$gamme=="_1"){i=2}
                if (input$assurance=="a" & input$gamme=="_2"){i=3}
                if (input$assurance=="a" & input$gamme=="_3"){i=4}
                
                if (input$assurance=="h" & input$gamme=="_1"){i=5}
                if (input$assurance=="h" & input$gamme=="_2"){i=6}
                if (input$assurance=="h" & input$gamme=="_3"){i=7}
                
                if (input$assurance=="s" & input$gamme=="_1"){i=8}
                if (input$assurance=="s" & input$gamme=="_2"){i=9}
                if (input$assurance=="s" & input$gamme=="_3"){i=10}
                
                if (input$assurance=="g" & input$gamme=="_1"){i=11}
                if (input$assurance=="g" & input$gamme=="_2"){i=12}
                if (input$assurance=="g" & input$gamme=="_3"){i=13}
                
                coul <- c("#8064A2") 
                
                X_v=pj11(i,14)
                X_v2=t(X_v)
                X=t(X_v)
                gamme=c("Détention")
                X=data.frame(X_v2,gamme)
                
                pl1=ggplot(data=X,aes(x=gamme,y=X_v2))+geom_bar(stat="identity",fill=coul)
                pl2=pl1 + scale_x_discrete(limits=c("Détention"))
                pl2+geom_text(aes(label=round(X_v2,3)), vjust=1.6, color="black", size=3.5) + labs(x="Gamme",y="Probabilité")
            }  
            else{
                
                if (input$assurance=="a" & input$gamme=="_1"){i=2}
                if (input$assurance=="a" & input$gamme=="_2"){i=3}
                if (input$assurance=="a" & input$gamme=="_3"){i=4}
                
                if (input$assurance=="h" & input$gamme=="_1"){i=5}
                if (input$assurance=="h" & input$gamme=="_2"){i=6}
                if (input$assurance=="h" & input$gamme=="_3"){i=7}
                
                if (input$assurance=="s" & input$gamme=="_1"){i=8}
                if (input$assurance=="s" & input$gamme=="_2"){i=9}
                if (input$assurance=="s" & input$gamme=="_3"){i=10}
                
                if (input$assurance=="g" & input$gamme=="_1"){i=11}
                if (input$assurance=="g" & input$gamme=="_2"){i=12}
                if (input$assurance=="g" & input$gamme=="_3"){i=13}
                
                if (input$assurance=="pj"){i=14}
                
                if (input$assurance_p=="a"){
                    j=2
                    k=3
                    l=4}
                
                if (input$assurance_p=="h"){
                    j=5
                    k=6
                    l=7}
                
                if (input$assurance_p=="s"){
                    j=8
                    k=9
                    l=10}
                
                if (input$assurance_p=="g"){
                    j=11
                    k=12
                    l=13}
                
                
                coul <- c("#C2D638","#FBD073","#E46C0A") 
                
                X_v=p11(i,c(j,k,l))
                X_v2=t(X_v)
                X=t(X_v)
                gamme=c("Bas de gamme","Milieu de gamme","Haut de gamme")
                X=data.frame(X_v2,gamme)
                
                pl1=ggplot(data=X,aes(x=gamme,y=X_v2))+geom_bar(stat="identity",fill=coul)
                pl2=pl1 + scale_x_discrete(limits=c("Bas de gamme", "Milieu de gamme","Haut de gamme"))
                pl2+geom_text(aes(label=round(X_v2,3)), vjust=1.6, color="black", size=3.5) + labs(x="Gamme",y="Probabilité")
            }
            
        }
        
        else if (input$nbcontrats=="c2")  { 
            
            if (input$assurance2==input$assurance22){
                cat("Veuillez choisir des produits différents","\n")
            }
            
            else if(input$assurance2==input$assurance_p2 | input$assurance22==input$assurance_p2){
                cat("Veuillez choisir un produit d'assurance différent de celui qu'il possède","\n")
            }
            
            else{
                
                if (input$assurance_p2=="pj2"){
                    if (input$assurance2=="a2" & input$gamme2=="_11"){i1=2}
                    if (input$assurance2=="a2" & input$gamme2=="_22"){i1=3}
                    if (input$assurance2=="a2" & input$gamme2=="_33"){i1=4}
                    
                    if (input$assurance2=="h2" & input$gamme2=="_11"){i1=5}
                    if (input$assurance2=="h2" & input$gamme2=="_22"){i1=6}
                    if (input$assurance2=="h2" & input$gamme2=="_33"){i1=7}
                    
                    if (input$assurance2=="s2" & input$gamme2=="_11"){i1=8}
                    if (input$assurance2=="s2" & input$gamme2=="_22"){i1=9}
                    if (input$assurance2=="s2" & input$gamme2=="_33"){i1=10}
                    
                    if (input$assurance2=="g2" & input$gamme2=="_11"){i1=11}
                    if (input$assurance2=="g2" & input$gamme2=="_22"){i1=12}
                    if (input$assurance2=="g2" & input$gamme2=="_33"){i1=13}
                    
                    
                    if (input$assurance22=="a2" & input$gamme22=="_11"){i2=2}
                    if (input$assurance22=="a2" & input$gamme22=="_22"){i2=3}
                    if (input$assurance22=="a2" & input$gamme22=="_33"){i2=4}
                    
                    if (input$assurance22=="h2" & input$gamme22=="_11"){i2=5}
                    if (input$assurance22=="h2" & input$gamme22=="_22"){i2=6}
                    if (input$assurance22=="h2" & input$gamme22=="_33"){i2=7}
                    
                    if (input$assurance22=="s2" & input$gamme22=="_11"){i2=8}
                    if (input$assurance22=="s2" & input$gamme22=="_22"){i2=9}
                    if (input$assurance22=="s2" & input$gamme22=="_33"){i2=10}
                    
                    if (input$assurance22=="g2" & input$gamme22=="_11"){i2=11}
                    if (input$assurance22=="g2" & input$gamme22=="_22"){i2=12}
                    if (input$assurance22=="g2" & input$gamme22=="_33"){i2=13}
                    
                    coul <- c("#8064A2") 
                    
                    X_v=pj22(c(i1,i2),14)
                    X_v2=t(X_v)
                    X=t(X_v)
                    gamme=c("Détention")
                    X=data.frame(X_v2,gamme)
                    
                    pl1=ggplot(data=X,aes(x=gamme,y=X_v2))+geom_bar(stat="identity",fill=coul)
                    pl2=pl1 + scale_x_discrete(limits=c("Détention"))
                    pl2+geom_text(aes(label=round(X_v2,3)), vjust=1.6, color="black", size=3.5) + labs(x="Gamme",y="Probabilité")
                } 
                
                else if (input$assurance_p2!="pj2"){
                    
                    if (input$assurance2=="a2" & input$gamme2=="_11"){i1=2}
                    if (input$assurance2=="a2" & input$gamme2=="_22"){i1=3}
                    if (input$assurance2=="a2" & input$gamme2=="_33"){i1=4}
                    
                    if (input$assurance2=="h2" & input$gamme2=="_11"){i1=5}
                    if (input$assurance2=="h2" & input$gamme2=="_22"){i1=6}
                    if (input$assurance2=="h2" & input$gamme2=="_33"){i1=7}
                    
                    if (input$assurance2=="s2" & input$gamme2=="_11"){i1=8}
                    if (input$assurance2=="s2" & input$gamme2=="_22"){i1=9}
                    if (input$assurance2=="s2" & input$gamme2=="_33"){i1=10}
                    
                    if (input$assurance2=="g2" & input$gamme2=="_11"){i1=11}
                    if (input$assurance2=="g2" & input$gamme2=="_22"){i1=12}
                    if (input$assurance2=="g2" & input$gamme2=="_33"){i1=13}
                    
                    if (input$assurance2=="pj2"){i1=14}
                    
                    
                    if (input$assurance22=="a2" & input$gamme22=="_11"){i2=2}
                    if (input$assurance22=="a2" & input$gamme22=="_22"){i2=3}
                    if (input$assurance22=="a2" & input$gamme22=="_33"){i2=4}
                    
                    if (input$assurance22=="h2" & input$gamme22=="_11"){i2=5}
                    if (input$assurance22=="h2" & input$gamme22=="_22"){i2=6}
                    if (input$assurance22=="h2" & input$gamme22=="_33"){i2=7}
                    
                    if (input$assurance22=="s2" & input$gamme22=="_11"){i2=8}
                    if (input$assurance22=="s2" & input$gamme22=="_22"){i2=9}
                    if (input$assurance22=="s2" & input$gamme22=="_33"){i2=10}
                    
                    if (input$assurance22=="g2" & input$gamme22=="_11"){i2=11}
                    if (input$assurance22=="g2" & input$gamme22=="_22"){i2=12}
                    if (input$assurance22=="g2" & input$gamme22=="_33"){i2=13}
                    
                    if (input$assurance22=="pj2"){i2=14}
                    
                    if (input$assurance_p2=="a2"){
                        j=2
                        k=3
                        l=4}
                    
                    if (input$assurance_p2=="h2"){
                        j=5
                        k=6
                        l=7}
                    
                    if (input$assurance_p2=="s2"){
                        j=8
                        k=9
                        l=10}
                    
                    if (input$assurance_p2=="g2"){
                        j=11
                        k=12
                        l=13}
                    
                    coul <- c("#C2D638","#FBD073","#E46C0A") 
                    
                    X_v=p22(c(i1,i2),c(j,k,l))
                    X_v2=t(X_v)
                    X=t(X_v)
                    gamme=c("Bas de gamme","Milieu de gamme","Haut de gamme")
                    X=data.frame(X_v2,gamme)
                    
                    pl1=ggplot(data=X,aes(x=gamme,y=X_v2))+geom_bar(stat="identity",fill=coul)
                    pl2=pl1 + scale_x_discrete(limits=c("Bas de gamme", "Milieu de gamme","Haut de gamme"))
                    pl2+geom_text(aes(label=round(X_v2,3)), vjust=1.6, color="black", size=3.5) + labs(x="Gamme",y="Probabilité")
                    
                    
                }
                
                
            }}
        
        
        
        else if (input$nbcontrats=="c3")  { 
            
            
            if (input$assurance3==input$assurance33 | input$assurance3==input$assurance333 | input$assurance33==input$assurance333){
                cat("Veuillez choisir des produits différents")
            }
            else if(input$assurance3==input$assurance_p3 | input$assurance33==input$assurance_p3 | input$assurance333==input$assurance_p3){
                cat("Veuillez choisir un produit d'assurance différent de celui qu'il possède")
            }
            else{
                if (input$assurance_p3=="pj3"){
                    
                    if (input$assurance3=="a3" & input$gamme3=="_111"){i1=2}
                    if (input$assurance3=="a3" & input$gamme3=="_222"){i1=3}
                    if (input$assurance3=="a3" & input$gamme3=="_333"){i1=4}
                    
                    if (input$assurance3=="h3" & input$gamme3=="_111"){i1=5}
                    if (input$assurance3=="h3" & input$gamme3=="_222"){i1=6}
                    if (input$assurance3=="h3" & input$gamme3=="_333"){i1=7}
                    
                    if (input$assurance3=="s3" & input$gamme3=="_111"){i1=8}
                    if (input$assurance3=="s3" & input$gamme3=="_222"){i1=9}
                    if (input$assurance3=="s3" & input$gamme3=="_333"){i1=10}
                    
                    if (input$assurance3=="g3" & input$gamme3=="_111"){i1=11}
                    if (input$assurance3=="g3" & input$gamme3=="_222"){i1=12}
                    if (input$assurance3=="g3" & input$gamme3=="_333"){i1=13}
                    
                    if (input$assurance33=="a3" & input$gamme33=="_111"){i2=2}
                    if (input$assurance33=="a3" & input$gamme33=="_222"){i2=3}
                    if (input$assurance33=="a3" & input$gamme33=="_333"){i2=4}
                    
                    if (input$assurance33=="h3" & input$gamme33=="_111"){i2=5}
                    if (input$assurance33=="h3" & input$gamme33=="_222"){i2=6}
                    if (input$assurance33=="h3" & input$gamme33=="_333"){i2=7}
                    
                    if (input$assurance33=="s3" & input$gamme33=="_111"){i2=8}
                    if (input$assurance33=="s3" & input$gamme33=="_222"){i2=9}
                    if (input$assurance33=="s3" & input$gamme33=="_333"){i2=10}
                    
                    if (input$assurance33=="g3" & input$gamme33=="_111"){i2=11}
                    if (input$assurance33=="g3" & input$gamme33=="_222"){i2=12}
                    if (input$assurance33=="g3" & input$gamme33=="_333"){i2=13}
                    
                    
                    if (input$assurance333=="a3" & input$gamme333=="_111"){i3=2}
                    if (input$assurance333=="a3" & input$gamme333=="_222"){i3=3}
                    if (input$assurance333=="a3" & input$gamme333=="_333"){i3=4}
                    
                    if (input$assurance333=="h3" & input$gamme333=="_111"){i3=5}
                    if (input$assurance333=="h3" & input$gamme333=="_222"){i3=6}
                    if (input$assurance333=="h3" & input$gamme333=="_333"){i3=7}
                    
                    if (input$assurance333=="s3" & input$gamme333=="_111"){i3=8}
                    if (input$assurance333=="s3" & input$gamme333=="_222"){i3=9}
                    if (input$assurance333=="s3" & input$gamme333=="_333"){i3=10}
                    
                    if (input$assurance333=="g3" & input$gamme333=="_111"){i3=11}
                    if (input$assurance333=="g3" & input$gamme333=="_222"){i3=12}
                    if (input$assurance333=="g3" & input$gamme333=="_333"){i3=13}
                    
                    coul <- c("#8064A2") 
                    
                    X_v=pj33(c(i1,i2,i3),14)
                    X_v2=t(X_v)
                    X=t(X_v)
                    gamme=c("Détention")
                    X=data.frame(X_v2,gamme)
                    
                    pl1=ggplot(data=X,aes(x=gamme,y=X_v2))+geom_bar(stat="identity",fill=coul)
                    pl2=pl1 + scale_x_discrete(limits=c("Détention"))
                    pl2+geom_text(aes(label=round(X_v2,3)), vjust=1.6, color="black", size=3.5) + labs(x="Gamme",y="Probabilité")
                    
                }
                else if (input$assurance_p3!="pj3"){
                    if (input$assurance3=="a3" & input$gamme3=="_111"){i1=2}
                    if (input$assurance3=="a3" & input$gamme3=="_222"){i1=3}
                    if (input$assurance3=="a3" & input$gamme3=="_333"){i1=4}
                    
                    if (input$assurance3=="h3" & input$gamme3=="_111"){i1=5}
                    if (input$assurance3=="h3" & input$gamme3=="_222"){i1=6}
                    if (input$assurance3=="h3" & input$gamme3=="_333"){i1=7}
                    
                    if (input$assurance3=="s3" & input$gamme3=="_111"){i1=8}
                    if (input$assurance3=="s3" & input$gamme3=="_222"){i1=9}
                    if (input$assurance3=="s3" & input$gamme3=="_333"){i1=10}
                    
                    if (input$assurance3=="g3" & input$gamme3=="_111"){i1=11}
                    if (input$assurance3=="g3" & input$gamme3=="_222"){i1=12}
                    if (input$assurance3=="g3" & input$gamme3=="_333"){i1=13}
                    
                    if (input$assurance3=="pj3"){i1=14}
                    
                    
                    if (input$assurance33=="a3" & input$gamme33=="_111"){i2=2}
                    if (input$assurance33=="a3" & input$gamme33=="_222"){i2=3}
                    if (input$assurance33=="a3" & input$gamme33=="_333"){i2=4}
                    
                    if (input$assurance33=="h3" & input$gamme33=="_111"){i2=5}
                    if (input$assurance33=="h3" & input$gamme33=="_222"){i2=6}
                    if (input$assurance33=="h3" & input$gamme33=="_333"){i2=7}
                    
                    if (input$assurance33=="s3" & input$gamme33=="_111"){i2=8}
                    if (input$assurance33=="s3" & input$gamme33=="_222"){i2=9}
                    if (input$assurance33=="s3" & input$gamme33=="_333"){i2=10}
                    
                    if (input$assurance33=="g3" & input$gamme33=="_111"){i2=11}
                    if (input$assurance33=="g3" & input$gamme33=="_222"){i2=12}
                    if (input$assurance33=="g3" & input$gamme33=="_333"){i2=13}
                    
                    if (input$assurance33=="pj3"){i2=14}
                    
                    
                    if (input$assurance333=="a3" & input$gamme333=="_111"){i3=2}
                    if (input$assurance333=="a3" & input$gamme333=="_222"){i3=3}
                    if (input$assurance333=="a3" & input$gamme333=="_333"){i3=4}
                    
                    if (input$assurance333=="h3" & input$gamme333=="_111"){i3=5}
                    if (input$assurance333=="h3" & input$gamme333=="_222"){i3=6}
                    if (input$assurance333=="h3" & input$gamme333=="_333"){i3=7}
                    
                    if (input$assurance333=="s3" & input$gamme333=="_111"){i3=8}
                    if (input$assurance333=="s3" & input$gamme333=="_222"){i3=9}
                    if (input$assurance333=="s3" & input$gamme333=="_333"){i3=10}
                    
                    if (input$assurance333=="g3" & input$gamme333=="_111"){i3=11}
                    if (input$assurance333=="g3" & input$gamme333=="_222"){i3=12}
                    if (input$assurance333=="g3" & input$gamme333=="_333"){i3=13}
                    
                    if (input$assurance333=="pj3"){i3=14}
                    
                    
                    if (input$assurance_p3=="a3"){
                        j=2
                        k=3
                        l=4}
                    
                    if (input$assurance_p3=="h3"){
                        j=5
                        k=6
                        l=7}
                    
                    if (input$assurance_p3=="s3"){
                        j=8
                        k=9
                        l=10}
                    
                    if (input$assurance_p3=="g3"){
                        j=11
                        k=12
                        l=13}
                    
                    coul <- c("#C2D638","#FBD073","#E46C0A") 
                    
                    X_v=p33(c(i1,i2,i3),c(j,k,l))
                    X_v2=t(X_v)
                    X=t(X_v)
                    gamme=c("Bas de gamme","Milieu de gamme","Haut de gamme")
                    X=data.frame(X_v2,gamme)
                    
                    pl1=ggplot(data=X,aes(x=gamme,y=X_v2))+geom_bar(stat="identity",fill=coul)
                    pl2=pl1 + scale_x_discrete(limits=c("Bas de gamme", "Milieu de gamme","Haut de gamme"))
                    pl2+geom_text(aes(label=round(X_v2,3)), vjust=1.6, color="black", size=3.5) + labs(x="Gamme",y="Probabilité")
                    
                    
                }
                
                
            }
            
            
        }
        
        else if (input$nbcontrats=="c4")  { 
            
            
            if (input$assurance4==input$assurance44 | input$assurance4==input$assurance444 | input$assurance4==input$assurance4444 | input$assurance44==input$assurance444 | input$assurance44==input$assurance4444 | input$assurance444==input$assurance4444){
                cat("Veuillez choisir des produits différents")
            }
            else if(input$assurance4==input$assurance_p4 | input$assurance44==input$assurance_p4 | input$assurance444==input$assurance_p4 | input$assurance4444==input$assurance_p4){
                cat("Veuillez choisir un produit d'assurance différent de celui qu'il possède")
            }
            else{
                if (input$assurance_p4=="pj4"){
                    if (input$assurance4=="a4" & input$gamme4=="_1111"){i1=2}
                    if (input$assurance4=="a4" & input$gamme4=="_2222"){i1=3}
                    if (input$assurance4=="a4" & input$gamme4=="_3333"){i1=4}
                    
                    if (input$assurance4=="h4" & input$gamme4=="_1111"){i1=5}
                    if (input$assurance4=="h4" & input$gamme4=="_2222"){i1=6}
                    if (input$assurance4=="h4" & input$gamme4=="_3333"){i1=7}
                    
                    if (input$assurance4=="s4" & input$gamme4=="_1111"){i1=8}
                    if (input$assurance4=="s4" & input$gamme4=="_2222"){i1=9}
                    if (input$assurance4=="s4" & input$gamme4=="_3333"){i1=10}
                    
                    if (input$assurance4=="g4" & input$gamme4=="_1111"){i1=11}
                    if (input$assurance4=="g4" & input$gamme4=="_2222"){i1=12}
                    if (input$assurance4=="g4" & input$gamme4=="_3333"){i1=13}
                    
                    
                    if (input$assurance44=="a4" & input$gamme44=="_1111"){i2=2}
                    if (input$assurance44=="a4" & input$gamme44=="_2222"){i2=3}
                    if (input$assurance44=="a4" & input$gamme44=="_3333"){i2=4}
                    
                    if (input$assurance44=="h4" & input$gamme44=="_1111"){i2=5}
                    if (input$assurance44=="h4" & input$gamme44=="_2222"){i2=6}
                    if (input$assurance44=="h4" & input$gamme44=="_3333"){i2=7}
                    
                    if (input$assurance44=="s4" & input$gamme44=="_1111"){i2=8}
                    if (input$assurance44=="s4" & input$gamme44=="_2222"){i2=9}
                    if (input$assurance44=="s4" & input$gamme44=="_3333"){i2=10}
                    
                    if (input$assurance44=="g4" & input$gamme44=="_1111"){i2=11}
                    if (input$assurance44=="g4" & input$gamme44=="_2222"){i2=12}
                    if (input$assurance44=="g4" & input$gamme44=="_3333"){i2=13}
                    
                    
                    
                    if (input$assurance444=="a4" & input$gamme444=="_1111"){i3=2}
                    if (input$assurance444=="a4" & input$gamme444=="_2222"){i3=3}
                    if (input$assurance444=="a4" & input$gamme444=="_3333"){i3=4}
                    
                    if (input$assurance444=="h4" & input$gamme444=="_1111"){i3=5}
                    if (input$assurance444=="h4" & input$gamme444=="_2222"){i3=6}
                    if (input$assurance444=="h4" & input$gamme444=="_3333"){i3=7}
                    
                    if (input$assurance444=="s4" & input$gamme444=="_1111"){i3=8}
                    if (input$assurance444=="s4" & input$gamme444=="_2222"){i3=9}
                    if (input$assurance444=="s4" & input$gamme444=="_3333"){i3=10}
                    
                    if (input$assurance444=="g4" & input$gamme444=="_1111"){i3=11}
                    if (input$assurance444=="g4" & input$gamme444=="_2222"){i3=12}
                    if (input$assurance444=="g4" & input$gamme444=="_3333"){i3=13}
                    
                    
                    
                    if (input$assurance4444=="a4" & input$gamme4444=="_1111"){i4=2}
                    if (input$assurance4444=="a4" & input$gamme4444=="_2222"){i4=3}
                    if (input$assurance4444=="a4" & input$gamme4444=="_3333"){i4=4}
                    
                    if (input$assurance4444=="h4" & input$gamme4444=="_1111"){i4=5}
                    if (input$assurance4444=="h4" & input$gamme4444=="_2222"){i4=6}
                    if (input$assurance4444=="h4" & input$gamme4444=="_3333"){i4=7}
                    
                    if (input$assurance4444=="s4" & input$gamme4444=="_1111"){i4=8}
                    if (input$assurance4444=="s4" & input$gamme4444=="_2222"){i4=9}
                    if (input$assurance4444=="s4" & input$gamme4444=="_3333"){i4=10}
                    
                    if (input$assurance4444=="g4" & input$gamme4444=="_1111"){i4=11}
                    if (input$assurance4444=="g4" & input$gamme4444=="_2222"){i4=12}
                    if (input$assurance4444=="g4" & input$gamme4444=="_3333"){i4=13}
                    
                    coul <- c("#8064A2") 
                    
                    X_v=pj44(c(i1,i2,i3,i4),14)
                    X_v2=t(X_v)
                    X=t(X_v)
                    gamme=c("Détention")
                    X=data.frame(X_v2,gamme)
                    
                    pl1=ggplot(data=X,aes(x=gamme,y=X_v2))+geom_bar(stat="identity",fill=coul)
                    pl2=pl1 + scale_x_discrete(limits=c("Détention"))
                    pl2+geom_text(aes(label=round(X_v2,3)), vjust=1.6, color="black", size=3.5) + labs(x="Gamme",y="Probabilité")
                    
                }
                else if (input$assurance_p4!="pj4"){
                    
                    if (input$assurance4=="a4" & input$gamme4=="_1111"){i1=2}
                    if (input$assurance4=="a4" & input$gamme4=="_2222"){i1=3}
                    if (input$assurance4=="a4" & input$gamme4=="_3333"){i1=4}
                    
                    if (input$assurance4=="h4" & input$gamme4=="_1111"){i1=5}
                    if (input$assurance4=="h4" & input$gamme4=="_2222"){i1=6}
                    if (input$assurance4=="h4" & input$gamme4=="_3333"){i1=7}
                    
                    if (input$assurance4=="s4" & input$gamme4=="_1111"){i1=8}
                    if (input$assurance4=="s4" & input$gamme4=="_2222"){i1=9}
                    if (input$assurance4=="s4" & input$gamme4=="_3333"){i1=10}
                    
                    if (input$assurance4=="g4" & input$gamme4=="_1111"){i1=11}
                    if (input$assurance4=="g4" & input$gamme4=="_2222"){i1=12}
                    if (input$assurance4=="g4" & input$gamme4=="_3333"){i1=13}
                    
                    if (input$assurance4=="pj4"){i1=14}
                    
                    
                    if (input$assurance44=="a4" & input$gamme44=="_1111"){i2=2}
                    if (input$assurance44=="a4" & input$gamme44=="_2222"){i2=3}
                    if (input$assurance44=="a4" & input$gamme44=="_3333"){i2=4}
                    
                    if (input$assurance44=="h4" & input$gamme44=="_1111"){i2=5}
                    if (input$assurance44=="h4" & input$gamme44=="_2222"){i2=6}
                    if (input$assurance44=="h4" & input$gamme44=="_3333"){i2=7}
                    
                    if (input$assurance44=="s4" & input$gamme44=="_1111"){i2=8}
                    if (input$assurance44=="s4" & input$gamme44=="_2222"){i2=9}
                    if (input$assurance44=="s4" & input$gamme44=="_3333"){i2=10}
                    
                    if (input$assurance44=="g4" & input$gamme44=="_1111"){i2=11}
                    if (input$assurance44=="g4" & input$gamme44=="_2222"){i2=12}
                    if (input$assurance44=="g4" & input$gamme44=="_3333"){i2=13}
                    
                    if (input$assurance44=="pj4"){i2=14}
                    
                    
                    if (input$assurance444=="a4" & input$gamme444=="_1111"){i3=2}
                    if (input$assurance444=="a4" & input$gamme444=="_2222"){i3=3}
                    if (input$assurance444=="a4" & input$gamme444=="_3333"){i3=4}
                    
                    if (input$assurance444=="h4" & input$gamme444=="_1111"){i3=5}
                    if (input$assurance444=="h4" & input$gamme444=="_2222"){i3=6}
                    if (input$assurance444=="h4" & input$gamme444=="_3333"){i3=7}
                    
                    if (input$assurance444=="s4" & input$gamme444=="_1111"){i3=8}
                    if (input$assurance444=="s4" & input$gamme444=="_2222"){i3=9}
                    if (input$assurance444=="s4" & input$gamme444=="_3333"){i3=10}
                    
                    if (input$assurance444=="g4" & input$gamme444=="_1111"){i3=11}
                    if (input$assurance444=="g4" & input$gamme444=="_2222"){i3=12}
                    if (input$assurance444=="g4" & input$gamme444=="_3333"){i3=13}
                    
                    if (input$assurance444=="pj4"){i3=14}
                    
                    
                    if (input$assurance4444=="a4" & input$gamme4444=="_1111"){i4=2}
                    if (input$assurance4444=="a4" & input$gamme4444=="_2222"){i4=3}
                    if (input$assurance4444=="a4" & input$gamme4444=="_3333"){i4=4}
                    
                    if (input$assurance4444=="h4" & input$gamme4444=="_1111"){i4=5}
                    if (input$assurance4444=="h4" & input$gamme4444=="_2222"){i4=6}
                    if (input$assurance4444=="h4" & input$gamme4444=="_3333"){i4=7}
                    
                    if (input$assurance4444=="s4" & input$gamme4444=="_1111"){i4=8}
                    if (input$assurance4444=="s4" & input$gamme4444=="_2222"){i4=9}
                    if (input$assurance4444=="s4" & input$gamme4444=="_3333"){i4=10}
                    
                    if (input$assurance4444=="g4" & input$gamme4444=="_1111"){i4=11}
                    if (input$assurance4444=="g4" & input$gamme4444=="_2222"){i4=12}
                    if (input$assurance4444=="g4" & input$gamme4444=="_3333"){i4=13}
                    
                    if (input$assurance4444=="pj4"){i4=14}
                    
                    
                    
                    if (input$assurance_p4=="a4"){
                        j=2
                        k=3
                        l=4}
                    
                    if (input$assurance_p4=="h4"){
                        j=5
                        k=6
                        l=7}
                    
                    if (input$assurance_p4=="s4"){
                        j=8
                        k=9
                        l=10}
                    
                    if (input$assurance_p4=="g4"){
                        j=11
                        k=12
                        l=13}
                    
                    
                    coul <- c("#C2D638","#FBD073","#E46C0A") 
                    
                    X_v=p44(c(i1,i2,i3,i4),c(j,k,l))
                    X_v2=t(X_v)
                    X=t(X_v)
                    gamme=c("Bas de gamme","Milieu de gamme","Haut de gamme")
                    X=data.frame(X_v2,gamme)
                    
                    pl1=ggplot(data=X,aes(x=gamme,y=X_v2))+geom_bar(stat="identity",fill=coul)
                    pl2=pl1 + scale_x_discrete(limits=c("Bas de gamme", "Milieu de gamme","Haut de gamme"))
                    pl2+geom_text(aes(label=round(X_v2,3)), vjust=1.6, color="black", size=3.5) + labs(x="Gamme",y="Probabilité")
                    
                    
                    
                }
                
                
                
            }
            
            
        }
        
    })
    
    output$logothelem <- renderImage({
        list(src = "thelem.png",width="200px")
    }, deleteFile = FALSE)
    
    
    
    
    
}) 

# Run the application 
shinyApp(ui = ui, server = server)
