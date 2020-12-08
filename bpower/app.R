#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinythemes)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage("Why not?", theme = shinytheme("lumen"),
               
        tabPanel("Treatment effect",fluid=TRUE,
    # Application title
            sidebarLayout(
            titlePanel("Treatment effect (in treatable)"),

    # Sidebar with a slider input for number of bins 
            sidebarLayout(
            sidebarPanel(
                sliderInput("pnull",
                        "Percentage null:",
                        min = 0,
                        max = 100,
                        value = 20),
                hr(),
                helpText("Adjust these so the graph matches your prior"),
                sliderInput("beta_a","Beta(a,)",min=1,max=10,value=2),
                sliderInput("beta_b","Beta(,b)",min=1,max=10,value=4)
            ),

        # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distPlot")
            )
        )
        )
    ),
    tabPanel("Treatable",fluid=TRUE,
             # Application title
             sidebarLayout(
                 titlePanel("Fraction of patients who could potentially benefit"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(

                         helpText("Adjust these so the graph matches your prior"),
                         sliderInput("treat_a","Beta(a,)",min=1,max=50,value=10),
                         sliderInput("treat_b","Beta(,b)",min=1,max=50,value=30)
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("treatPlot")
                 )
             )
    )),
    tabPanel("Everyone",fluid=TRUE,
             # Application title
             sidebarLayout(
                 titlePanel("True treatment effect in all-comers"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("power", "Power:",
                                      c("80%" = .8,
                                        "85%" = .85,
                                        "90%" = .9)),
                         sliderInput("power_at","at all-comers mortality reduction of:",min=1,max=20,value=4)
                         
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("nettreatPlot"),
                         hr(),
                         br(),
                         h4(paste("Expected fraction of positive trials:"),
                            textOutput("fraction_positive"))
                     )
                 )
             )
    ))
    
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        x[runif(10000)<(input$pnull/100)]<-0

        # draw the histogram with the specified number of bins
        hist(x, breaks = (0:20), col = 'red', border = 'white',prob=TRUE,xlab="True treatment effect (% mortality reduction)",
             main="")
    })
    
    output$treatPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        xgrid<-seq(0,1,length=101)
        ygrid<-dbeta(xgrid,input$treat_a,input$treat_b)
        curve(dbeta(x,input$treat_a,input$treat_b),from=0,to=1,xlab="Proportion treatable")
        polygon(c(xgrid,0),c(ygrid,0),col="blue")

    })
    
    output$nettreatPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        x[runif(10000)<(input$pnull/100)]<-0
        y<-rbeta(10000,input$treat_a,input$treat_b)
        z<-x*y
        hist(z,breaks = (0:20), col = 'purple', border = 'white',prob=TRUE,xlab="Net treatment effect (% mortality reduction)",
             main="" )
        
    })
    
    output$fraction_positive<-renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        x[runif(10000)<(input$pnull/100)]<-0
        y<-rbeta(10000,input$treat_a,input$treat_b)
        d<-x*y
        "Need formula"       
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
