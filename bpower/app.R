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

magic_power_number<-function(halfalpha,beta){
    (qnorm(halfalpha)+qnorm(beta))^2
}

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
                helpText("This panel sets the effect size distribution (mortality reduction) across trials, for the subset of patients who could realistically benefit from the intervention. Our prior expectation is that this is mostly non-zero, but rarely more than a 10% mortality reduction"),
                br(),
                sliderInput("pnull",
                        "Percentage null:",
                        min = 0,
                        max = 100,
                        value = 20),
                hr(),
                helpText("Adjust these so the graph matches your prior"),
                sliderInput("beta_a","Beta(a,)",min=1,max=10,value=2),
                sliderInput("beta_b","Beta(,b)",min=1,max=10,value=6)
            ),

        # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distPlot"),
                hr(),
                br(),
                h4(textOutput("prior_mean",inline=TRUE)),
                h4(textOutput("prior_mean0",inline=TRUE)),
                h4(textOutput("prior_median",inline=TRUE)),
                h4(textOutput("prior_median0",inline=TRUE)),
                
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
                         helpText("This panel sets the distribution across trials of the proportion of patients who could realistically benefit from the intervention. Our prior expectation is that this is typically about 20%, but could sometimes be larger"),
                         br(),
                         hr(),
                         helpText("Adjust these so the graph matches your prior"),
                         sliderInput("treat_a","Beta(a,)",min=1,max=50,value=10),
                         sliderInput("treat_b","Beta(,b)",min=1,max=50,value=40)
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
                         helpText("This panel sets the typical power for trials, in terms of the effect size in all-comers. For example, you might think it's typical to have 90% power for a 10% reduction in mortality, or that it's conservative to try 80% power for a 5% reduction."),
                         hr(),
                         
                         radioButtons("power", "Power:",
                                      c("80%" = .8,
                                        "85%" = .85,
                                        "90%" = .9)),
                         sliderInput("power_at","at all-comers mortality reduction of:",min=1,max=20,value=5)
                         
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("nettreatPlot"),
                         hr(),
                         br(),
                         h4(p("Combining the assumptions about treatment effect distribution, fraction treatable, and typical design power, we would expect to see")),
                         h4(textOutput("fraction_positive",inline=TRUE)),
                         h4(textOutput("net_mean",inline=TRUE)),
                         h4(textOutput("net_mean0",inline=TRUE)),
                         h4(textOutput("net_median",inline=TRUE)),
                         h4(textOutput("net_median0",inline=TRUE))
                         
                         
                         
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
        curve(dbeta(x,input$treat_a,input$treat_b),from=0,to=1,xlab="Proportion treatable",ylab="Density")
        polygon(c(xgrid,0),c(ygrid,0),col="blue")

    })
    
    output$prior_mean <- renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        x[runif(10000)<(input$pnull/100)]<-0
        paste0("Mean mortality reduction (in treatable)= ", round(mean(x),1),"%") 
        
    })
    output$prior_median <- renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        x[runif(10000)<(input$pnull/100)]<-0
        paste0("Median mortality reduction (in treatable)= ", round(median(x),1),"%") 
        
    })
    
    output$prior_mean0 <- renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        paste0("Mean non-zero mortality reduction (in treatable)= ", round(mean(x),1),"%") 
        
    })
    
    output$prior_median0 <- renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        paste0("Median non-zero mortality reduction (in treatable)= ", round(median(x),1),"%") 
        
    })
    
    
    output$nettreatPlot <- renderPlot({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        x[runif(10000)<(input$pnull/100)]<-0
        y<-rbeta(10000,input$treat_a,input$treat_b)
        z<-x*y
        hist(z,breaks = (0:20), col = 'purple', border = 'white',prob=TRUE,xlab="Net treatment effect (% mortality reduction)",
             main="" )
        abline(v=input$power_at,lty=2)
        
    })
    
    output$fraction_positive<-renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        x[runif(10000)<(input$pnull/100)]<-0
        y<-rbeta(10000,input$treat_a,input$treat_b)
        d<-x*y
        zthing<-sqrt(magic_power_number(0.025, 1-as.numeric(input$power))*(d/input$power_at))
        zbeta<-zthing+qnorm(0.025)
        power<-pnorm(zthing+qnorm(0.025))
        power_wrong_tail<-pnorm(zthing-qnorm(0.025),lower.tail=FALSE)
        paste("% of positive trials =", round(100*mean((power+power_wrong_tail))))
    })
    
    output$net_mean<-renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        x[runif(10000)<(input$pnull/100)]<-0
        y<-rbeta(10000,input$treat_a,input$treat_b)
        d<-x*y
        paste0("Mean mortality reduction (in all-comers)= ", round(mean(d),1),"%") 
    })
    
    output$net_mean0<-renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        y<-rbeta(10000,input$treat_a,input$treat_b)
        d<-x*y
        paste0("Mean non-zero mortality reduction (in all-comers)= ", round(mean(d),1),"%") 
    })
    
    output$net_median<-renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        x[runif(10000)<(input$pnull/100)]<-0
        y<-rbeta(10000,input$treat_a,input$treat_b)
        d<-x*y
        paste0("Median mortality reduction (in all-comers)= ", round(median(d),1),"%") 
    })
    
    output$net_median0<-renderText({
        x <- rbeta(10000,input$beta_a,input$beta_b)*20
        y<-rbeta(10000,input$treat_a,input$treat_b)
        d<-x*y
        paste0("Median non-zero mortality reduction (in all-comers)= ", round(median(d),1),"%") 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
