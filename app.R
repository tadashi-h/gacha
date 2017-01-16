library(shiny)
library(googleVis)

# Input probability of rare and trails of draw
ui <- shinyUI(
    pageWithSidebar(
        headerPanel("Gacha Gacha chart"),
        sidebarPanel(
            numericInput("prob", label = h3("Probability"),
                         value = 0.015, step = 0.001),
            helpText("SSR: 0.015, SR: 0.10"),
            
            hr(),
        
            numericInput("num", label = h3("Number of Gacha"),
                         value = 10, step = 1),
            helpText("The probability you can get at least once..."),
        
            fluidRow(column(10, verbatimTextOutput("value")))
        ),
        
        mainPanel(
        htmlOutput("plot1"),
        
        hr(),
        
        htmlOutput("plot2")
        )
    )
)

# Define server logic required to draw graph
server <- shinyServer(
    function(input, output) {
        output$plot1 <- renderGvis({
            
          #Calculate the probability of rare
          p <- input$prob
          num <- input$num
          num_of_rare <- 0:(10*log10(input$num))
          dens <- dbinom(num_of_rare, size=num, p=p)
          data1 <- data.frame(num_of_rare, dens)
          gvisLineChart(data1, options=list(
            title="Density",
            vAxis="{title:'probability'}", hAxis="{title:'expected number of rare'}"))
        })
    
        output$plot2 <- renderGvis({
            # Use to the negative binomial distribution.
            # For example, if q = 100, p = 0.01, size = 1, a numerical value of about 0.63 will be returned,
            # it means if 100 people draw 100 times, 37 people can not get rare.
    
            p <- input$prob
            num <- 100    #input$num
            data2 <- data.frame(num_of_rare=1:num, prob=pnbinom(1:num, size=1, p=p))
            gvisLineChart(data2, options=list(
                title="Expectation when you draw 100 times",
                vAxis="{title:'probability'}", hAxis="{title:'number of trials'}"))
        })
    
        output$value <- renderPrint({
            #The probability you can get at least once
      
            least_one <- round(sum(dbinom(1:input$num, input$num +1, p=input$prob)), 3)
            least_one
        })
    }
)

# Run the application 
shinyApp(ui = ui, server = server)

