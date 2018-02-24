#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Mortgage overpayment calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("bal", "Current mortgage balance:", min = 5000, value=100000),
        numericInput("rem", "Remaining term (years):", min=5, max=30, value = 10),
        numericInput("curRate", "Current interest rate:", min=0, max=10, value = 4),
        
        numericInput("monOv", "Regular monthly overpayment:", min=0, value = 300),
        
        numericInput("lumOv", "Lump sum overpayment:", min=0, value = NULL)
        
        # sliderInput("bins",
        #              "Number of bins:",
        #              min = 1,
        #              max = 50,
        #              value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   output$distPlot <- renderPlot({
     
         morbal = function(pv,r,n,p,nt) {
           return(pv*(1+r)^n - p*((1+r)^nt-1)/r);
         }
         
         amt = function(L, r, t) {
           return (L*r/(1-(1+r)^-t))
         }
         
         yearlyAMT = amt(input$bal, r=input$curRate/100, t=input$rem)
         per = seq(from=0, to=input$rem, by=0.5)
         
         amtBalances = round(morbal(pv=input$bal, r=input$curRate/100, n=input$rem, nt=per, p=yearlyAMT))
         ovBalances = round(morbal(pv = input$bal, r = input$curRate / 100,
                                   n = input$rem, nt = per, p = (yearlyAMT + input$monOv*12)) )
         # ovBalancesPos = ovBalances[ovBalances >= 0 ? ovBalances : 0]
         amtdf<-data.frame(years=per,amtBalances=amtBalances, ovBalances=ovBalances)
         amtdf$ovBalancesPos <- ifelse(ovBalances > 0, ovBalances, 0)
         # mutate(amtdf,
         #        ovBalancesPos = ifelse(ovBalances > 0, ovBalances, 0)
         #        )
         
         # ovdf<-data.frame(x=per,y=ovBalances)
         
         ggplot(data = amtdf) +
           geom_line(mapping = aes(x = years, y = amtBalances), color = "blue") +
           geom_point(mapping = aes(x = years, y = amtBalances), color = "blue") +
           geom_line(mapping = aes(x = years, y = ovBalancesPos), color = "green") +
           geom_point(mapping = aes(x = years, y = ovBalancesPos), color = "green")
         
           # geom_line(color="blue")
         # ggplot(ovdf, aes(x=x,y=y)) + geom_line(color="green")
     
      # generate bins based on input$bins from ui.R
      # x    <- seq(from=0, to=12*input$rem)
      # y    <- seq(from=0, to=12*input$rem)
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

