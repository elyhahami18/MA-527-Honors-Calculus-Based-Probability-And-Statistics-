install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='3lyhahami',token='8545BA0CE5031FC07520106144C8915F', secret='VDyFKFiDWUwVjMpGTjisiu1frQWA+45EIPXlOwPG')
install.packages(c('ggplot2', 'shiny'))

##data table
mydata2<-read.csv("/Users/elyhahami/Documents/HCBPS Folder/2014 Stocks.csv",header=TRUE)
library(ggplot2)
mydata5<-as.data.frame(subset(mydata2, PE.ratio!=0)) 
mydata6<-as.data.frame(subset(mydata2, Payout.Ratio!=0)) 
mydata2$Payout.Ratio

Sector <- mydata2$Sector
PE.ratio <- mydata2$PE.ratio
PEdist <- ggplot(data = mydata5, mapping = aes(x = PE.ratio)) +
  geom_density(aes(fill = Sector)) +
  facet_wrap(~ Sector) + 
  scale_x_continuous(limits = c(-100, 100))

library(ggplot2)
Payoutdist <- ggplot(data = mydata2, mapping = aes(x = mydata2$payoutRatio)) 
  geom_density(aes(fill = Sector)) +
  facet_wrap(~ Sector) + 
  scale_x_continuous(limits = c(-2, 2))
library(ggplot2)

linreg <- ggplot(data = mydata2, mapping = aes(x = PE.ratio, y = Payout.Ratio)) +
  geom_point() +
  stat_smooth(method = lm) +
  ggtitle("Linear Regression of Technical Indicators")  +
  annotate(geom="text", x=80, y=.25, label="y = 0.3890106x + .125",
           color="blue")+
  annotate(geom="text", x=80, y=.15, label="Kendall Tau Correlation Coefficient: 0.389", 
           color = "blue")+
  xlab("PE Ratio")+
  xlim(0, 100)+
  ylim(0, 2)+
  ylab("Payout Ratio")+
  theme_linedraw()

PE.Ratio <- mydata2$PE.ratio
payout.lm = lm(Payout.Ratio ~ PE.Ratio, data = mydata2)
payout.res = resid(payout.lm)

# Define UI for application
library(shiny)
ui <- fluidPage(
  navbarPage("Stock Profitability", 
             
             tabPanel("Linear Regression: Technical Indicators", 
                      mainPanel(
                        plotOutput("linregPlot", click = "plot_click"), 
                        verbatimTextOutput("info"), plotOutput("residplot", click = "plot_click1"), 
                        verbatimTextOutput("info1")
                        
                      )), 
             tabPanel("Histogram of Price Variance Across All Sectors",
                      selectInput(inputId = "n_breaks",
                                  label = "Number of Bins in histogram (approximate):",
                                  choices = c(10, 25, 40, 55, 70, 85, 100, 115, 130, 145, 160), 
                                  selected = 85), 
                      mainPanel(
                        plotOutput(outputId = "hist")
                      )
                      
                      
             ),
             tabPanel("Per-Sector Density Plots ",
                      mainPanel(
                        plotOutput("PEplot"), verbatimTextOutput("summary1"),
                        plotOutput("Payoutplot"), verbatimTextOutput("summary2"),
                      ),
             ),
             tabPanel("Aggregate Summary Table",
                      mainPanel(
                        DT::dataTableOutput("mytable"))
             )
             
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$PEplot <- renderPlot({
    PEdist
  })
  
  output$Payoutplot <- renderPlot({
    Payoutdist
  })
  output$mytable <- DT::renderDataTable({
    data.frame(mydata2 [, c(1, 223, 224, 225)])
  })
  output$linregPlot <- renderPlot({
    linreg
    
    
  })
  output$summary1 <- renderPrint({
    summary(mydata5$PE.ratio)    
  })
  output$summary2 <- renderPrint({
    summary(mydata6$payoutRatio)    
  })
  
  output$info <- renderText({
    paste0("Click on the above linear regression plot to produce an output below:\nP/E Ratio (Sharings Price / Earnings = ", 
           input$plot_click$x, "\nPayout Ratio (Total Dividends / Net Income)= ", 
           input$plot_click$y)
    
  })
  
  output$info1 <- renderText({
    paste0("Click on the above residual regression plot to produce an output below:\nP/E Ratio (Sharings Price / Earnings = ", 
           input$plot_click1$x, "\nPayout Ratio (Total Dividends / Net Income)= ", 
           input$plot_click1$y)
    
  })
  
  output$residplot <- renderPlot({
    plot(mydata2$PE.ratio, payout.res[1:length(mydata2$PE.ratio)],
         xlim = c(0,100),
         ylim=c(-25,25),
         xlab = "PE Ratio" ,
         ylab = "Payout Ratio", 
         main = "Plot of Residuals")
    
  })
  
  output$hist <- renderPlot({
    ggplot(mydata2, aes(x=mydata2$X2015.PRICE.VAR....))+
      xlab("Price Variance (%)")+
      geom_histogram(bins = input$n_breaks)+
      xlim(-100, 100)
  })
  
  
}

# Run the application 
shinyApp(ui, server)
library(rsconnect)
deployApp()

# above works!!! now make it interactive
#selectInput(
#inputId = "color1", label = "Choose color", choices = c("Black", "Orange", "Yellow"),

library(rsconnect)
deployApp()
