library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)

server <- function(input,output){
#   output$powerPlot <- renderPlot({  
#     alpha <- 10^((-20:-1000)/10)
#     n <- 10^((1:4)/1)
#     x <- .05
#     
#     df <- data.frame(sapply(n,function(n1){ 
#       sapply(alpha,function(x){ 
#         power.prop.test(n=n1, 
#                         p1=input$p1, 
#                         p2=input$p2, 
# #                         p1=0.3, 
# #                         p2=0.4, 
#                         sig.level=x, 
#                         alternative='two.sided')$power})
#       }))
#     df$alpha <- alpha
#     colnames(df) <- c(n,'alpha')
#     stacked.df <- gather(df,n,power,-alpha)
#     stacked.df %>% head
#     ggplot(stacked.df,aes(x=alpha,y=power,group=n,color=n,size=1.1))+
#       geom_line()+
#       scale_color_discrete()+
# #       scale_size(range = c(1.1,1.1))+
#       scale_size_continuous(guide=F,range = c(1.15,1.15))+
#       guides(color=guide_legend(override.aes=list(size=2)))+
# #       scale_x_log10()+
# #   scale_x_reverse()+
#       theme_classic()
#     
#   }),
  output$samplesize <- renderUI({
    if(input$calc=='power'){
      checkboxGroupInput(inputId = 'n',
                         label = 'Sample Size',
                         choices=c('100','1000','5000','10000','50000','100000'),
                         inline = T,
                         selected = c('100','1000','5000','10000','50000','100000'),
    }
  })
  output$alpha <- renderUI({
    if(TRUE){
      numericInput(inputId = 'alpha',label = 'Type I Error',value=0.05)
    }
  })
  output$p1 <- renderUI({
    if(input$test.type=='proportions'){
      sliderInput(inputId = 'p1',label = 'Sample Proportion 1',value = 0.3,min=0,max=1)
    }else if(input$test.type=='means'){
      numericInput(inputId = 'x1',label = 'Sample Mean 1',value = 20)
    }
  })
  output$var1 <- renderUI({
    if(input$test.type=='means'){
      numericInput(inputId = 'v1',label = 'Sample Variance 1',value = 5)
    }
  })
  output$p2 <- renderUI({
    if(input$test.type=='proportions'){
      sliderInput(inputId = 'p2',label = 'Sample Proportion 2',value = 0.31,min=0,max=1)
    }else if(input$test.type=='means'){
      numericInput(inputId = 'p1',label = 'Sample Mean ',value = 10)
    }
  })
  output$var2 <- renderUI({
    if(input$test.type=='means'){
      numericInput(inputId = 'v2',label = 'Sample Variance 2',value = 5)
    }
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = 'test.type',
                   inline = T,
                   selected = 'proportions',
                   choices = c('means','proportions'),
                   label = 'Type of Test'
                  ),
      radioButtons(inputId = 'calc',
                   inline = T,
                   selected = 'power',
                   choices = c('power','sample size'),
                   label = 'Type of Calculation'
      ),
      radioButtons(inputId = 'xaxis',
                   inline = T,
                   selected = 'Type I Error',
                   choices = c('Type I Error','Sample Size'),
                   label = 'Type of Calculation'
      ),
      uiOutput('samplesize'),
      uiOutput('alpha'),
      uiOutput('p1'),
      uiOutput('var1'),
      uiOutput('p2'),
      uiOutput('var2')
#       numericInput(inputId = 'p1',label = 'Sample Proportion 1',value=0.3),
#       numericInput(inputId = 'p2',label = 'Sample Proportion 2',value=0.4)  
    ),
  mainPanel(plotOutput('powerPlot'))
  )
)

shinyApp(ui=ui,server=server)
