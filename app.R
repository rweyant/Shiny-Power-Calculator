library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)

server <- function(input,output){

  output$p1 <- renderUI({
    if(input$test.type=='Proportions'){
      sliderInput(inputId = 'p1',label = 'Sample Proportion 1',value = 0.3,min=0,max=1)
    }else if(input$test.type=='Means'){
      numericInput(inputId = 'x1',label = 'Sample Mean 1',value = 20)
    }
  })
  output$var1 <- renderUI({
    if(input$test.type=='Means'){
      numericInput(inputId = 'v1',label = 'Sample Variance 1',value = 5)
    }
  })
  output$p2 <- renderUI({
    if(input$test.type=='Proportions'){
      sliderInput(inputId = 'p2',label = 'Sample Proportion 2',value = 0.31,min=0,max=1)
    }else if(input$test.type=='Means'){
      numericInput(inputId = 'p1',label = 'Sample Mean ',value = 10)
    }
  })
  output$var2 <- renderUI({
    if(input$test.type=='Means'){
      numericInput(inputId = 'v2',label = 'Sample Variance 2',value = 5)
    }
  })  
  
  # Select type of x-axis
  output$xaxis <- renderUI({
    if(input$type.of.calculation=='Power'){
      radioButtons(inputId = 'xaxis',
                   inline = T,
                   selected = 'Type I Error',
                   choices = c('Type I Error','Sample Size'),
                   label = 'x-axis'
      )
    } else if(input$type.of.calculation=='Sample Size'){
      radioButtons(inputId = 'xaxis',
                   inline = T,
                   selected = 'Type I Error',
                   choices = c('Type I Error','Power'),
                   label = 'x-axis'
      )
    }
  })
  
  # Show Sample Size as groups if it is a power calculation and x-axis is type I error
  output$sample.size.group <- renderUI({
    if(input$type.of.calculation=='Power' & input$xaxis != 'Sample Size'){
      checkboxGroupInput(inputId = 'n.groups',
                         label = 'Sample Size',
                         choices=c('100','1000','5000','10000','50000','100000'),
                         inline = F,
                         selected = c('100','1000','5000','10000','50000','100000'))
    }
  })
  
  # Show Type I Error groups if x-axis is either Sample Size or power
  output$type.I.error.group <- renderUI({
    if(input$xaxis != 'Type I Error'){
      checkboxGroupInput(inputId = 'alpha.groups',
                         label = 'Type I Error',
                         choices=c('0.05','0.01','0.001','0.0001','0.0001','0.00001'),
                         inline = F,
                         selected = c('0.05','0.01','0.001','0.0001','0.0001','0.00001'))
    }
  })
  # Show Power groups if type of calculation and x-axis is type I error
  output$power.group <- renderUI({
    if(input$type.of.calculation=='Sample Size' & input$xaxis != 'Power'){
      checkboxGroupInput(inputId = 'power.groups',
                         label = 'Power',
                         choices=c('0.7','0.8','0.9','0.95','0.99'),
                         inline = F,
                         selected = c('0.7','0.8','0.9','0.95','0.99'))
    }
  })
  
  # Determine x-axis limits with double slider
  output$xaxis.limits <- renderUI({
    # Issue here [ERROR]
    if(input$xaxis=='Type I Error'){
      sliderInput(inputId = 'xaxis.limits',
                  label = 'Type I Error',
                  value=c(0.01,0.05),
                  min = 0.0001, 
                  max=0.05,
                  step=0.0001)
    } else if(input$xaxis=='Power'){
      sliderInput(inputId = 'xaxis.limits',
                  label = 'Power',
                  value=c(0.8,0.95),
                  min = 0.5, 
                  max=1,
                  step=0.01)
    }else if(input$xaxis=='Sample Size'){
      sliderInput(inputId = 'xaxis.limits',
                  label = 'Sample Size',
                  value=c(100,10000),
                  min = 10, 
                  max=100000,
                  step=10)
    }
  })
  
  output$powerPlot <- renderPlot({
    names(input) %>% print
    'test.type: ' %>% paste(input$test.type) %>% print
    'type.of.calculation: ' %>% paste(input$type.of.calculation)  %>% print
    'n.groups: ' %>% paste(input$n.groups) %>% print
    'alpha.groups: ' %>% paste(input$alpha.groups) %>% print
    'power.groups: ' %>% paste(input$power.groups) %>% print
    'sample.size: ' %>% paste(input$sample.size) %>% print
    'xaxis: ' %>% paste(input$xaxis) %>% print
    'xaxis.limits ' %>% paste(input$xaxis.limits) %>% print  
    print("\n\n\n-------")
    
    if(input$test.type=='Proportions'){
      if(input$type.of.calculation=='Power'){
        if(input$xaxis=='Sample Size'){
          df <- sapply(seq(to=input$xaxis.limits[1],
                           from=input$xaxis.limits[2],
                           length.out=20),
                       function(n){ 
                        sapply(as.numeric(input$alpha.groups),function(x){
                          power.prop.test(n=n, 
                                          p1=input$p1, 
                                          p2=input$p2, 
                                          sig.level=x, 
                                          alternative='two.sided')$power})
                      }) %>% as.data.frame
        } else if(input$xaxis=='Type I Error'){
          df <- sapply(seq(to=input$xaxis.limits[1],
                           from=input$xaxis.limits[2],
                           length.out=20),
                       function(alpha){ 
                         sapply(as.numeric(input$n.groups),function(x){
                           power.prop.test(n=x, 
                                           p1=input$p1, 
                                           p2=input$p2, 
                                           sig.level=alpha, 
                                           alternative='two.sided')$power})
                       }) %>% as.data.frame
        }
      }
    }
    df %>% dim %>% print
    df %>% head %>% print
#     x <- .05
#     
#     df <- data.frame(sapply(n,function(n1){ 
#       sapply(alpha,function(x){
#         power.prop.test(n=n1, 
#                         p1=input$p1, 
#                         p2=input$p2, 
#                         #                         p1=0.3, 
#                         #                         p2=0.4, 
#                         sig.level=x, 
#                         alternative='two.sided')$power})
#     }))
#     df$alpha <- alpha

#     df$x <- .
#     df$y <- .
#     df$groups <- .
#     df$xmin <- .
#     df$xmax <- .

#     colnames(df) <- c(n,'alpha')
#     stacked.df <- gather(df,n,power,-alpha)
#     stacked.df %>% head
#     ggplot(stacked.df,aes(x=alpha,y=power,group=n,color=n,size=1.1))+
#       geom_line()+
#       scale_color_discrete()+
#       scale_size_continuous(guide=F,range = c(1.15,1.15))+
#       guides(color=guide_legend(override.aes=list(size=2)))+
#       theme_classic()  
  })
  
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = 'test.type',
                   inline = T,
                   selected = 'Proportions',
                   choices = c('Means','Proportions'),
                   label = 'Test of '
                  ),
      radioButtons(inputId = 'type.of.calculation',
                   inline = T,
                   selected = 'Power',
                   choices = c('Power','Sample Size'),
                   label = 'Type of Calculation'
      ),
      uiOutput('xaxis'),    
      uiOutput('sample.size.group'),
      uiOutput('type.I.error.group'),
      uiOutput('power.group'),
      uiOutput('xaxis.limits'),
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




#     
#       x.var <- seq(from=input$xaxis.limits[1],to=input$xaxis.limits[2],
#                   by = (input$xaxis.limits[2]-input$xaxis.limits[1]) / 20)
#       if(input$xaxis != 'Type I Error'){
#         groups <-  input$alpha.groups
#       } else if(input$type.of.calculation=='Sample Size' & input$xaxis != 'Power'){
#         groups <- input$power.groups
#       }else if(input$type.of.calculation=='Power' & input$xaxis != 'Sample Size'){
#         groups <- n.groups
#       }
