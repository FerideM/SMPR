library(shiny)
library(MASS)
#iaeaiue aaean
bayes <- function(data, mu1, sg1, mu2, sg2){
  n <- length(data)
  res1 <- log(1*0.5)
  for(i in 1 : n){
    smth1 <- Pyj(sg1[i], data[i], mu1[i])
    res1 <- res1 + log(smth1)
  }
  res2 <- log(1*0.5)
  for(i in 1 : n){
    smth2 <- Pyj(sg2[i], data[i], mu2[i])
    res2 <- res2 + log(smth2)
  }
  
  if(res1 > res2){ 
    class <- 1
  } else { 
    class <- 2
  }
  return(class)
}
#au??eneyai ieioiinou
Pyj <- function(disp, data, mu){
  pyj <- (1/(disp*sqrt(2*pi))) * exp(-1 * ((data - mu)^2)/(2*disp^2))
  return(pyj)
}

newmu <- function(data){
  l <- nrow(data)
  res <- c(sum(data[,1])/l, sum(data[,2])/l)
  return(res)
}

newsg <- function(data, mu){
  l <- nrow(data) 
  res <- c(sum((data[,1] - mu[1])^2)/l, sum((data[,2] - mu[2])^2)/l)
  return(res)
}

#iaiaeu aaiaa cia??aiee
ui <- fluidPage(titlePanel("Naive Bayes"),
                sidebarLayout(position = "right", sidebarPanel(
                  fluidRow(
                    h5("Values Sigma 1"),
                    column(6, textInput(inputId = "a", "a", 2)),
                    column(6,textInput(inputId = "b", "b", 0)),
                    column(6,textInput(inputId = "c", "c", 0)),
                    column(6,textInput(inputId = "d", "d", 2)),
                    h5("Values Sigma 2"),
                    column(6, textInput(inputId = "e", "e", 1)),
                    column(6,textInput(inputId = "f", "f", 0)),
                    column(6,textInput(inputId = "g", "g", 0)),
                    column(6,textInput(inputId = "h", "h", 1)),
                    h5("Values Mu 1"),
                    column(6, textInput(inputId = "v", "1)", 0)),
                    column(6,textInput(inputId = "w", "2)", 0)),
                    h5("Values Mu 2"),
                    column(6,textInput(inputId = "m", "1)", 1)),
                    column(6,textInput(inputId = "n", "2)", 1)),
                  )
                ),
                mainPanel(h4("Map"),
                          plotOutput("Plot")
                )))


server <- function(input, output) {
  
  output$Plot <- renderPlot({
    #iieo??aai aaaaaiiua aaiiua
    matr1 <- matrix(as.numeric(c(input$a, input$b, input$c, input$d)), 2, 2)
    matr2 <- matrix(as.numeric(c(input$e, input$f, input$g, input$h)), 2, 2)
    mu1 <- as.numeric(c(input$v,input$w))
    mu2 <- as.numeric(c(input$m,input$n))
    
    d1 <- cbind(mvrnorm(50, mu1, matr1), 1) 
    d2 <- cbind(mvrnorm(50, mu2, matr2), 2) 
    data <- rbind(d1,d2)
    
    #au??eniyai iiaua cia??aiey i?? e neaiu
    nmu1 <- newmu(d1[,1:2])
    nmu2 <- newmu(d2[,1:2])
    nsg1 <- newsg(d1[,1:2], nmu1)
    nsg2 <- newsg(d2[,1:2], nmu2)
    
    colors <- c('1' = "red", '2' = "green3")
    plt <- c()
    #no??iei ea??oo eeanneoeeaoee
    for(i in seq(-10,10,0.2)){
      for(j in seq(-10,10,0.2)){
        #iieo??aai eeann oi??ee ii iaeaiiio aaeano
        class<-bayes(c(i,j), nmu1, nsg1, nmu2, nsg2)         
        plt <- rbind(plt,c(i,j,class))
        
      }
    }
    #aecoaeecaoey
    plot(data[,1:2], pch = 21, col = colors[data[,3]], bg = colors[data[,3]], asp = 1)
    points(plt[,1:2], pch = 21, col =colors[plt[,3]], asp = 1)
  })
}

shinyApp(ui = ui, server = server)