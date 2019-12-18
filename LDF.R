library(shiny)
library(MASS)

## Оценка ковариационной матрицы для ЛДФ
estimateFisherCovarianceMatrix <- function(objects1,
                                           objects2, mu1, mu2)
{
  rows1 <- dim(objects1)[1]
  rows2 <- dim(objects2)[1]
  rows <- rows1 + rows2
  cols <- dim(objects1)[2]
  sigma <- matrix(0, cols, cols)
  for (i in 1:rows1)
  {
    sigma <- sigma + (t(objects1[i,] - mu1) %*%
                        (objects1[i,] - mu1)) / (rows + 2)
  }
  for (i in 1:rows2)
  {
    sigma <- sigma + (t(objects2[i,] - mu2) %*%
                        (objects2[i,] - mu2)) / (rows + 2)
  }
  return (sigma)
}



#панель ввода значений
ui <- fluidPage(titlePanel("LDF"),
                sidebarLayout(position = "right", sidebarPanel(
                  fluidRow(
                    h5("Values Sigma 1"),
                    column(6, textInput(inputId = "a", "а", 10)),
                    column(6,textInput(inputId = "b", "b", 0)),
                    column(6,textInput(inputId = "c", "c", 0)),
                    column(6,textInput(inputId = "d", "d", 1)),
                    h5("Values Sigma 2"),
                    column(6, textInput(inputId = "e", "e", 1)),
                    column(6,textInput(inputId = "f", "f", 0)),
                    column(6,textInput(inputId = "g", "g", 0)),
                    column(6,textInput(inputId = "h", "h", 5)),
                    h5("Values Mu 1"),
                    column(6, textInput(inputId = "v", "1)", 1)),
                    column(6,textInput(inputId = "w", "2)", 0)),
                    h5("Values Mu 2"),
                    column(6,textInput(inputId = "m", "1)", 15)),
                    column(6,textInput(inputId = "n", "2)", 0)),
                  )
                ),
                mainPanel(h4("Plot"),
                          plotOutput("Plot")
                )))

server <- function(input, output) {
  
  output$Plot <- renderPlot({
    #получаем введенные данные
    Sigma1 <- matrix(as.numeric(c(input$a, input$b, input$c, input$d)), 2, 2)
    Sigma2 <- matrix(as.numeric(c(input$e, input$f, input$g, input$h)), 2, 2)
    Mu1 <- as.numeric(c(input$v,input$w))
    Mu2 <- as.numeric(c(input$m,input$n))
    
    xy1 <- mvrnorm(100, Mu1, Sigma1)
    xy2 <- mvrnorm(100, Mu2, Sigma2)
    ## Собираем два класса в одну выборку
    xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))
    ## Рисуем обучающую выборку
    colors <- c(rgb(0/255, 162/255, 232/255), rgb(0/255, 200/255, 0/255))
    plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1)
    
    ## Оценивание
    objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
    objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]
    
    mu1 <- estimateMu(objectsOfFirstClass)
    mu2 <- estimateMu(objectsOfSecondClass)
    Sigma <-estimateFisherCovarianceMatrix(objectsOfFirstClass,objectsOfSecondClass, mu1, mu2)
    ## Получаем коэффициенты ЛДФ
    inverseSigma <- solve(Sigma)
    alpha <- inverseSigma %*% t(mu1 - mu2)
    mu_st <- (mu1 + mu2) / 2
    beta <- mu_st %*% alpha
    
    ## Рисуем дискриминантую функцию – красная линия
    abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col ="red", lwd = 3)

  })
}

shinyApp(ui = ui, server = server)

deployApp()