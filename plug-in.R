library(shiny)
library(MASS)

## Восстановление центра нормального распределения
estimateMu <- function(objects)
{
  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  mu <- matrix(NA, 1, cols)
  for (col in 1:cols)
  {
    mu[1, col] = mean(objects[,col])
  }
  return(mu)
}

## Восстановление ковариационной матрицы нормального распределения
estimateCovarianceMatrix <- function(objects, mu)
{
  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  sigma <- matrix(0, cols, cols)
  for (i in 1:rows)
  {
    sigma <- sigma + (t(objects[i,] - mu) %*%
                        (objects[i,] - mu)) / (rows - 1)
  }
  return (sigma)
}

## Получение коэффициентов подстановочного алгоритма
getPlugInDiskriminantCoeffs <- function(mu1, sigma1, mu2, sigma2)
{
  ## Line equation: a*x1^2 + b*x1*x2 + c*x2 + d*x1 + e*x2 + f = 0
  invSigma1 <- solve(sigma1)
  invSigma2 <- solve(sigma2)
  f <- log(abs(det(sigma1))) - log(abs(det(sigma2))) + mu1 %*% invSigma1 %*% t(mu1) - mu2 %*% invSigma2 %*% t(mu2);
  alpha <- invSigma1 - invSigma2
  a <- alpha[1, 1]
  b <- 2 * alpha[1, 2]
  c <- alpha[2, 2]
  beta <- invSigma1 %*% t(mu1) - invSigma2 %*% t(mu2)
  d <- -2 * beta[1, 1]
  e <- -2 * beta[2, 1]
  return (c("x^2" = a, "xy" = b, "y^2" = c, "x" = d, "y"= e, "1" = f))
}

#панель ввода значений
ui <- fluidPage(titlePanel("Plug-in"),
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
    sigma1 <- estimateCovarianceMatrix(objectsOfFirstClass, mu1)
    sigma2 <- estimateCovarianceMatrix(objectsOfSecondClass, mu2)
    
    coeffs <- getPlugInDiskriminantCoeffs(mu1, sigma1, mu2,sigma2)
    
    ## Рисуем дискриминантую функцию – красная линия
    x <- y <- seq(-10, 20, len=100)
    z <- outer(x, y, function(x, y) coeffs["x^2"]*x^2 + coeffs["xy"]*x*y + coeffs["y^2"]*y^2 + coeffs["x"]*x + coeffs["y"]*y + coeffs["1"])
    contour(x, y, z, levels=0, drawlabels=FALSE, lwd = 3, col = "red", add = TRUE)

  })
}

shinyApp(ui = ui, server = server)