library(shiny)
library(MASS)


## Нормализация обучающей выборки
trainingSampleNormalization <- function(xl)
{
  n <- dim(xl)[2] - 1
  for(i in 1:n)
  {
    xl[, i] <- (xl[, i] - mean(xl[, i])) / sd(xl[, i])
  }
  return (xl)
}

## Добавление колонки для из -1 для w0
trainingSamplePrepare <- function(xl)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  xl <- cbind(xl[, 1:n], seq(from = -1, to = -1,
                             length.out = l), xl[, n + 1])
}

## Квадратичная функция потерь
lossQuad <- function(x)
{
  return ((x-1)^2)
}
lossP <- function(x)
{
  return (max(-x, 0))
}

## Логарифмическая функция потерь
lossLog <- function(x)
{
  return (log2(1 + exp(-x)))
}

## Сигмоидная функция
sigmoidFunction <- function(z)
{
  return (1 / (1 + exp(-z)))
}

## Стохастический градиент для логистической регрессии
sg.LogRegression <- function(xl)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  lambda <- 1/l
  ## initialize Q
  Q <- 0
  for (i in 1:l)
  {
    ## calculate the scalar product <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## calculate a margin
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossLog(margin)
  }
  repeat
  {
    # select the random index from the error objects errorIndexes 
    i <- sample(1:l, 1)
    iterCount <- iterCount + 1
    # i <- sample(1:l, 1)
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    ## calculate the scalar product <w,xi>
    wx <- sum(w * xi)
    ## make a gradient step
    margin <- wx * yi
    ex <- lossLog(margin)
    eta <- 0.3#1 / iterCount
    w <- w + eta * xi * yi * sigmoidFunction(-wx * yi)
    ## Calculate a new Q
    Qprev <- Q
    Q <- (1 - lambda) * Q + lambda * ex
    if (abs(Qprev - Q) / abs(max(Qprev, Q)) < 1e-5)
      break
  }
  return (w)
}
    
## Стохастический градиент для ADALINE
sg.ADALINE <- function(xl, eta = 1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  ## initialize Q
  Q <- 0
  for (i in 1:l)
  {
    ## calculate the scalar product <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## calculate a margin
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossQuad(margin)
  }
  repeat
  {
    ## calculate the margins for all objects of the training sample
    margins <- array(dim = l)
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
    ## select the error objects
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0)
    {
      # select the random index from the errors
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      ## calculate the scalar product <w,xi>
      wx <- sum(w * xi)
      ## make a gradient step
      margin <- wx * yi
      ## calculate an error
      ex <- lossQuad(margin)
      eta <- 1 / sqrt(sum(xi * xi))
      w <- w - eta * (wx - yi) * xi
      ## Calculate a new Q
      Qprev <- Q
      Q <- (1 - lambda) * Q + lambda * ex
    }
    else
    {
      break
    }
  }
  return (w)
}

sg.Hebb <- function(xl, eta = 0.1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  ## initialize Q
  Q <- 0
  for (i in 1:l)
  {
    ## calculate the scalar product <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## calculate a margin
    margin <- wx * xl[i, n + 1]
    # Q <- Q + lossQuad(margin)
    Q <- Q + lossP(margin)
  }
  repeat
  {
    ## Поиск ошибочных объектов
    margins <- array(dim = l)
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
    ## выбрать ошибочные объекты
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0)
    {
      # выбрать случайный ошибочный объект
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      
      yi <- xl[i, n + 1]
      w <- w + eta * yi * xi
    }
    else
      break;
  }
  return (w)
}

ui <- fluidPage(titlePanel("ALL_Lines"),
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
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))
    
    newxl <- trainingSampleNormalization(xl)
    newxl <- trainingSamplePrepare(newxl)
    
    w <- sg.ADALINE(newxl)
    ww <- sg.Hebb(newxl)
    www <- sg.LogRegression(newxl)
    ## Визуализация
    colors <- c("green3","white", "blue")
    plot(newxl[, 1], newxl[, 2], pch = 21, bg = colors[xl[,3]+2], asp = 1)
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col ="red")
    abline(a = ww[3] / ww[2], b = -ww[1] / ww[2], lwd = 3, col ="yellow")
    abline(a = www[3] / www[2], b = -www[1] / www[2], lwd = 3, col ="purple")
    
    for(i in seq(-10,10,0.1))
      for(j in seq(-10,10,0.1)){
        if(c(i,j,-1) %*% www >0){
          M <- c(www %*% c(i,j,-1))
          color <- adjustcolor("blue", sigmoidFunction(M)/1.5)
          points(i,j, col = color, bg = color, pch = 18)
        }
        else{
          M <- c(www %*% c(i,j,-1))* (-1)
          color <- adjustcolor("green", sigmoidFunction(M)/1.5)
          points(i,j, col = color, bg = color, pch = 18)
        }
        
      }
  })
}

shinyApp(ui = ui, server = server)
