library(shiny)

P <- function(X, m, mtrx){ #функци€ рассчитывающа€ плотность
  det <- det(mtrx)
  a <- mtrx[1,1]
  b <- mtrx[1,2]
  c <- mtrx[2,1]
  d <- mtrx[2,2]
  A <- d/det  #вычисление коэффициентов дл€ преобразованной формулы
  B <- (-b-c)/det
  C <- a/det
  D <- (-2*d*m[1] + b*m[2] + c*m[2])/det
  E <- (b*m[1] + c*m[1] - 2*a*m[2])/det
  f <- (d*m[1]*m[1]-b*m[1]*m[2]-c*m[1]*m[2]+a*m[2]*m[2])/det
  res <- 1/sqrt(2*pi*d) * exp(-1/2 * (A*X[1]^2+B*X[2]*X[1]+C*X[2]^2+D*X[1]+E*X[2]+f))
  return(res)
  
}

#бокава€ панель дл€ выбора параметров
ui <- fluidPage(titlePanel("Level lines"),
                sidebarLayout(position = "right", sidebarPanel(
                  helpText("Change values to built another lines and see difference."),
                  fluidRow(
                    column(12, sliderInput(inputId = "a", "а", 1, 20, 1, 1)),
                    column(12,sliderInput(inputId = "b", "b", 0, 20, 0, 1)),
                    column(12,sliderInput(inputId = "c", "c", 0, 20, 0, 1)),
                    column(12,sliderInput(inputId = "d", "d", 1, 20, 1, 1)),
                    column(12,sliderInput(inputId = "p", "p", 0.01, 0.1, 0.01, 0.01)))),
                    mainPanel(h4("Brush and double-click to zoom"),
                              plotOutput("lines", dblclick = "plot1_dblclick",
                                         brush = brushOpts(id = "plot1_brush", fill = "pink",
                                           resetOnNew = TRUE
                                         )))))

#построение линий
server <- function(input, output) {
  ranges <- reactiveValues(x = NULL, y = NULL)
  output$lines <- renderPlot({
    matr <- matrix(c(input$a, input$c, input$b, input$d), 2, 2)
    m <- c(0,0)
    if(det(matr)>0){
      #границы
      x1 <- matr[1,1]+5
      x2 <- -matr[1,1]-5
      y1 <- matr[2,2]+5
      y2 <- -matr[2,2]-5
      
      #точки
      x <- seq(x2, x1, length.out = 100)
      y <- seq(y2, y1, length.out = 100)
      xl<-length(x)
      yl<-length(y)
      pm <- matrix(0, xl, yl) 
      
      for(i in 1:xl){
        for(j in 1:yl){
          
          pm[i,j] <- P(c(x[i], y[j]), m, matr) # дл€ каждой точки находим плотность
        
        }
      }
      #строим линии
      g <- F
      for(i in seq(0.001, 0.1, input$p)){
        contour(x, y, pm, col = randomcoloR::randomColor(), levels = i, add = g, asp = 1)
        g <- T 
      } 
      #попытка сделать zoom
      observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
          
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
    }
  })
}

shinyApp(ui = ui, server = server)