iri<-iris
len<-nrow(iri)
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

rectangle <- function(p,h){
  r<-abs(p/h)
  if(r <= 1){
    return (0.5)
  } else {
    return(0)
  }
}

triangle <- function(p,h){
  r<-abs(p/h)
  if(r <= 1){
    return (1-r)
  } else {
    return(0)
  }
}

quartic <- function(p,h){
  r<-abs(p/h)
  if(r <= 1){
    return ((15/16)*(1-r^2)^2)
  } else {
    return(0)
  }
}

epanech <- function(p,h){
  r<-abs(p/h)
  if(r <= 1){
    return ((3/4)*(1-r^2))
  } else {
    return(0)
  }
}

gauss <- function(p,h){
  r<-abs(p/h)
  if(r <= 1){
    return ((2*pi)^(-1/2) * exp(-1/2 * (r)^2 ))
  } else {
    return(0)
  }
}

sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- nrow(xl)
  #n <- ncol(xl) - 1
  n<-2
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  a<-distances[order(distances[,2], distances[,1]),]
  b<-xl[order(distances[, 2]), ]
  orderedXl <- cbind(b,rast=a[,2])
  orderedXl <- orderedXl[,3:4]
  #orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

pwin <- function(xl, z, h, f)
{
  l <- nrow(xl)
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:l, n]
  
  counts <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in 1:l){
    w<-f(orderedXl[i,2],h)
    counts[[classes[i]]] <- counts[[classes[i]]] + w
    
  }
  if(sum(counts) ==0){
    return ('none')
  }
  else{
    class <- names(which.max(counts))
    return (class)
  }
}

#алгоритм скользящего контроля для поиска оптимального параметра
LO<-c()
H<-seq(0.01,2,0.05)
for(h in H){
  LOO<-0
  for(i in 1:len){
    obj<-iri[i,3:4] # i-й образец

    resp<-iri[i,5] #класс i-го образца
    clss<-pwin(iris[-i,3:5], obj, h, gauss) #получаем класс образца
    if(clss!=resp){ #сравнение результатов
      LOO<-LOO+1
    }
  }
  LO<-c(LO,LOO/len) #результаты проверки критерия,
  #где наименьший покажет оптимальное значение параметра
}
H[which.min(LO)] #оптимальная ширина окна чтобы правильно определить класс
min(LO)

#график зависимости LOO от h
plot(H,LO,type = "o",main = "LOO from h Gauss core", xlab = "h-values",ylab = "LOO-values",col = "red")
points(min(LO),col = "green")

#визуализация разбиения на классы

colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue","none"="white")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], asp = 1, main = "Gauss core")

for(i in seq(1,7,0.1)){
  for(j in seq(0,2.5,0.1)){
    class <- pwin(iris[, 3:5], c(i,j), h=0.35, gauss)
    points(i, j, pch = 21, bg = "white", col = colors[class], asp = 1)
  }
}
points(iris[, 3], iris[, 4], pch = 21, bg = colors[iris$Species], asp = 1)