iri<-iris
len<-nrow(iri)

#функция расстояния
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

#функция сортировки относительно расстояния
sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- nrow(xl)
  #n <- ncol(xl) - 1
  n<-2
  #заполняем матрицу, вычисляя расстояния
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  #сортируем ее
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl)
}

#алгоритм ближайщих соседей
kNN <- function(xl, z, k)
{
  orderedXl <- sortObjectsByDist(xl, z) #получаем отсортированные по расстояниям данные
  n <- dim(orderedXl)[2] - 1
  #берем ближайших соседей и находим среди них тот класс который встречается чаще
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}

#алгоритм скользящего контроля для поиска оптимального количества соседей
LO<-c()
for(k in 1:9){
  LOO<-0
  for(i in 1:len){
    obj<-iri[i,1:2] # i-й образец
    resp<-iri[i,5] #класс i-го образца
    clss<-(kNN(iri[-i,],obj,k)) #получаем класс образца по kNN
    if(clss!=resp){ #сравнение результатов
      LOO<-LOO+1
    }
  }
  LO<-c(LO,LOO/len) #результаты проверки критерия,
                    #где наименьший покажет оптимальное количество соседей
}
which.min(LO) #оптимальное количество соседей чтобы правильно определить класс

#график зависимости LOO от k
plot(seq(9),LO,type = "o",main = "LOO from k", xlab = "k-values",ylab = "LOO-values",col = "red")

#визуализация разбиения на классы
colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], asp = 1, main = "kNN")

#карта классификации
for(i in seq(1,7,0.1)){
  for(j in seq(0,2.5,0.1)){
    class <- kNN(iris[, 3:5], c(i,j), k=6)
    points(i, j, pch = 21, bg = "white", col = colors[class], asp = 1)
  }
}
points(iris[, 3], iris[, 4], pch = 21, bg = colors[iris$Species], asp = 1)



