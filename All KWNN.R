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
  return (orderedXl);
}

#алгоритм взвешенных соседей
kwNN <- function(xl, z, k, q)
{
  orderedXl <- sortObjectsByDist(xl, z) #получаем отсортированные по расстояниям данные
  n <- dim(orderedXl)[2] - 1
  #берем ближайших соседей и находим среди них тот класс который весит больше всего
  classes <- orderedXl[1:k, n + 1]
  counts <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  #подсчет веса
  for (i in 1:k){
    w <- q^i
    counts[[classes[i]]] <- counts[[classes[i]]] + w                  
  }
  class <- names(which.max(counts))
  return (class)
}

#алгоритм скользящего контроля для поиска оптимального параметра весовой функции
LO<-c()
Q<-seq(0.0,1.0,0.2)
for(q in Q){
  LOO<-0
  for(i in 1:len){
    obj<-iri[i,1:2] # i-й образец
    resp<-iri[i,5] #класс i-го образца
    clss<-kwNN(iri[-i,],obj,k=6,q) #получаем класс образца по kwNN
    if(clss!=resp){ #сравнение результатов
      LOO<-LOO+1
    }
  }
  LO<-c(LO,LOO/len) #результаты проверки критерия,
                    #где наименьший покажет оптимальное значение параметра
}
Q[which.min(LO)] #оптимальное значение параметра чтобы правильно определить класс
min(LO)

#график зависимости LOO от q
plot(Q,LO,type = "o",main = "LOO from q", xlab = "q-values",ylab = "LOO-values",col = "red")

#визуализация разбиения на классы
colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], asp = 1, main = "kwNN")

#карта классификации
for(i in seq(1,7,0.1)){
  for(j in seq(0,2.5,0.1)){
    class <- kwNN(iris[, 3:5], c(i,j), k=6, q = 1)
    points(i, j, pch = 21, bg = "white", col = colors[class], asp = 1)
  }
}
points(iris[, 3], iris[, 4], pch = 21, bg = colors[iris$Species], asp = 1)
