iri<-iris
len<-nrow(iri)

#������� ����������
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

#������� ���������� ������������ ����������
sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- nrow(xl)
  #n <- ncol(xl) - 1
  n<-2
  #��������� �������, �������� ����������
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  #��������� ��
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl)
}

#�������� ��������� �������
kNN <- function(xl, z, k)
{
  orderedXl <- sortObjectsByDist(xl, z) #�������� ��������������� �� ����������� ������
  n <- dim(orderedXl)[2] - 1
  #����� ��������� ������� � ������� ����� ��� ��� ����� ������� ����������� ����
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}

#�������� ����������� �������� ��� ������ ������������ ���������� �������
LO<-c()
for(k in 1:9){
  LOO<-0
  for(i in 1:len){
    obj<-iri[i,1:2] # i-� �������
    resp<-iri[i,5] #����� i-�� �������
    clss<-(kNN(iri[-i,],obj,k)) #�������� ����� ������� �� kNN
    if(clss!=resp){ #��������� �����������
      LOO<-LOO+1
    }
  }
  LO<-c(LO,LOO/len) #���������� �������� ��������,
                    #��� ���������� ������� ����������� ���������� �������
}
which.min(LO) #����������� ���������� ������� ����� ��������� ���������� �����

#������ ����������� LOO �� k
plot(seq(9),LO,type = "o",main = "LOO from k", xlab = "k-values",ylab = "LOO-values",col = "red")

#������������ ��������� �� ������
colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], asp = 1, main = "kNN")

#����� �������������
for(i in seq(1,7,0.1)){
  for(j in seq(0,2.5,0.1)){
    class <- kNN(iris[, 3:5], c(i,j), k=6)
    points(i, j, pch = 21, bg = "white", col = colors[class], asp = 1)
  }
}
points(iris[, 3], iris[, 4], pch = 21, bg = colors[iris$Species], asp = 1)



