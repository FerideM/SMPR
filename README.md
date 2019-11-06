# SMPR
## Метрические алгоритмы классификации  
###		Алгоритм ближайших соседей  
Метрический алгоритм - алгоритм классификации основанный на оценке близости объектов, используя функцию расстояния. 

При этом метрические алгоритмы основываются на гипотезе компактности, которяа говорит что схожие объекты чаще лежат в одном классе, чем в разных, мера сходства здесь - введенное расстояние.
### Алгоритм k ближайших соседей (kNN)  
Метрический алгоритм классификации, основанный на оценивании сходства объектов. Классифицируемый объект относится к тому классу, которому принадлежат ближайшие к нему объекты обучающей выборки. 
Пусть задана обучающая выборка пар «объект-ответ».

Пусть на множестве объектов задана функция расстояния . Эта функция должна быть достаточно точной "мерой" сходства объектов. Для точки выборки отсортируем остальные её объекты по возрастанию значения расстояния. Для успешного обучения выборка должна быть пересортирована для каждого новой точки.


Алгоритм зависит от параметра k, оптимальное значение которого определяется по критерию скользящего контроля,в нашем случае используется метод исключения объектов по одному (leave-one-out cross-validation).  
Оптимальным значением является k=6, при нем LOO=0.033.

![Карта классификации kNN](https://github.com/FerideM/SMPR/blob/master/knn%20map.JPG)
![LOO от k](https://github.com/FerideM/SMPR/blob/master/LOO%20k.JPG)  


### Алгоритм k взвешенных ближайших соседей (kwNN)  
Метод взвешенных ближайших соседей, в отличии от kNN, оценивает степень важности каждого объекта обучающей выборки, используя параметр w - вес объекта, где w = q^i. q - некоторое число ( 0 < q < 1), оптимальное значение которого определяется по критерию скользящего контроля.  При q = 1 получаем наилучший результат, LOO=0.033.
![Карта классификации kwNN](https://github.com/FerideM/SMPR/blob/master/kwnn%20map.JPG)
![LOO от q](https://github.com/FerideM/SMPR/blob/master/LOO%20q.JPG)  

### Метод парзеновского окна
Парзеновское окно - частный случай метрических алгоритмов.
Парзеновская оценка плотности имеет вид:
![](https://github.com/FerideM/SMPR/blob/master/PWindw.gif)

Формулы ядер:
Прямоугольное ядро: ![](https://github.com/FerideM/SMPR/blob/master/15.gif)
Треугольное ядро: ![](https://github.com/FerideM/SMPR/blob/master/13.gif)
Квартическое ядро: ![](https://github.com/FerideM/SMPR/blob/master/12121.gif)
Ядро Епанечникова: ![](https://github.com/FerideM/SMPR/blob/master/11.gif)
Гауссовское ядро: ![](https://github.com/FerideM/SMPR/blob/master/14.gif)

h - параметр характеризующий ширину окна, который мы находим по критерию скользящего контроля LOO.
При H = 0.35 получаем наилучший результат, LOO=0.04.

![Прямоугольное ядро](https://github.com/FerideM/SMPR/blob/master/map%20rect.JPG)
![LOO для прямоугольного ядра](https://github.com/FerideM/SMPR/blob/master/LOO%20h%20rect.JPG)

![Треугольное ядро](https://github.com/FerideM/SMPR/blob/master/map%20triang.JPG)
![LOO для треугольного ядра](https://github.com/FerideM/SMPR/blob/master/LOO%20h%20triang.JPG)

![Квартическое ядро](https://github.com/FerideM/SMPR/blob/master/quart%20map.JPG)
![LOO для квартического ядра](https://github.com/FerideM/SMPR/blob/master/LOO%20h%20quartic.JPG)

![Ядро Епанечникова](https://github.com/FerideM/SMPR/blob/master/epanech%20map.JPG)
![LOO для ядра Епанечникова](https://github.com/FerideM/SMPR/blob/master/LOO%20h%20epanech.JPG)

![Гауссовское ядро](https://github.com/FerideM/SMPR/blob/master/gauss%20map1.JPG)
![LOO для гауссовского ядра](https://github.com/FerideM/SMPR/blob/master/LOO%20h%20gauss.JPG)
