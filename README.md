# SMPR
## Метрические алгоритмы классификации  
###		Алгоритм ближайших соседей  
Метрический алгоритм - алгоритм классификации основанный на оценке близости объектов, используя функцию расстояния. 

При этом метрические алгоритмы основываются на гипотезе компактности, которяа говорит что схожие объекты чаще лежат в одном классе, чем в разных, мера сходства здесь - введенное расстояние.
### Алгоритм k ближайших соседей (kNN)  
Метрический алгоритм классификации основан на оценивании общих свойств объектов. Классифицируемый объект относится к тому классу, к которому относятся ближайшие к нему объекты выборки. 
Пусть задана обучающая выборка пар «объект-ответ».

Зададим функцию расстояния на множестве объектов. Эта функция должна быть достаточно точной "мерой" схожести объектов. Для каждой точки выборки отсортируем остальные объекты по близости расстояния. 


Алгоритм зависит от параметра k, оптимальное значение которого определяется по критерию скользящего контроля,в нашем случае используется метод исключения объектов по одному (leave-one-out cross-validation).  
Оптимальным значением является k=6, при нем LOO=0.033.

![Карта классификации kNN](https://github.com/FerideM/SMPR/blob/master/knn%20map.JPG)
![LOO от k](https://github.com/FerideM/SMPR/blob/master/LOO%20k.JPG)  

Код: [KNN](https://github.com/FerideM/SMPR/blob/master/All%20KNN.R)

### Алгоритм k взвешенных ближайших соседей (kwNN)  
Метод взвешенных ближайших соседей отличается от kNN тем, что оценивает степень важности каждого объекта выборки, где параметр w - вес объекта и w = q^i. q - число ( 0 < q < 1), его оптимальное значение мы определяем по критерию скользящего контроля.  При q = 1 получаем наилучший результат, LOO=0.033.
![Карта классификации kwNN](https://github.com/FerideM/SMPR/blob/master/kwnn%20map.JPG)
![LOO от q](https://github.com/FerideM/SMPR/blob/master/LOO%20q.JPG)  

Код: [KwNN](https://github.com/FerideM/SMPR/blob/master/All%20KWNN.R)

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

Код: [pwin](https://github.com/FerideM/SMPR/blob/master/pwindow.R)

## Байесовские алгоритмы классификации  
###	Линии уровня нормального распределения

Случайная величина имеет многомерное нормальное распределение. Нормальное распределение имеет плотность:
![p](https://github.com/FerideM/SMPR/blob/master/p.JPG)

Параметр µ является мат.ожиданием, а Σ — матрицей ковариации нормального распределения.
Матрица Σ является симметричной и положительно определенной.

Линии уровня плотности нормального распределения представляют собой эллипсы. Можно выделить три основных
вида этих линий уровня в зависимости от значения матрицы ковариации. Если матрица
ковариации пропорциональна единичной, т.е. имеет вид Σ = αI, то все компоненты нормального
распределения являются независимыми друг от друга и имеют одинаковую дисперсию α. Линии
уровня при этом образуют окружности. Диагональная матрица ковариации соответствует независимым компонентам, но с различными дисперсиями. Линии уровня в этом случае являются эллипсами, параллельными координатным осям. Наконец, произвольная положительная определенная
матрица ковариации соответствует эллипсам общего вида.

![Линии уровня](https://github.com/FerideM/SMPR/blob/master/lines.JPG)

Как работает программа:

![](https://github.com/FerideM/SMPR/blob/master/levln1.JPG)
![](https://github.com/FerideM/SMPR/blob/master/levln2.JPG)

![](https://github.com/FerideM/SMPR/blob/master/levln3.JPG)

Shiny: [lines](https://feridem.shinyapps.io/LevelLines/)

###	Наивный нормальный байесовский классификатор
Наивный Байесовский классификатор один из самых простых из алгоритмов классификации. Тем не менее, очень часто он работает не хуже, а то и лучше более сложных алгоритмов.
Определить класс объекта мы можем по формуле: 

![Байес](https://github.com/FerideM/SMPR/blob/master/a1.gif)

плотность для которой мы считаем, применяя формулу:

![Плотность](https://github.com/FerideM/SMPR/blob/master/p1.gif)

Алгоритм состоит в том, что мы получаем значения мат. ожидания и дисперсии(мю и сигма) и строим по ним выборку с нормальным распределением, для которой потом восстанавливаем мю и сигма. Затем классифицируем точки по формуле выше и можем строить карту классификации.

Пример работы программы:

![Пример](https://github.com/FerideM/SMPR/blob/master/naive.JPG)

Shiny: [bayes](https://feridem.shinyapps.io/NaiveBayes/)

###	Подстановочный алгоритм (plug-in)
Случайная величина имеет многомерное нормальное распределение. Нормальное распределение имеет плотность:
![p](https://github.com/FerideM/SMPR/blob/master/p.JPG)

Параметр µ является мат.ожиданием, а Σ — матрицей ковариации нормального распределения.
Матрица Σ является симметричной и положительно определенной.

Смысл алгоритма заключается в том, что мы восстанавливаем математическое ожидание и матрицу ковариации для каждого класса y, используя принцип максимального правдоподобия:

![Мю](https://github.com/FerideM/SMPR/blob/master/mu.gif)

![Сигма](https://github.com/FerideM/SMPR/blob/master/sg.gif)

Затем мы подставляем их в формулу оптимального байеса, предполагая что ковариационные матрицы не равны.

Строим разделяющую кривую, которая задается уравнением a*x1^2 + b*x1*x2 + c*x2 + d*x1 + e*x2 + f = 0.

В зависимости от вводимых значений µ и Σ можно увидеть различные типы разделяющей кривой:

![Парабола](https://github.com/FerideM/SMPR/blob/master/plg1.JPG)
![Гипербола](https://github.com/FerideM/SMPR/blob/master/plg2.JPG)
![Эллипс](https://github.com/FerideM/SMPR/blob/master/plg3.JPG)
![Прямая](https://github.com/FerideM/SMPR/blob/master/plg4.JPG)

Shiny: [plug-in](https://feridem.shinyapps.io/PlugIN/)

###	Линейный дискриминант Фишера – ЛДФ
У нас по-прежнему случайная величина имеет нормальное распределение, но теперь мы еще предполагаем, что ковариационные матрицы равны. И теперь для их восстановления нам нужно использовать все объекты обучающей выборки.

Является частным случаем подстановочного алгоритма, и считается более устойчивым, если нам нужно получить более точную классификацию.
Лучше всего работает на простых задачах, в которых объекты классов больше похожи друг на друга.

Пример работы алгоритма:

![](https://github.com/FerideM/SMPR/blob/master/LDF.JPG)

Код: [LDF](https://github.com/FerideM/SMPR/blob/master/LDF.R)

## Линейные алгоритмы классификации
Линейный классификатор имеет вид - 𝑎(𝑥,𝑤) = 𝑠𝑖𝑔𝑛<𝑤, 𝑥> , где 𝑤 - вектор весов. Объекты классифицируются по принципу: если <𝑤, 𝑥> положительное, то мы относим объект к классу +1, если <𝑤, 𝑥> отрицательное, то к -1. 
<𝑤, 𝑥> = 0 - разделяющая гиперплоскость.

Для нахождения ошибок нужно проверить отступ объекта от классификации. Если отстум имеет отрицательный знак, то это значит что алгоритм допускает ошибку на этом объекте. 

Также для лучшей классификации нам нужна функция потерь, которая характеризует потери при неправильном принятии решений.

###	ADALINE. Правило Хэбба (персептрон Розенблатта)
Лучше всего помогает определить вектор весов Метод стохастического градиента, который заключается в том, что на каждом шаге мы сдвигаемся в противоположную от градиента сторону до тех пока не прекратятся изменения вектора 𝑤. Стохастическим он называется потому что обучение происходит на случайном объекте выборки. 

Линейные алгоритмы отличаются функциями потерь. В ADALINE мы используем квадратичную функцию (1-x)^2, а в правиле Хэбба - кусочно-линейную max(-x,0). Также их отличает то как мы обновляем значения весов.

Пример работы алгоритма, где красная линия показывает ADALINE, а желтая - правило Хэбба:

![](https://github.com/FerideM/SMPR/blob/master/adaline.JPG)

Shiny: [ADALINE_HEBB](https://feridem.shinyapps.io/ADALINE/)

###	Логистическая регрессия
Линейный байесовсий классификатор. Здесь мы используем логарифмическую функцию потерь. Одной из его особенностей является то, что он способен также определить и степень принадлежности объекта классу.

Содержит отличие в обновлении вектора весов, где мы используем сигмоидную функцию.

Пример работы всех 3 алгоритмов, красная линия -ADALINE, желтая - правило Хэбба, фиолетовая - логистческая регрессия:

![](https://github.com/FerideM/SMPR/blob/master/all_lines.JPG)

Пример работы алгоритма, где видна степень принадлежности:

![](https://github.com/FerideM/SMPR/blob/master/prob.JPG)

Shiny: [Linear](https://feridem.shinyapps.io/AllLines/)
