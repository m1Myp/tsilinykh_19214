# tsilinykh_19214

Чтобы нереально нарисовать функцию
cabal install diagrams (поставилось с 1 раза на шиндоус 10)

Чтобы сделать график
>>:load Project.hs
>>:main -o "NameSvg.svg" -w 400
-o Куда писать
-w Размер графика, лучше оставлять на 400
Для теста замены графика локально внутри кода копаться в vertices меняя cos(x) на другую функцию,   
также можно менять диапазон "x <- [(-3*pi),(-3*pi + 0.001) .. 3*pi]" на любой, чтобы понять, что функции рисовать он научился.
За выходные надеюсь допилю +- до нормального вида, потом буду делать оформления, циферки там.. оси координат...
