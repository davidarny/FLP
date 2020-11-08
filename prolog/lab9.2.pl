/**
a) Создать базу данных "Хобби".
Предикат likes определяет отношение человек - хобби.
*/
likes(ellen, reading).
likes(john, computers).
likes(john, badminton).
likes(john, photo).
likes(john, reading).
likes(leonard, badminton).
likes(eric, swimming).
likes(eric, reading).
likes(eric, chess).
likes(paul, swimming).

/**
b) Составить вопрос и найти тех, кто имеет четыре хобби.
*/
four_hobbies(X):-aggregate(count, Y^likes(X, Y), 4).

/**
c) Составить вопрос и найти тех, у кого одинаковые хобби.
*/
same_hobby(X, Y):-likes(X, Z), likes(Y, Z), dif(X, Y).
