/**
a) Создать базу данных для отношений parent,male,female.
*/
parent(bill, joe).
parent(bill, ann).
parent(sue, ann).
parent(sue, joe).
parent(joe, tammy).
parent(paul, jim).
parent(mary, jim).
parent(jim, bob).
parent(ann, bob).

child(Y, X):-parent(X, Y).

male(bill).
male(paul).
male(joe).
male(jim).
male(bob).
female(sue).
female(mary).
female(ann).
female(tammy).

mother(X, Y):-parent(X, Y), female(X).

/**
b) Составить вопрос и найти в базе данных бабушку для bob.
*/
grandma(X, Y):-parent(E, X), parent(Y, E), female(Y).

/**
с) Составить вопрос и найти в базе данных внука.
*/
grandchild(X, Y):-child(Z, X), child(Y, Z).

/**
d) Составить вопрос и найти в базе данных сестру для jim, включив в базу дaнных  правило сестра (sister (X,Y)).
*/
different(X, Y):-X\=Y.
sister(X, Y):-female(X), parent(Z, Y), parent(Z, X), different(X, Y).

/**
e) Определите отношение "тётя" (aunt (X,Y)), используя правило для sister (X,Y).
*/
aunt(X, Y):-parent(Z, Y), sister(X, Z).

/**
f) Определите отношение "кузин"- cousin(X,Y).
*/
sister_or_brother(X, Y):-parent(Z, Y), parent(Z, X), different(X, Y).
cousin(X, Y):-parent(Z, X), sister_or_brother(E, Z), parent(E, Y).
