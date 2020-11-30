/*
1. Укажите, красным или зеленым является отсечение в процедуре:
Если отсечение красное, приведите пример цели, которая дает разные решения при наличии отсечения в процедуре и его изъятии.

1 [1]
*/
member(X,[X|_]):-!.
member(X,[_|L]):-member(X,L).

/*
3. Написать отношение fib(N,F), которое находит по аргументу N (номер числа в последовательности) число Фибоначчи F.
*/
fib(N, error):-N < 1, !.
fib(N, 1):-N < 3, !.
fib(N, F):-
    fib(N - 1, F1),
    fib(N - 2, F2),
    F is F1 + F2.

/*
4. Для каждого из перечисленных методов сортировки выполните следующее:
Дополните программы сортировки предикатами ввода-вывода таким образом,
чтобы мог быть реализован диалог.
*/
order(X, Y):-X =< Y.

dialog(F):-write("list? "), read(X), call(F, X, R), !, write("answer: "), write(R).

permutation(L, [H|T]):-append(V, [H|U], L), append(V, U, W), permutation(W, T).
permutation([], []).
sorted([X,Y|T]):-order(X,Y), sorted([Y|T]).
sorted([_]).
sortn(L1, L2):-permutation(L1, L2), sorted(L2),!.
sort_4_1:-dialog(sortn).

busort(L, S):-swap(L, M), !, busort(M, S).
busort(L, L):-!.
swap([X, Y|R], [Y, X|R]):-order(Y, X).
swap([X|R], [X|R1]):-swap(R, R1).
sort_4_2:-dialog(busort).

insortx(X, [A|L], [A|M]):-order(A, X), !, insortx(X, L, M).
insortx(X, L, [X|L]).
insort([], []).
insort([X|L], M):-insort(L, N), insortx(X, N, M).
sort_4_3:-dialog(insort).

qsort([], []).
qsort([H|Tail], S):-
    split(H, Tail, Small, Big),
    qsort(Small, Small1),
    qsort(Big, Big1),
    append(Small1, [H|Big1], S).
split(H, [A|Tail], [A|Small], Big):-
    order(A, H), !,
    split(H, Tail, Small, Big).
split(H, [A|Tail], Small, [A|Big]):-split(H, Tail, Small, Big).
split(_, [], [], []).
sort_4_4:-dialog(qsort).

/*
5. Построить бесповторный упорядоченный список L3, состоящий из всех элементов,
содержащихся как в списке L1, так и в списке L2.
*/
common(L1, L2, L3):-findall(X, (member(X, L1); member(X, L2)), R), sort(R, L3).

/*
7. Определить самый распространенный элемент X в списке L.
Если в списке несколько самых распространенных элементов, то ответ надо сделать в виде списка из самых распространенных элементов.
*/
seconds([], []).
seconds([[_, X]|T], R):-seconds(T, L), append(L, [X], R).
most_oft(L, X):-
    sort(L, SortedList),
    findall([Freq, Element], (member(Element, SortedList), include(=(Element), L, XX), length(XX, Freq)), FreqList),
    sort(FreqList, SortedFreqList),
    last(SortedFreqList, [HighestFreq, _]),
    findall([Freq, Element], (member([Freq, Element], SortedFreqList), Freq = HighestFreq), HighestFreqList),
    length(HighestFreqList, HighestFreqListLength),
    (HighestFreqListLength > 1 -> seconds(HighestFreqList, HighestFreqListSeconds), reverse(HighestFreqListSeconds, X) ; last(HighestFreqList, [_, X])).
