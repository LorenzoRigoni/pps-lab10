% 0: Intro
a.
a.
a :- d.
b.
c :- a, b.
c :- c.

% 1: queries on list

% 1.1: search
% search(Elem, List)

search (X, cons(X, _)).
search (X, cons(_, Xs)) :- search(X, Xs).

% 1.2: search2
% search2(Elem, List)
% looks for two consecutive occurrences of Elem

search2(X, cons(X, cons(X, _))).
search2(X, cons(_, Xs)) :- search2(X, Xs).

% 1.3: search_two
% search_two(Elem, List)
% looks for two occurrences of Elem with any element in between !

search_two(X, cons(X, cons(_, cons(X, _)))).
search_two(X, cons(_, T)) :- search_two(X, T).

% search_anytwo
% search_anytwo (Elem , List )
% looks for any Elem that occurs two times , anywhere

search_anytwo(X, cons(X, T)) :- search(X, T).
search_anytwo(X, cons(_, T)) :- search_anytwo(X, T).

% 2: Informations of a list

% 2.1: size
% size (List , Size )
% Size will contain the number of elements in List, written using notation zero, s(zero), s(s(zero))...

size(nil, zero).
size(cons(_, T), s(X)) :- size(T, X).

% 2.2: sum_list
% sum_list(List, Sum)

add(zero, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

sum_list(nil, zero).
sum_list(cons(H, T), R) :- sum_list(T, P), add(H, P, R).

% 2.3: count
% count (List, Element, NOccurrences)
% it uses count (List, Element, NOccurrencesSoFar, NOccurrences)

count_tail(List, E, N) :- count_tail(List, E, zero, N).
count_tail(nil, E, N, N).
count_tail(cons(E, L), E, N, M) :- count_tail(L, E, s(N), M).
count_tail(cons(E, L), E2, N, M) :- E \= E2, count_tail(L, E2, N, M).

% Compact version
count(L, N) :- count(L, zero, N).
count(nil, N, N).
count(cons(_, T), A, N) :- count(T, s(A), N).

% 2.4: max
% max(List, Max)
% Max is the biggest element in List
% Suppose the list has at least one element

greater(s(_), zero).
greater(s(X), s(Y)) :- greater(X, Y).

max(nil, Max, Max).
max(cons(H, T), TempMax, Max) :-
		max(T, H, Max),
    greater(H, TempMax).
max(cons(H, T), TempMax, Max) :-
		max(T, TempMax, Max),
    greater(TempMax, H).
max(cons(H, T), Max) :- max(T, H, Max).

% 2.5: min-max
% Not implemented

% 3: Compare list

% 3.1: same
% same(List1, List2)
% are the two lists exactly the same?

same(nil, nil).
same(cons(H, T1), cons(H, T2)) :- same(T1, T2).

% 3.2: all_bigger
% all_bigger(List1, List2)
% all elements in List1 are bigger than those in List2, by 1

all_bigger(nil, nil).
all_bigger(cons(H1, T1), cons(H2, T2)) :-
	all_bigger(T1, T2),
	greater(H1, H2).

% 3.3: sublist
% sublist(List1, List2)
% List1 should contain elements all also in List2

sublist(nil, L2).
sublist(cons(H, T), L2) :-
	sublist(T, L2),
	search(H, L2).

% 4: creating lists

% 4.1: seq
% seq(N,E, List ) --> List is [E,E ,... ,E] with size N
% example : seq(s(s(s(zero))), a, cons(a, cons(a, cons(a, nil)))).

seq(zero, _, nil).
seq(s(N), E, cons(E, T)) :- seq(N, E, T).

% 4.2: seqR
% seqR(N, List)

seqR(zero, cons(zero, nil)).
seqR(s(H), cons(s(H), T)) :- seqR(H, T).

% 4.3: seqR2
% seqR2(N, List) --> is [0, 1, ..., N - 1]

last(nil, X, cons(X, nil)).
last(cons(H1, T1), X, cons(H2, T2)) :- last(T1, X, T2).

seqR2(zero, nil).
seqR2(s(N), R) :-
	seqR2(N, T),
	last(T, N, R).

% 5: List functions

% 5.1: map_1(List, Res)
% Map every Peano number N of the list in his successor s(N).
% Inputs =>	map_1(cons(zero, cons(s(zero), cons(s(s(zero)), nil))), X).
%						map_1(nil, X).

map_1(nil, nil).
map_1(cons(H, T), cons(s(H), R)) :- map_1(T, R).

% 5.2: filter_0(List, Res)
% Create a new list without "zero" Peano numbers.
% Inputs =>	filter_0(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), X).
%						filter_0(nil, X).

filter_0(nil,nil).
filter_0(cons(H, T),cons(H, R)):-
	H \= zero,
	filter_0(T, R).
filter_0(cons(zero, T), R) :-
	filter_0(T, R).

% 5.3: count_0(List, Res)
% Count the number of elements not "zero".
% Inputs =>	count_0(cons(s(zero), cons(zero, cons(s(s(zero)), nil))), N).
%						count_0(nil, N).

count_0(L, N) :-
	filter_0(L, R),
	count(R, N).

% 5.4: find_0(Elem, List)
% Return the first element of the list in filtered list (if filtered list has elements).
% Inputs =>	find_0(X, cons(zero, cons(s(zero), cons(s(s(zero)), nil)))).
%						find_0(X, nil).

find_0(N, L) :-
	filter_0(L, R),
	search(N, R).

% 5.5: take(N, List, Res)
% Take the first N elements from the list.
% Inputs => take(s(s(zero)), cons(a, cons(b, cons(c, nil))), X).
%						take(zero, cons(a, cons(b, cons(c, nil))), X).
%						take(s(s(zero)), nil, X).

take(zero, _, nil).
take(_, nil, nil).
take(s(N), cons(H, T), cons(H, R)) :- take(N, T, R).

% 5.6: drop(N, List, Res)
% Drop N elements from the list starting from the left.
% Inputs =>	drop(s(zero), cons(a, cons(b, cons(c, nil))), X).
%						drop(zero, cons(a, cons(b, cons(c, nil))), X).
%						drop(s(zero), nil, X).

drop(zero, L, L).
drop(_, nil, nil).
drop(s(N), cons(_, T), R) :- drop(N, T, R).

% 5.7: reversed(List, Res)
% Reverse the list.
% Input =>	reversed(cons(1, cons(2, cons(3, nil))), R).
%						reversed(nil, R).

reversed(L, R) :- reversed(L, nil, R).
reversed(nil, A, A).
reversed(cons(H, T), A, R) :- reversed(T, cons(H, A), R).

% 5.8: zip(List1, List2, Res)
% Zip the elements of two lists.
% Input => zip(cons(1, cons(2, nil)), cons(a, cons(b, nil)), R).

zip(nil, _, nil).
zip(_, nil, nil).
zip(cons(H1, T1), cons(H2, T2), cons(pair(H1, H2), R)) :- zip(T1, T2, R).

% 5.9: drop_while(List, Res)
% Drop all the element not equal to zero until finds the first zero (if it exists).
% Inputs: =>	drop_while(cons(s(zero), cons(s(s(zero)), cons(zero, cons(s(zero), nil)))), R).
%	     				drop_while(cons(s(zero), cons(s(zero), nil)), R). (All elements dropped)
%	     				drop_while(cons(zero, cons(s(zero), cons(zero, nil))), R). (No element dropped)

drop_while(nil, nil).
drop_while(cons(zero, T), cons(zero, T)).
drop_while(cons(H, T), R) :-
	H \= zero,
	drop_while(T, R).

% 5.10: drop_right(N, List, Res)
% Drop N elements from the right of the list.
% Inputs =>	drop_right(s(s(zero)), cons(a, cons(b, cons(c, cons(d, nil)))), R).
%	    			drop_right(s(s(s(s(zero)))), cons(a, cons(b, cons(c, cons(d, nil)))), R).

sub(N, zero, N).
sub(s(N), s(M), R) :- sub(N, M, R).

drop_right(N, L, R) :-
	size(L, S),
	sub(S, N, NTake),
	take(NTake, L, R).