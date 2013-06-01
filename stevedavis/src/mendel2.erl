-module(mendel2).
% This module simulates breeding to calculate the outcome.
% See also mendel which uses combinatorial math.

-export([inherit/3, test/0]).

test() ->
	inherit(2,2,2).

inherit(K, M, N) ->
	Cohort = get_cohort(K, M, N),
	Pairs = get_pairs(Cohort,[]),
	Progeny = crossbreed_pairs(Pairs, []),
	DominantPhenotypes = get_dominant_phenotypes(Progeny),
	length(DominantPhenotypes) / length(Progeny).

get_cohort(K, M, N) ->
	L = lists:duplicate(K, {d, d}),
	L0 = L ++ lists:duplicate(M, {d, r}),
	L0 ++ lists:duplicate(N, {r, r}).

get_pairs([H|T], Acc) ->
	P = [{H, X} || X <- T],
	get_pairs(T, lists:append(Acc,P));
get_pairs([], Acc) ->
	Acc.

crossbreed_pairs([{{A0,A1}, {A2,A3}}|T], Acc) ->
	crossbreed_pairs(T, [{A0,A2}, {A0,A3}, {A1,A2}, {A1,A3}|Acc]);
crossbreed_pairs([], Acc) ->
	Acc.

get_dominant_phenotypes(Progeny) ->
	[X || X <- Progeny, X =/= {r, r}].
