-module(mendel).

% This module uses math to calculate the outcome.
% See also mendel2 which simulates breeding.

-export([inherit/3, test/0]).

test() ->
	inherit(2, 2, 2).

inherit(K, M, N) ->
	Total = pairs(K + M + N),
	CK = pairs(K),
	CM = pairs(M),
	CN = pairs(N),
	CKM = pairs(K + M) - (CK + CM),
	CKN = pairs(K + N) - (CK + CN),
	CMN = pairs(M + N) - (CM + CN),
	Dominant = CK + CM * 0.75 + CKM + CKN + CMN * 0.5,
	Dominant / Total.

pairs(N) ->
	combinations(2, N).

combinations(R, N) ->
	factorial(N) / (factorial(R) * factorial(N - R)).

factorial(0) ->
	1;
factorial(N) when N > 0 ->
	N * factorial(N - 1).
