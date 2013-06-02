-module(rabbits).

-export([breed/2, test/0]).

test() ->
	breed(5, 3).

breed(N, K) when is_integer(N), is_integer(K) ->
	breed(N - 1, K, 0, 1).

breed(N, K, C, Cn) when N > 0 ->
	breed(N - 1, K, C + Cn, C * K);
breed(_, _K, C, Cn) ->
	C + Cn.