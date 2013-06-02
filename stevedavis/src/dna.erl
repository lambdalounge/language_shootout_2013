-module(dna).

-export([count/1, test/0]).

test() ->
	T = count("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"),
	format(T).

count(S) when is_list(S) ->
	count(S, {0, 0, 0, 0}).

count([$A|T], C) ->
	count(T, update(C, 1));
count([$C|T], C) ->
	count(T, update(C, 2));
count([$G|T], C) ->
	count(T, update(C, 3));
count([$T|T], C) ->
	count(T, update(C, 4));
count([], Sum) ->
	Sum.

update(T, N) ->
	setelement(N, T, element(N, T) + 1).

% Only really needed to meet the criterion that the output
% should be a space separated string of the totals
format({A, C, G, T}) ->
	lists:flatten([
		integer_to_list(A), $ ,
		integer_to_list(C), $ ,
		integer_to_list(G), $ ,
		integer_to_list(T)
		]).