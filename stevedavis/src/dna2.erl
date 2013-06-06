-module(dna2).

% You'd likely store DNA data as 2 bits per nucleotide rather than character strings. 
% If that was the case, then the logic would be even simpler.

-export([count/1, test/0]).

test() ->
	Bin = encode("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"),
	count(Bin).

% Logic
count(B) when is_bitstring(B) ->
	count(B, {0, 0, 0, 0}).

count(<<X:2, B/bits>>, C) ->
	count(B, update(C, X + 1));
count(<<>>, Sum) ->
	Sum.

update(T, N) ->
	setelement(N, T, element(N, T) + 1).

% The rest is just to encode the test string into the binary input format
encode(String) ->
	encode(String, <<>>).
encode([H|T], Acc) ->
	encode(T, <<Acc/bits, (value(H)):2>>);
encode([], Acc) ->
	Acc.

value($A) -> 0;
value($C) -> 1;
value($G) -> 2;
value($T) -> 3.