% MIT License

% Copyright (c) 2020 Konstantin Duman

% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.

-module(lesson3).
-export([sum/1, sum2/1, len/1, dna_calc/1, cag_count/1, seq/1, concat/2,
    strip_abc/1, strip_abc2/1, reverse/1, verbose_insert/2, ins_sort/1]).

sum([]) -> 0;
sum(L) -> hd(L) + sum(tl(L)).

sum2([]) -> 0;
sum2([H|T]) -> H + sum2(T).

len([]) -> 0;
len([_|T]) -> 1 + len(T).

dna_calc([], A, T, G, C) ->
    {A, T, G, C, (G + C) / (A + T + G + C) * 100};
dna_calc([a|Tail], A, T, G, C) -> dna_calc(Tail, A + 1, T, G, C);
dna_calc([t|Tail], A, T, G, C) -> dna_calc(Tail, A, T + 1, G, C);
dna_calc([g|Tail], A, T, G, C) -> dna_calc(Tail, A, T, G + 1, C);
dna_calc([c|Tail], A, T, G, C) -> dna_calc(Tail, A, T, G, C + 1).

dna_calc(L) -> dna_calc(L, 0, 0, 0, 0).

cag_count([]) -> 0;
cag_count([c, a, g|T]) -> 1 + cag_count(T);
cag_count([_, _, _|T]) -> cag_count(T).

seq(0) -> [];
seq(N) -> [N|seq(N - 1)].

concat([], B) -> B;
concat([H|T], B) -> [H|concat(T, B)].

strip_abc("abc" ++ T) -> T;
strip_abc(L) -> L.

strip_abc2([$a, $b, $c|T]) -> T;
strip_abc2(L) -> L.

reverse([], Acc) -> Acc;
reverse([H|T], Acc) -> reverse(T, [H|Acc]).

reverse(L) -> reverse(L, []).

verbose_insert(X, []) -> [X];
verbose_insert(X, [H|T] = L) ->
    if
        X < H -> [X|L];
        true -> [H|verbose_insert(X, T)]
    end.

insert(X, []) -> [X];
insert(X, [H|_] = L) when X < H -> [X|L];
insert(X, [H|T]) -> [H|insert(X, T)].

ins_sort([]) -> [];
ins_sort([H|T]) -> insert(H, ins_sort(T)).
