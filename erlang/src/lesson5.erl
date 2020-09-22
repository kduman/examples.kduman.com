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

-module(lesson5).
-export([map/2, foldr/3, foldl/3, reverse/1, map2/2, map3/2, filter/2, length/1,
    sum/1, product/1, max/1, sum_of_primes/1]).

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

foldr(_, Acc, []) -> Acc;
foldr(F, Acc, [H|T]) -> F(H, foldr(F, Acc, T)).

foldl(_, Acc, []) -> Acc;
foldl(F, Acc, [H|T]) -> foldl(F, F(H, Acc), T).

reverse(L) -> foldl(fun(X, Y) -> [X|Y] end, [], L).

map2(F, L) -> foldr(fun(X, Y) -> [F(X)|Y] end, [], L).

map3(F, L) -> reverse(foldl(fun(X, Y) -> [F(X)|Y] end, [], L)).

filter(P, L) -> foldr(
    fun(X, Y) ->
        case P(X) of
            true -> [X|Y];
            _ -> Y
        end
    end, [], L).

length(L) -> foldl(fun(_, Y) -> Y + 1 end, 0, L).

sum(L) -> foldl(fun erlang:'+'/2, 0, L).

product(L) -> foldl(fun erlang:'*'/2, 1, L).

max([H|T]) -> foldl(fun(X, Y) when X > Y -> X; (_, Y) -> Y end, H, T).

% BONUS: The Puzzle!

factors(N) -> filter(fun(X) -> N rem X =:= 0 end, lists:seq(1, N)).

is_prime(N) -> factors(N) =:= [1, N].

primes(N) -> filter(fun is_prime/1, lists:seq(1, N)).

sum_of_primes(N) -> sum(primes(N)).
