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

-module(lesson4).
-export([is_even/1, is_odd/1, is_even2/1, is_even3/1, filter_even/1, filter_odd/1, filter/2,
    filter_even_simple/1, filter_odd_simple/1, any/2, all/2, zip/2, is_sorted/1]).

is_even(0) -> true;
is_even(N) -> is_odd(N - 1).

is_odd(0) -> false;
is_odd(N) -> is_even(N - 1).

is_even2(N) -> N rem 2 =:= 0.

is_even3(N) -> 1 band N =:= 0.

filter_even([]) -> [];
filter_even([H|T]) ->
    case is_even(H) of
        true -> [H|filter_even(T)];
        false -> filter_even(T)
    end.

filter_odd([]) -> [];
filter_odd([H|T]) ->
    case is_odd(H) of
        true -> [H|filter_odd(T)];
        false -> filter_odd(T)
    end.

filter(_, []) -> [];
filter(P, [H|T]) ->
    case P(H) of
        true -> [H|filter(P, T)];
        false -> filter(P, T)
    end.

filter_even_simple(L) -> filter(fun is_even/1, L).

filter_odd_simple(L) -> filter(fun is_odd/1, L).

any(_, []) -> false;
any(P, [H|T]) -> P(H) orelse any(P, T).

all(_, []) -> true;
all(P, [H|T]) -> P(H) andalso all(P, T).

% differs from lists:zip!
zip([HX|TX], [HY|TY]) -> [{HX, HY}|zip(TX, TY)];
zip(_, _) -> [].

gte({X, Y}) -> X =< Y.

lte({X, Y}) -> X >= Y.

is_sorted([]) -> true;
is_sorted([_|T] = L) ->
    Pairs = zip(L, T),
    all(fun gte/1, Pairs) orelse
    all(fun lte/1, Pairs).
