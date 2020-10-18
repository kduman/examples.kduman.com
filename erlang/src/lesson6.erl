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

-module(lesson6).
-export([abs/1, double/1, const/1, make_adder/1,
    pair_naive/2, pair/2, first/1, second/1, sum/1, pair_complex/2,
    cons/2, head/1, tail/1, nil/0, is_empty/1,
    foldl/3, reverse/1, from_erlang_list/1, to_erlang_list/1, do/2,
    concat/2, filter/2, qsort/1]).

abs(X) when X < 0 -> -X;
abs(X) -> X.

double(X) ->
    F = fun(X) -> X + X end,
    F(X).

const(X) -> fun() -> X end.

make_adder(X) -> fun(Y) -> X + Y end.

pair_naive(X, Y) ->
    fun(Idx) ->
        case Idx of
            0 -> X;
            1 -> Y
        end
    end.

pair(X, Y) -> fun(F) -> F(X, Y) end.
first(P) -> P(fun(X, _) -> X end).
second(P) -> P(fun(_, Y) -> Y end).

sum(P) -> P(fun erlang:'+'/2).

pair_complex(X, Y) ->
    Sum = X + Y,
    Product = X * Y,
    fun(Msg) ->
        case Msg of
            x -> X;
            y -> Y;
            sum -> Sum;
            product -> Product;
            map -> fun(F) -> F(X, Y) end;
            print -> io:fwrite("(~w . ~w)~n", [X, Y])
        end
    end.

cons(H, T) -> pair(H, T).
head(L) -> first(L).
tail(L) -> second(L).

nil() -> fun(_) -> true end.
is_empty(L) -> L(fun(_, _) -> false end).

foldl(F, Acc, L) ->
    case is_empty(L) of
        true -> Acc;
        _ -> foldl(F, F(head(L), Acc), tail(L))
    end.

reverse(L) -> foldl(fun cons/2, nil(), L).

from_erlang_list(L) -> reverse(lists:foldl(fun cons/2, nil(), L)).

to_erlang_list(L) -> lists:reverse(foldl(fun(H, T) -> [H|T] end, [], L)).

do(F, L) -> to_erlang_list(F(from_erlang_list(L))).

concat(A, B) -> foldl(fun cons/2, B, reverse(A)).

filter(P, L) ->
    reverse(foldl(fun(H, T) ->
        case P(H) of
            true -> cons(H, T);
            _ -> T
        end
    end, nil(), L)).

qsort(L) ->
    case is_empty(L) of
        true -> L;
        _ ->
            Pivot = head(L),
            S = fun(F) -> qsort(filter(fun(X) -> F(X, Pivot) end, tail(L))) end,
            concat(S(fun erlang:'<'/2), cons(Pivot, S(fun erlang:'>='/2)))
    end.
