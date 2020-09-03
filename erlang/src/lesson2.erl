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

-module(lesson2).
-export([loop/1, add/2, mul/2, pow/2, tet/2, hyper/3]).

loop(0) -> done;
loop(N) ->
    io:fwrite("N = ~w~n", [N]),
    loop(N - 1).

succ(X) -> X + 1.
pred(X) -> X - 1.

add(A, 0) -> A;
add(A, B) -> succ(add(A, pred(B))).

mul(_, 0) -> 0;
mul(A, B) -> add(A, mul(A, pred(B))).

pow(_, 0) -> 1;
pow(A, B) -> mul(A, pow(A, pred(B))).

tet(_, 0) -> 1;
tet(A, B) -> pow(A, tet(A, pred(B))).

hyper(0, _, B) -> B + 1;
hyper(1, A, 0) -> A;
hyper(2, _, 0) -> 0;
hyper(_, _, 0) -> 1;
hyper(N, A, B) -> hyper(N - 1, A, hyper(N, A, B - 1)).
