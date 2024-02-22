-module(game).
-export([canWin/1]).
-export([nextMove/1]).
-export([explanation/0]).

canWin(0) -> false;
canWin(1) -> true;
canWin(2) -> true;
canWin(3) -> false;
canWin(N) when N > 3, is_integer(N) ->
    not canWin(N - 1) or not canWin(N - 2).

nextMove(0) -> false;
nextMove(N) when is_integer(N) and N >= 0 ->
    case N of
        1 -> {true, 1};
        2 -> {true, 2};
        _ -> case nextMove(N - 1) of
            false -> {true, 1};
            {true, 1} -> {true, 2};
            {true, 2} -> false
        end
    end.

explanation() -> {"The definition of tail recursion is performing the recursive function call as the last operation in the function.
    This is so the traditional stack operations related to recursive functions calls are not called, and the associated memory overhead
    is optimized. With this question we are presented with a problem - we must perform two recursive function calls in order to return a
    value, therefore tail recursion is not possible in the basic implementation we were taught."}.