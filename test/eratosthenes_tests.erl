-module(eratosthenes_tests).
-compile([export_all, warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").

start() ->
    eratosthenes:start_link(1000).

stop(_) ->
    eratosthenes:stop().

setup_test_() ->
    {foreach, local,
        fun start/0,
        fun stop/1, [
            fun test_prime_numbers/1,
            fun test_no_prime_numbers/1
        ]
    }.

test_prime_numbers(_) ->
    Primes = [1,2,3,5,7,11,13,97],
    lists:all(fun(X) -> eratosthenes:is_prime(X) end, Primes).

test_no_prime_numbers(_) ->
    NoPrimes = [4,6,8,10,12,14,15,16,18,20,100,102,104,106,200],
    lists:all(fun(X) -> not eratosthenes:is_prime(X) end, NoPrimes).
