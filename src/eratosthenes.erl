-module(eratosthenes).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/1,
    stop/0,
    is_prime/1
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%% API

start_link(N) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [N], []).

stop() ->
    gen_server:call(?MODULE, stop).

is_prime(N) ->
    gen_server:call(?MODULE, {prime, N}).

%% gen_server callbacks

init([N]) when is_integer(N) andalso N >= 1 ->
    init(1, N+1),
    {ok, sieve(2, N+1)}.

handle_call({prime, N}, _From, Primes) ->
    {reply, lists:member(N, Primes), Primes};
handle_call(stop, _From, Primes) ->
    {stop, normal, ok, Primes}.

handle_cast(_Msg, Primes) ->
    {noreply, Primes}.

handle_info(_Info, Primes) ->
    {noreply, Primes}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% internal functions

init(Max,Max) ->
    ok;
init(N,Max) ->
    put(N, true),
    init(N+1,Max).

sieve(I, Max) ->
    sieve(I, Max, trunc(math:sqrt(Max))).

sieve(I, _, SqrtMax) when I > SqrtMax ->
    Primes = lists:sort(orddict:fetch_keys(erlang:get())),
    erlang:erase(),
    Primes;
sieve(I, Max, SqrtMax) ->
    case erlang:get(I) of
        true ->
            remove_mul(I, I, Max),
            sieve(I+1, Max, SqrtMax);
        _ ->
            sieve(I+1, Max, SqrtMax)
    end.

remove_mul(I, N, Max) when I*N > Max ->
    ok;
remove_mul(I, N, Max) ->
    erase(I*N),
    remove_mul(I, N+1, Max).
