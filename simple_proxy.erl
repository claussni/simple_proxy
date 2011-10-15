-module(simple_proxy).

-export([start/0, stop/0]).

-export([start/1, wait_connect/1]).

start() ->
	register(?MODULE, spawn(?MODULE, start, [1500])).

start(Port) ->
	{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
	spawn(?MODULE, wait_connect, [ListenSocket]),
	io:format("wait for stop signal~n"),
	receive
		{stop, From} ->
			From ! {ok, stopped}
	end.

stop() ->
	?MODULE ! {stop, self()},
	receive
		{ok, Return} -> Return
	end.
	

wait_connect(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, AcceptSocket} ->
			spawn(?MODULE, wait_connect, [ListenSocket]),
			receive
				{tcp, AcceptSocket, Data} ->
					io:format("received:~p~n", [Data]),
					gen_tcp:send(AcceptSocket, <<"HTTP/1.1 501 Not Implemented\r\n">>);
				{tcp_error, AcceptSocket, Reason} ->
					io:format("error:~p~n", [Reason]);
				{tcp_closed, AcceptSocket} ->
					io:format("closed:~p~n", [AcceptSocket]);
				_ -> ok
			end;
		{error, Reason} ->
			io:format("error:~p~n", [Reason])
	end.

