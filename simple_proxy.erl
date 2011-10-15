-module(simple_proxy).

-export([start/0]).

-export([wait_connect/1, handle/1]).

start() ->
	{ok, ListenSocket} = gen_tcp:listen(1500, [binary, {active, true}]),
	spawn(?MODULE, wait_connect, [ListenSocket]),
	ok.

wait_connect(ListenSocket) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, wait_connect, [ListenSocket]),
	handle(AcceptSocket).

handle(Socket) ->
	receive
		{tcp, Socket, Data} ->
			io:format("received:~p~n", [Data]),
			gen_tcp:send(Socket, <<"HTTP/1.1 501 Not Implemented\r\n">>);
		{tcp_error, Socket, Reason} ->
			io:format("error:~p~n", [Reason]);
		{tcp_closed, Socket} ->
			io:format("closed:~p~n", [Socket])
	end.
