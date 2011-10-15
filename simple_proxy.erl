-module(simple_proxy).

-compile(export_all).

start() ->
	register(?MODULE, spawn(?MODULE, proxy_start, [1500])),
	ok.

stop() ->
	try
		?MODULE ! {stop, self()},
		receive
			{ok, stopped} -> ok;
			_Other -> {error, unknown_response}
		end
	catch
		error:badarg -> {error, {?MODULE, not_started}}
	end.

proxy_start(Port) ->
	{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
	inets:start(),
	spawn_link(?MODULE, proxy_await_connect, [ListenSocket]),
	receive
		{stop, From} ->
			inets:stop(),
			From ! {ok, stopped};
		_Other -> {error, unknown_message}
	end.


proxy_await_connect(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, AcceptSocket} ->
			spawn(?MODULE, proxy_await_connect, [ListenSocket]),
			receive
				{tcp, AcceptSocket, Data} ->
					Result = proxy_perform_request(Data),
					gen_tcp:send(AcceptSocket, Result);
				{tcp_error, AcceptSocket, Reason} ->
					io:format("error:~p~n", [Reason]);
				{tcp_closed, AcceptSocket} ->
					io:format("closed:~p~n", [AcceptSocket]);
				_ -> ok
			end;
		{error, Reason} ->
			exit(Reason)
	end.

proxy_perform_request(Request) ->
	Url = extract_target_url(Request),
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
	      httpc:request(Url),
	Body.
	

extract_target_url(Request) ->
	[Req, _] = binary:split(Request, <<"HTTP">>),
	[_, Url] = binary:split(Req, <<" ">>),
	binary_to_list(Url).

