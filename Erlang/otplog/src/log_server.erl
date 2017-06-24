-module(log_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, log/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {handle}).

%%% ----------------------------------------------------------------------------
%%% -- API
%%% ----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:stop(?SERVER),
	ok.

log(Msg) ->
	gen_server:cast(?SERVER, {log, Msg}),
	ok.


%%% ----------------------------------------------------------------------------
%%% -- gen_server callbacks
%%% ----------------------------------------------------------------------------

init([]) ->
	case file:open('otplog.log', [append]) of
		{ok, File} ->
			{ok, #state{handle=File}};
		{error, Reason} ->
			exit({log_server,failed,to,open,file,Reason})
	end.

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast({log, Msg}, #state{handle=File} = _State) ->
	io:format("Logging: ~p~n", [Msg]),
	NewLine = 10,
	ToLog = lists:append(Msg, [NewLine]),
	case file:write(File, ToLog) of
		ok ->
			{noreply, _State};
		{error, Reason} ->
			exit({log,server,write,failed,Reason})
	end;
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, #state{handle=File} = _State) ->
	file:close(File),
	io:format("Terminated with reason ~p~n", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
