% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_log).
-behaviour(gen_server).

-export([start_link/0, debug/2, info/2, notice/2, warning/2, error/2,
        critical/2, alert/2, emergency/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

debug(Fmt, Args) ->
    gen_server:cast(?MODULE, {log, debug, Fmt, Args}).

info(Fmt, Args) ->
    gen_server:cast(?MODULE, {log, info, Fmt, Args}).

notice(Fmt, Args) ->
    gen_server:cast(?MODULE, {log, notice, Fmt, Args}).

warning(Fmt, Args) ->
    gen_server:cast(?MODULE, {log, warning, Fmt, Args}).

error(Fmt, Args) ->
    gen_server:cast(?MODULE, {log, 'error', Fmt, Args}).

critical(Fmt, Args) ->
    gen_server:cast(?MODULE, {log, critical, Fmt, Args}).

alert(Fmt, Args) ->
    gen_server:cast(?MODULE, {log, alert, Fmt, Args}).

emergency(Fmt, Args) ->
    gen_server:cast(?MODULE, {log, emergency, Fmt, Args}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% erlang:apply does not work as lager uses parse transforms
handle_cast({log, debug, Fmt, Args}, State) ->
    catch couch_stats:increment_counter([couch_log, level, debug]),
    lager:debug(Fmt, Args),
    {noreply, State};
handle_cast({log, info, Fmt, Args}, State) ->
    catch couch_stats:increment_counter([couch_log, level, info]),
    lager:info(Fmt, Args),
    {noreply, State};
handle_cast({log, notice, Fmt, Args}, State) ->
    catch couch_stats:increment_counter([couch_log, level, notice]),
    lager:notice(Fmt, Args),
    {noreply, State};
handle_cast({log, warning, Fmt, Args}, State) ->
    catch couch_stats:increment_counter([couch_log, level, warning]),
    lager:warning(Fmt, Args),
    {noreply, State};
handle_cast({log, 'error', Fmt, Args}, State) ->
    catch couch_stats:increment_counter([couch_log, level, 'error']),
    lager:error(Fmt, Args),
    {noreply, State};
handle_cast({log, critical, Fmt, Args}, State) ->
    catch couch_stats:increment_counter([couch_log, level, critical]),
    lager:critical(Fmt, Args),
    {noreply, State};
handle_cast({log, alert, Fmt, Args}, State) ->
    catch couch_stats:increment_counter([couch_log, level, alert]),
    lager:alert(Fmt, Args),
    {noreply, State};
handle_cast({log, emergency, Fmt, Args}, State) ->
    catch couch_stats:increment_counter([couch_log, level, emergency]),
    lager:emergency(Fmt, Args),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
