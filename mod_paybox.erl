%% @author Michael Connors <michael@bring42.net>
%% @copyright 2012 Michael Connors
%% @date 2012-01-19
%% @doc PayBox Module. Support PayBox payment processing in Zotonic.

%% Copyright 2012 Michael Connors
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_paybox).
-author("Michael Connors <michael@bring42.net>").

-mod_title("PayBox").
-mod_description("Integration with PayBox Payment Processing. For taking online payments in France.").

-include_lib("zotonic.hrl").

%% interface functions
-export([
    event/2
]).

event({submit, {make_payment, Args}, TriggerId, _TargetId}, Context) ->
    Host = Context#context.host,
    HostName = m_config:get_value(site, hostname, Context),
    Language = m_config:get_value(?MODULE, paybox_language, 'fra', Context),
    {amount, Amount} = proplists:lookup(amount, Args),
    {order_number, OrderNumber} = proplists:lookup(order_number, Args),
    {email, Email} = proplists:lookup(email, Args),

    %%% If any of Site, Identifier or Rang is undefiend we can't process the payment
    Site = m_config:get_value(?MODULE, paybox_site, undefined, Context),
    Identifier = m_config:get_value(?MODULE, paybox_identifier, undefined, Context),
    Rang = m_config:get_value(?MODULE, paybox_rang, undefined, Context),
    
    case lists:member(undefined, [Site, Identifier, Rang]) of
        true ->
            Context;
        false ->
            Currency = m_config:get_value(?MODULE, paybox_currency, 978, Context),
            ReturnUrl = m_config:get_value(?MODULE, paybox_return_url, '/paybox/callback', Context),
            SuccessUrl = m_config:get_value(?MODULE, paybox_success_url, '/paybox/success', Context),
            FailedUrl = m_config:get_value(?MODULE, paybox_failed_url, '/paybox/failure', Context),
            CancelledUrl = m_config:get_value(?MODULE, paybox_cancelled_url, '/paybox/cancelled', Context),
            
            Parameters = io_lib:format("PBX_MODE=4 PBX_SITE=~s PBX_RANG=~s PBX_IDENTIFIANT=~s PBX_DEVISE=~s "
                                       "PBX_PORTEUR='~s' PBX_CMD=~s PBX_TOTAL=~s "
                                       "PBX_RETOUR='amount:M;error:E;reference:R;transaction:T;status:O'; "
                                       "PBX_EFFECTUE='~s~s' PBX_REFUSE='~s~s' PBX_ANNULE='~s~s' "
                                       "PBX_LANGUE=FRA PBX_REPONDRE_A='~s~s' ", [Site, Rang, Identifier, Language, Email, OrderNumber, Amount, HostName, SuccessUrl, HostName, FailedUrl, HostName, CancelledUrl, HostName, ReturnUrl]),
            Command = io_lib:format("~s ~s", [filename:join([z_utils:lib_dir(priv), "sites", Host, "deps", "modulev2.cgi"]), Parameters]),
            Result = list_to_binary(os:cmd(Command)),
            io:format("Result: ~s~n", [Result]),
            %% find a way to render this page here
            Context
    end;

event({postback, {make_payment, Args}, TriggerId, _TargetId}, Context) ->
    {amount, Amount} = proplists:lookup(amount, Args),
    io:format("Amount: ~p~n", [Amount]),
    Context.