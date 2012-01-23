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

%% gen_server exports
-export([init/1]).

%% interface functions
-export([
    event/2
]).

make_payment(OrderNumber, Context) ->
    Host = Context#context.host,
    HostName = m_config:get_value(site, hostname, Context),
    Language = m_config:get_value(?MODULE, paybox_language, 'fra', Context),

    %%% If any of Site, Identifier or Rang is undefiend we can't process the payment
    Site = m_config:get_value(?MODULE, paybox_site, undefined, Context),
    Identifier = m_config:get_value(?MODULE, paybox_identifier, undefined, Context),
    Rang = m_config:get_value(?MODULE, paybox_rang, undefined, Context),
    
    Order = m_paybox_order:get(OrderNumber, Context),
    io:format("Order: ~p~n", [Order]),
    {email, Email} = proplists:lookup(email, Order),
    {order_total, OrderTotal} = proplists:lookup(order_total, Order),
    Amount = integer_to_list(OrderTotal),
    OrderId = integer_to_list(OrderNumber),
    case lists:member(undefined, [Site, Identifier, Rang]) of
        true ->
            Context;
        false ->
            Currency = binary_to_list(m_config:get_value(?MODULE, paybox_currency, <<"978">>, Context)),
            ReturnUrl = binary_to_list(m_config:get_value(?MODULE, paybox_return_url, <<"/paybox/callback">>, Context)),
            SuccessUrl = binary_to_list(m_config:get_value(?MODULE, paybox_success_url, <<"/paybox/success">>, Context)),
            FailedUrl = binary_to_list(m_config:get_value(?MODULE, paybox_failed_url, <<"/paybox/failure">>, Context)),
            CancelledUrl =binary_to_list( m_config:get_value(?MODULE, paybox_cancelled_url, <<"/paybox/cancelled">>, Context)),
            
            Parameters = io_lib:format("PBX_MODE=4 PBX_SITE=~s PBX_RANG=~s PBX_IDENTIFIANT=~s PBX_DEVISE=~s "
                                       "PBX_PORTEUR='~s' PBX_CMD=~s PBX_TOTAL=~s "
                                       "PBX_RETOUR='amount:M;error:E;reference:R;transaction:T;status:O'; "
                                       "PBX_EFFECTUE='~s~s' PBX_REFUSE='~s~s' PBX_ANNULE='~s~s' "
                                       "PBX_LANGUE=~s PBX_REPONDRE_A='~s~s' ", [Site, Rang, Identifier, Currency, Email, OrderId, Amount, HostName, SuccessUrl, HostName, FailedUrl, HostName, CancelledUrl, Language, HostName, ReturnUrl]),
            Command = io_lib:format("~s ~s", [filename:join([z_utils:lib_dir(priv), "sites", Host, "deps", "modulev2.cgi"]), Parameters]),
            RedirectCode = list_to_binary(os:cmd(Command)),
            m_paybox_order:set_redirect_code(OrderNumber, RedirectCode, Context),
            io:format("Result: ~s~n", [RedirectCode]),
            z_render:wire({redirect, [{dispatch, paybox_redirect}, {order_number, OrderNumber}]}, Context)
    end.

event({submit, {make_payment, Args}, TriggerId, _TargetId}, Context) ->
    {order_number, OrderNumber} = proplists:lookup(order_number, Args),
    make_payment(OrderNumber, Context);


event({postback, {make_payment, Args}, TriggerId, _TargetId}, Context) ->
    {order_number, OrderNumber} = proplists:lookup(order_number, Args),
    make_payment(OrderNumber, Context);

event({submit, {create_order_example, Args}, TriggerId, _TargetId}, Context) ->
    Product = z_context:get_q_validated("product", Context),
    Email = z_context:get_q_validated("email", Context),
    case Product of
        "1" -> 
            case m_paybox_order:insert(Email, "10 Euro worth of stuff!", 1000, undefined, undefined, "", Context) of
                {ok, OrderId} ->
                    make_payment(OrderId, Context);
                {error, _} ->
                    Context
            end;
        "2" -> 
            case m_paybox_order:insert(Email, "20 Euro worth of stuff!", 2000, undefined, undefined, undefined, Context) of
                {ok, OrderId} ->
                    make_payment(OrderId, Context);
                {error, _} ->
                    Context
            end;
        _Others -> Context
    end.


%event({submit, {make_payment_new_order, Args}, TriggerId, _TargetId}, Context) ->
%    {amount, Amount} = proplists:lookup(amount, Args),
%    {order_number, OrderNumber} = proplists:lookup(order_number, Args),
%    {email, Email} = proplists:lookup(email, Args),
%    make_payment(Amount, OrderNumber, Email, Context);
%
%
%event({postback, {make_payment_new_order, Args}, TriggerId, _TargetId}, Context) ->
%    {amount, Amount} = proplists:lookup(amount, Args),
%    {order_description, OrderDescription} = proplists:lookup(order_number, Args),
%    {email, Email} = proplists:lookup(email, Args),
%    make_payment(Amount, OrderNumber, Email, Context).

%% @doc Check the installation of the paybox table.
%% in the default installer, this module installs a different table.
init(Context) ->
    ok = z_db:transaction(fun install1/1, Context),
    z_depcache:flush(Context),
    ok.
    
    install1(Context) ->
        ok = install_paybox_order_table(z_db:table_exists(paybox_order, Context), Context),
        ok.

install_paybox_order_table(true, Context) ->
    ok;
install_paybox_order_table(false, Context) ->
    z_db:q("
        create table paybox_order (
            id serial not null,
            user_id int,
            extra_info_rsc_id int,
            order_total int not null default 0,
            email character varying(80) not null default ''::character varying,
            shipping_address text not null default ''::character varying,
            order_description text not null default ''::character varying,
            paid boolean not null default 'f',
            redirect_page bytea,
            created timestamp with time zone not null default now(),
            
            constraint paybox_order_pkey primary key (id),
            constraint fk_paybox_order_user_id foreign key (user_id)
                references rsc(id)
                on delete set null on update cascade,
            constraint fk_paybox_order_extra_info_rsc_id foreign key (extra_info_rsc_id)
                references rsc(id)
                on delete set null on update cascade
        )
    ", Context),
    Indices = [
        {"fki_paybox_order_user_id", "user_id"},
        {"paybox_order_created_key", "created"}
    ],
    [ z_db:q("create index "++Name++" on paybox_order ("++Cols++")", Context) || {Name, Cols} <- Indices ],
    ok.