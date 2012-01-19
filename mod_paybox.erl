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

%% interface functions
-export([
    event/2
]).

event({submit, {make_payment, Args}, TriggerId, _TargetId}, Context) ->
    {amount, Amount} = proplists:lookup(amount, Args),
    Context.