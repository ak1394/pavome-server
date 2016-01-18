%% Author: anton
%% Created: 29 Sep 2009
%% Description: TODO: Add description to moco_gapi
-module(moco_gapi).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([detect/1]).

%%
%% API Functions
%%
%%	Url = "http://ajax.googleapis.com/ajax/services/language/detect?v=1.0&q=%D0%BF%D1%80%D0%B8%D0%B2%D0%B5%D1%82",

detect(Text) when is_list(Text) ->
    detect(unicode:characters_to_binary(Text));

detect(Text) when is_binary(Text) ->
    Encoded = yaws_api:url_encode(binary_to_list(Text)),
	Url = "http://ajax.googleapis.com/ajax/services/language/detect?v=1.0&q=" ++ Encoded,
	{ok, {_Status, _Headers, Body}} = http:request(get, {Url, [{"referer", "http://pavo.me"}]}, [], []),
	{struct, Response} = mochijson2:decode(Body),
	{struct, Data} = proplists:get_value(<<"responseData">>, Response),
	{proplists:get_value(<<"language">>, Data), 
		proplists:get_value(<<"isReliable">>, Data),
		proplists:get_value(<<"confidence">>, Data)}.
