-module(moco_topic).
-compile(export_all).
-include("moco.hrl").

init() ->
    file:delete("priv/topics.dets"),
    dets:open_file(topics, [{file, "priv/topics.dets"}]).

open(Channel) ->
    case proplists:get_value(id, Channel) of
        {search, _} ->
	        Topic = transient(Channel, false);
        {tweet, _} ->
	        Topic = transient(Channel, false);
        {default, favorites} ->
	        Topic = transient(Channel, true);
        {default, _} ->
	        Topic = persisted(Channel);
        {user, _} ->
	        Topic = transient(Channel, false)
    end,
    case Topic:last_message() of
        undefined ->
            LastStatusId = undefined;
        Message ->
            LastStatusId = Message#moco_message.reference
    end,
    [{topic, Topic}, {last_status_id, LastStatusId}] ++ Channel.

persisted(Channel) ->
    Id = {default, Name} = proplists:get_value(id, Channel),
    Profile = proplists:get_value(profile, Channel),
	TopicId = moco_db:topic_for_profile(proplists:get_value(profile_id, Profile), Name),
    moco_topic_persisted:new(Id, Profile, TopicId).

transient(Channel, PurgeOnClose) ->
    Id = proplists:get_value(id, Channel),
    Profile = proplists:get_value(profile, Channel),
	TopicId = now(),
	dets:insert(topics, {TopicId, [], 0}),
    moco_topic_transient:new(Id, Profile, TopicId, PurgeOnClose).

should_hush(Messages, Profile) ->
    Username = proplists:get_value(username, Profile),
    NotMyTweets = [Message || Message <- Messages, Message#moco_message.author /= Username],
    length(NotMyTweets) == 0.

notify(_Id, _Clients, 0, _ShouldHush) ->
    pass;
notify(Id, Clients, NewMessageNum, ShouldHush) ->
    io:format("nofity ~p~n", [[Id, Clients, NewMessageNum, ShouldHush]]),
    [Client ! {notify, Id, NewMessageNum, ShouldHush} || Client <- Clients].

sort_messages(Messages) ->
    lists:sort(fun(A, B) -> A#moco_message.reference =< B#moco_message.reference end, Messages).
