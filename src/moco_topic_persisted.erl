-module(moco_topic_persisted, [ChannelId, Profile, TopicId]).
-compile(export_all).

push(Messages, Clients) ->
    SortedMessages = moco_topic:sort_messages(Messages),
    SavedMessages = [Message || Message <- [moco_db:save_message2(TopicId, Message) || Message <- SortedMessages], Message /= undefined],
    ShouldHush = moco_topic:should_hush(SavedMessages, Profile),
    moco_topic:notify(ChannelId, Clients, length(SavedMessages), ShouldHush).


message(MessageId) ->
	moco_db:topic_message(TopicId, MessageId).

messages(MaxMessages) ->
	moco_db:topic_messages(TopicId, MaxMessages).

last_message() ->
    moco_db:topic_last_message_by_origin(TopicId, proplists:get_value(origin, Profile), proplists:get_value(profile_id, Profile)).

size() ->
	moco_db:topic_size(TopicId).

messages_before(MessageId, MaxMessages) ->
	moco_db:topic_messages_before(TopicId, MaxMessages, MessageId).

messages_after(MessageId, MaxMessages) ->
	moco_db:topic_messages_after(TopicId, MaxMessages, MessageId).

close() -> ok.

destroy() -> ok.
