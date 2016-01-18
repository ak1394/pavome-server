-module(moco_topic_transient, [ChannelId, Profile, TopicId, PurgeOnClose]).
-compile(export_all).
-include("moco.hrl").

push(Messages, Clients) ->
	[{TopicId, OldMessages, MaxReference}] = dets:lookup(topics, TopicId),
    %% only the messages with the Reference > than MaxReference will be added
    NewMessages = [M || M <- moco_topic:sort_messages(Messages), M#moco_message.reference > MaxReference],
	UpdatedNewMessages = [Message#moco_message{id=MessageId} || {Message, MessageId} <-
						lists:zip(NewMessages, lists:seq(length(OldMessages)+1, length(OldMessages) + length(NewMessages)))],
	dets:insert(topics, {TopicId, OldMessages ++ UpdatedNewMessages, max_reference(MaxReference, UpdatedNewMessages)}),
    moco_topic:notify(ChannelId, Clients, length(NewMessages), moco_topic:should_hush(NewMessages, Profile)).

message(MessageId) ->
	[{TopicId, Messages, _MaxReference}] = dets:lookup(topics, TopicId),
    lists:nth(MessageId, Messages).

messages(MaxMessages) ->
	[{TopicId, Messages, _MaxReference}] = dets:lookup(topics, TopicId),
	case MaxMessages < length(Messages) of
		true ->
			NumMessages = MaxMessages;
		false ->
			NumMessages = length(Messages)
	end,
	lists:sublist(Messages, 1 + length(Messages) - NumMessages, NumMessages).

size() ->
	[{TopicId, Messages, _MaxReference}] = dets:lookup(topics, TopicId),
    length(Messages).

last_message() ->
	[{TopicId, Messages, _MaxReference}] = dets:lookup(topics, TopicId),
    case Messages of
        [] ->
            undefined;
        _ ->
	        lists:last(Messages)
    end.

messages_before(MessageId, MaxMessages) ->
	[{TopicId, Messages, _MaxReference}] = dets:lookup(topics, TopicId),
    RealMessageId = if MessageId > length(Messages) -> length(Messages) + 1; true -> MessageId end,
    NumMessages = if RealMessageId > MaxMessages -> MaxMessages; true ->  RealMessageId - 1 end,
	lists:sublist(Messages, RealMessageId - NumMessages, NumMessages).

messages_after(MessageId, MaxMessages) ->
	[{TopicId, Messages, _MaxReference}] = dets:lookup(topics, TopicId),
	lists:sublist(Messages, 1 + MessageId, MaxMessages).	

close() ->
    case PurgeOnClose of
        true ->
	        dets:insert(topics, {TopicId, [], 0});
        _ ->
            pass
    end.

destroy() ->
    dets:delete(topics, TopicId).

max_reference(MaxReference, []) ->
    MaxReference;
max_reference(_MaxReference, NewMessages) ->
    Last = lists:last(NewMessages),
    Last#moco_message.reference.
