-record(moco_message, {kind, id, topic_id, origin, profile, reference, author, forwarded_by, author_id, recipient, irt_reference, irt_user, irt_user_id, posted, body, attachment, attached, favorited}).

-define(DB_TIMEOUT, 90000).
-define(TP_TIMEOUT, 30000).
