-define(PACKET_OK, 16#0a).
-define(PACKET_ERROR, 16#0b).
-define(PACKET_ERROR_MESSAGE, 16#0c).
-define(PACKET_POST, 16#0d).
-define(PACKET_LOGIN, 16#0e).
-define(PACKET_PARAMS, 16#0f).
-define(PACKET_ACK, 16#10).
-define(PACKET_SETTINGS, 16#11).
-define(PACKET_AVATAR_REQUEST, 16#12).
-define(PACKET_GET_ATTACHMENT2, 16#13).
-define(PACKET_PING, 16#14).
-define(PACKET_PONG, 16#15).
-define(PACKET_IMAGE, 16#16).
-define(PACKET_PREVIEW_IMAGE, 16#17).
-define(PACKET_TOPIC_OPEN, 16#18).
-define(PACKET_TOPIC_PAGE, 16#19).
-define(PACKET_TOPIC_MESSAGE, 16#1a).
-define(PACKET_TOPIC_CLOSE, 16#1b).
-define(PACKET_GET_ATTACHMENT, 16#1c).
-define(PACKET_DELETE_PREVIEW, 16#1d).
-define(PACKET_CHECK_REQUEST, 16#1e).
-define(PACKET_CHECK_RESULT, 16#1f).
-define(PACKET_TOPIC_PAGE_BEFORE, 16#20).
-define(PACKET_TOPIC_PAGE_AFTER, 16#21).
-define(PACKET_BLUETOOTH_DEVICES, 16#22).
-define(PACKET_BLUETOOTH_LOCAL, 16#23).
-define(PACKET_EXCEPTION, 16#24).
-define(PACKET_STREAM_CHUNK, 16#25).
-define(PACKET_STREAM_END, 16#26).
-define(PACKET_DELETE, 16#27).
-define(PACKET_USER_PROFILE, 16#28).
-define(PACKET_FOLLOW, 16#29).
-define(PACKET_UNFOLLOW, 16#30).
-define(PACKET_RE_TWEET, 16#3a).
-define(PACKET_TOPIC_OPEN2, 16#3b).
-define(PACKET_TOPIC_PAGE_BEFORE2, 16#3c).
-define(PACKET_TOPIC_PAGE_AFTER2, 16#3d).
-define(PACKET_DELETE2, 16#3e).
-define(PACKET_CREATE_FAVORITE, 16#3f).
-define(PACKET_DESTROY_FAVORITE, 16#40).
-define(PACKET_PREVIEW, 16#41).
-define(PACKET_STATUS_REQUEST2, 16#42).
-define(PACKET_TWEETPHOTO, 16#43).
-define(PACKET_REQUEST_TOKEN, 16#44).
-define(PACKET_REQUEST_URL, 16#45).

-define(PARAM_INT, 16#0a).
-define(PARAM_STR, 16#0b).
-define(PARAM_BIN, 16#0c).
-define(PARAM_IMG, 16#0d).
-define(PARAM_INT_LIST, 16#0e).
-define(PARAM_UNDEFINED, 16#0f).
-define(PARAM_STR_LIST, 16#10).
-define(PARAM_BIN_LIST, 16#11).
-define(PARAM_LONG, 16#12).
-define(PARAM_LONG_LIST, 16#13).
-define(PARAM_DATETIME, 16#14).
-define(PARAM_DATETIME_LIST, 16#15).
-define(PARAM_BOOL, 16#16).
-define(PARAM_BOOL_LIST, 16#17).
-define(PARAM_PARAMS, 16#18).

-define(TEST_SYSTEM_PROPERTY, 16#01).
-define(TEST_MIDLET_PROPERTY, 16#02).
-define(TEST_CLASS, 16#03).
-define(TEST_PERMISSION, 16#04).
-define(TEST_MEMORY, 16#05).
-define(TEST_LWUIT, 16#06).
-define(TEST_PLATFORM_REQUEST, 16#07).
-define(TEST_PLATFORM_REQUEST_FATAL, 16#08).
-define(TEST_DIALOG_INFO, 16#09).
-define(TEST_DIALOG_CONFIRM, 16#0a).
-define(TEST_PLATFORM_PROPERTY, 16#0b).

-define(TWEETPHOTO_FAVORITE, 1).
-define(TWEETPHOTO_UNFAVORITE, 2).
-define(TWEETPHOTO_UPVOTE, 3).
-define(TWEETPHOTO_DOWNVOTE, 4).
-define(TWEETPHOTO_COMMENT, 5).
