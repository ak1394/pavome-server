dojo.provide("moco.Server");

dojo.declare("moco.Server", null, {
    isPolling: false,
    currentPoll: null,
    currentTopic: null,
    currentUser: null,

    constructor: function() {
    },

    twitterRequestToken: function() {
        return dojo.xhrGet({url: "/api/twitter/request_token", handleAs: "json-comment-optional"}).addCallback(dojo.hitch(this, this.processResults));
    },

    jadToken: function() {
        return dojo.xhrGet({url: "/api/jad_token", handleAs: "json-comment-optional"}).addCallback(dojo.hitch(this, this.processResults));
    },

    downloadPin: function() {
        return dojo.xhrGet({url: "/api/download_pin", handleAs: "json-comment-optional"}).addCallback(dojo.hitch(this, this.processResults));
    },

    followUs: function() {
        return dojo.xhrGet({url: "/api/follow_us", handleAs: "json-comment-optional"}).addCallback(dojo.hitch(this, this.processResults));
    },

    sendJadSms: function(mobile) {
        return dojo.xhrGet({url: "/api/send_jad_sms", content: {'mobile': mobile}, handleAs: "json-comment-optional"}).addCallback(dojo.hitch(this, this.processResults));
    },

	contacts: function(username) {
		 return this.apiContacts(username).addCallback(dojo.hitch(this, this.gotContacts));
	},
	
	topic: function(username, name) {
		return this.apiReadTopic(username, name).addCallback(dojo.hitch(this, this.processResults)).addCallback(dojo.hitch(this, this.gotMessages)); 
	},

	topicBefore: function(username, name, message_id) {
		return this.apiReadTopicBefore(username, name, message_id).addCallback(dojo.hitch(this, this.processResults));
	},
	
	processResults: function(result) {
		if(result.result) {
			return result.result;
		} else {
			return new Error();
		}
	},

    _connect: function() {
        return this.apiSessionStart().addCallback(dojo.hitch(this, this.gotSession)).addCallback(dojo.hitch(this, function() {
            this.apiContacts().addCallback(dojo.hitch(this, function(contacts) {
                this.gotContacts(contacts);
                this.apiReadTopic("default").addCallback(dojo.hitch(this, function(messages) {
                    this.gotMessages(messages);
                    if(this.connectCallback) {
                        this.connectCallback();
                    }
                }));
            }));
        }));
    },

    startPolling: function() {
        this.isPolling = true;
        this.currentPoll = this.apiPoll().addCallbacks(dojo.hitch(this, this.gotEvent), dojo.hitch(this, this.errorEvent));
    },

    stop: function() {
        console.log("stop");
        this.isPolling = false;
        if(this.currentPoll) {
            this.currentPoll.cancel();
            this.currentPoll = null;
        }
    },

    username: function() {
        return this.contacts.username;
    },

    follow: function(username) {
        return this.apiFollow(username);
    },

    unfollow: function(username) {
        return this.apiUnFollow(username);
    },

    gotEvent: function(event) {
        console.log("event", event.result);
        if(event.result) {
		    dojo.publish("blah", event.result);
        }
        if(this.isPolling) {
			console.log("poll more");
            this.apiPoll().addCallbacks(dojo.hitch(this, this.gotEvent), dojo.hitch(this, this.errorEvent));
        }
    },

    errorEvent: function(err) {
        if(this.isPolling) {
            this.apiPoll().addCallbacks(dojo.hitch(this, this.gotEvent), dojo.hitch(this, this.errorEvent));
        }
    },

    post: function(topic, message) {
        this.apiPost(topic, dojo.toJson(message));
    },

    remove: function(topic, message) {
        this.apiDelete(topic, message);
    },

    onConnect: function(callback) {
        this.connectCallback = callback;
    },

    gotContacts: function(contacts) {
        this.contacts = contacts.result;
    },

    gotSession: function(result) {
        this.session = result.result.session_id;
    },

    gotMessages: function(messages) {
        this.messages = messages;
    },

    apiSessionStart: function() {
        return dojo.xhrPost({url: "/api/session_start", handleAs: "json-comment-optional", encoding: "utf-8", preventCache:true});
    },

    apiContacts: function(username) {
        var params = {session_id: this.session};
        if(username) {
            params.username = username;    
        }
        return dojo.xhrGet({url: "/api/contacts", content: params, handleAs: "json-comment-optional", encoding: "utf-8", preventCache:true});
    },

    apiReadTopic: function(username, name) {
        this.currentTopic = name;
        var params = {session_id: this.session};
        if(username) {
            params.username = username;
        }
        return dojo.xhrGet({url: "/api/topics/" + name, content: params, handleAs: "json-comment-optional", preventCache:true});
    },

    apiReadTopicBefore: function(username, name, message_id) {
        this.currentTopic = name;
        var params = {session_id: this.session, before: message_id};
        if(username) {
            params.username = username;
        }
        return dojo.xhrGet({url: "/api/topics/" + name, content: params, handleAs: "json-comment-optional", preventCache:true});
    },

    apiPost: function(topic, message) {
        return dojo.xhrPost({url: "/api/topics/" + topic + "?session_id=" + this.session, postData:message, handleAs: "json-comment-optional"});
    },


    apiUnFollow: function(username) {
        return dojo.xhrGet({url: "/api/unfollow", content: {session_id:this.session, username:username}, handleAs: "json-comment-optional"});
    },

    apiDelete: function(topic, message) {
        return dojo.xhrDelete({url: "/api/topics/" + topic, content: {session_id:this.session, id:message.id}, handleAs: "json-comment-optional"});
    },

    apiPoll: function() {
        return dojo.xhrGet({url: "/api/listen", content: {session_id: this.session}, handleAs: "json-comment-optional", preventCache:true});
    },

    end: function() {
    }
});
