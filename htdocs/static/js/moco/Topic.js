dojo.provide("moco.Topic");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dojox.av.FLVideo");
dojo.require("dojox.image.LightboxNano");
dojo.require("dijit.form.Button");
dojo.require("moco.util");
dojo.require("dojox.embed.Flash");

dojo.declare("moco.Topic", [dijit._Widget, dijit._Templated],
    {
  	templatePath: dojo.moduleUrl("moco", "Topic.html"),
    widgetsInTemplate: true,

	avatarTmpl: '<div class="avatar"><img src="/api/avatar?username=${author}"/></div>',
	bodyTmpl: '<div class="body"><a href="/${author}">${author}</a> ${body}</div>',
	attachmentTmpl: '<div class="attachment"><img src="/attachment/thumbnail/${id}.jpg"/></div>',
	detailTmpl: '<div class="detail">${detail}</div>',
	actionsTmpl: '<div class="actions"><div style="display: none" class="action-reply">&nbsp;</div><div style="display: none" class="action-delete">&nbsp;</div><div style="display: none" class="action-favorite">&nbsp;</div></div>',
    actionConnects: [],
	
	postCreate: function() {
		this.bodyOnly = this.avatarTmpl + this.actionsTmpl + this.bodyTmpl + this.detailTmpl,
		this.bodyAttachment = '<div class="content">' + this.actionsTmpl + this.avatarTmpl + this.attachmentTmpl + this.bodyTmpl + '</div>' + this.detailTmpl,
		this.attachmentOnly = '<div class="content">' + this.actionsTmpl + this.attachmentTmpl + '</div>';
		this.messageGroupTmpl = this.avatarTmpl + '<a href="/${author}">${author}</a>';

        if(this.messages.length > 0) {
            this.lastMessage = this.messages[0];
        }

		dojo.forEach(this.messages, function(message) {
			this.insertNewMessage(message, this.startDiv, "after");
		}, this);

		dojo.subscribe("blah", this, this.onNewMessage);
        dojo.connect(this.olderPosts, 'onClick', this, 'loadOlder');
	},
	
	onNewMessage: function(message) {
		console.log("message", message);
	    this.insertNewMessage(message, this.startDiv, "after");
	},
	
	insertNewMessage: function(message, div, position) {
		message.detail = moco.util.formatTimestamp(message.posted);
		if(message.body != "" && !message.attached) {
			var message_div = dojo.create("div", {"class":"message", id:'message-id-'+message.id}, div, position);
			dojo.place(dojo.string.substitute(this.bodyOnly, message), message_div);
			this.group = null;
		} else if(message.body != "" && message.attached) {
			var message_div = dojo.create("div", {"class":"message", id:'message-id-'+message.id}, div, position);
			dojo.place(dojo.string.substitute(this.bodyAttachment, message), message_div);
			this.group = null;
			dojo.connect(message_div, 'onclick', this, dojo.partial(this.zoom, message.id, message.attached));
		} else if(message.body == "" && message.attached) {
		    if(this.group == null) {
			    this.group = dojo.create("div", {"class":"message-group"}, div, position);
				dojo.place(dojo.string.substitute(this.messageGroupTmpl, message), this.group);
		    }
			var message_div = dojo.create("div", {"class":"picture-message", id:'message-id-'+message.id}, this.group);
			dojo.place(dojo.string.substitute(this.attachmentOnly, message), message_div);
			dojo.connect(message_div, 'onclick', this, dojo.partial(this.zoom, message.id, message.attached));				
		}		

        if(message_div) {
            dojo.connect(message_div, "onmouseenter", this, dojo.partial(this.mouseEnterMessage, message_div, message));
            dojo.connect(message_div, "onmouseleave", this, dojo.partial(this.mouseLeaveMessage, message_div, message));
        }
	},

    mouseEnterMessage: function(div, message) {
        dojo.addClass(div, "hover");
        if(message.author != mocoserver.username()) {
            dojo.query("div.action-reply", div).forEach(function(a) {
                dojo.style(a, "display", "block");
                this.actionConnects.push(dojo.connect(a, "onclick", this, dojo.partial(this.reply, message)));
            }, this);
        } else {
            dojo.query("div.action-delete", div).forEach(function(a) {
                dojo.style(a, "display", "block");
                this.actionConnects.push(dojo.connect(a, "onclick", this, dojo.partial(this.remove, div, message)));
            }, this);
        }
    },

    loadOlder: function() {
        mocoserver.topicBefore(mocoserver.username(), this.topic_name, this.lastMessage.id).addCallback(dojo.hitch(this, function(messages) {
            messages.reverse();
		    dojo.forEach(messages, function(message) {
			    this.insertNewMessage(message, this.endDiv, "before");
		    }, this);
        }));
    },

    mouseLeaveMessage: function(div, message) {
        dojo.removeClass(div, "hover");
        dojo.query(".action-reply", div).style("display", "none");
        dojo.query(".action-delete", div).style("display", "none");
        dojo.forEach(this.actionConnects, function(c) {
            dojo.disconnect(c);
        });
    },
	
    reply: function(message) {
        dijit.byId('new-post').setInReplyToMessage(message);
    },

    remove: function(div, message) {
        console.log("delete", message, div);
        mocoserver.remove("default", message);
        dojo.forEach(this.actionConnects, function(c) {
            dojo.disconnect(c);
        });
        div.parentNode.removeChild(div);
    },

	zoom: function(id, kind) {
		console.log("zoom", id, kind, this);
		var mid = 'message-id-' + id;
		dojo.destroy('zoom');
		var mc = dojo.coords(mid);
		var msg = dojo.byId(mid);
		var z = dojo.create("div", {"id":'zoom', innerHTML: ''}, this.domNode);
		dojo.style(z, 'position', 'absolute');
		var v = dijit.getViewport();
		dojo.style(z, 'left', '700px');
		dojo.style(z, 'top', v.t + (v.h / 2 - 160) + 'px');
        if(kind == "video") {
		    var divv = dojo.create("div", {}, z);
            var p = new dojox.embed.Flash({path: '/static/flash/player.swf', vars:{'type':'video', image:'/attachment/thumbnail/' + id + '.jpg', file: '/attachment/video/' + id + '.flv'}}, divv);
            /* var p = new dojox.av.FLVideo({initialVolume:.7, mediaUrl:"http://bidon.monimi.net/attachment/video/" + id, autoPlay:true}, divv); */
        } else if(kind == "audio") {
		    var divv = dojo.create("div", {}, z);
            var p = new dojox.embed.Flash({path: '/static/flash/player.swf', vars:{file: '/attachment/audio/' + id + '.mp4'}}, divv);
        } else {
		    var img = dojo.create("img", { src:"/attachment/preview/" + id + '.jpg', id:"lb-" + id}, z);
            var w = new dojox.image.LightboxNano({ href: "/attachment/image/" + id + '.jpg'}, "lb-" + id);
        }
	}


});
