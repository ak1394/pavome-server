dojo.provide("moco.NewPost");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");
dojo.require("dijit.form.SimpleTextarea");

dojo.declare("moco.NewPost", [dijit._Widget, dijit._Templated],
    {
  	templatePath: dojo.moduleUrl("moco", "NewPost.html"),
	widgetsInTemplate: true,
    inReplyToMessage: null,

	postCreate: function() {
		dojo.connect(this.submit, 'onClick', this, 'send');
	},
	
	setInReplyToMessage: function(message) {
		this.area.attr('value', '@' + message.author + ' ');
        this.inReplyToMessage = message;
    },

	send: function() {
		var text = this.area.attr('value');
		this.area.attr('value', '');
        var new_message = {body: text};
        if(this.inReplyToMessage) {
            new_message.in_reply_to = this.inReplyToMessage.id;
        }

        new_message.recipients = [];
        dojo.forEach(new_message.body.split(' '), function(fragment) {
            if(fragment.length > 1 && fragment.charAt(0) == '@') {
                new_message.recipients.push(fragment.substring(1));
            }
        }, this);

        mocoserver.post(mocoserver.currentTopic, new_message);
        this.inReplyToMessage = null;
	}
});
