dojo.provide("moco.UserList");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.Menu");
dojo.require("dijit.MenuItem");

dojo.declare("moco.UserList", [dijit._Widget, dijit._Templated],
    {
  	templatePath: dojo.moduleUrl("moco", "UserList.html"),
	userTemplate: '<div class="user"><a href="/${username}"><img style="width:24px; height:24px;" src="/api/avatar?username=${username}"></img></div></a></div>',

	postCreate: function() {
//        this.m = new dijit.Menu({leftClickToOpen:true});
//        this.m.addChild(new dijit.MenuItem({label:"Stop Following",
//                                            onClick: function(xx){console.log('i was clicked', xx, this)}
//                                            }));
//        this.m.addChild(new dijit.MenuItem({label:"Rest"}));
		dojo.forEach(this.users, function(username) {
				var user_div = dojo.place(dojo.string.substitute(this.userTemplate, {username: username}), this.usersNode, 'before');
//                this.m.bindDomNode(user_div);
			}, this);
	}
});
