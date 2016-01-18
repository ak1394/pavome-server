dojo.provide("moco.UserBadge");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");

dojo.declare("moco.UserBadge", [dijit._Widget, dijit._Templated],
    {
  	templatePath: dojo.moduleUrl("moco", "UserBadge.html"),

	postCreate: function() {
		console.log("here", this.username);
	}
});
