dojo.provide("moco.AddContact");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.Dialog");
dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.form.Form");
dojo.require("dijit.form.Button");
dojo.require("dijit.form.ValidationTextBox");
dojo.require("dijit.layout.ContentPane");

dojo.declare("moco.AddContact", [dijit._Widget, dijit._Templated],
    {   
        title: "Add",
        templatePath: dojo.moduleUrl("moco", "AddContact.html"),
        widgetsInTemplate: true,

        show: function(){
                this.dialog = new dijit.Dialog({
                        title: this.title,
                        parseOnLoad: false
                });
                this.dialog.setContent(this.domNode);
                dojo.connect(this.okButton, "onClick", this, this.addContact);
                this.dialog.show();
                
//                this.tabContainer.addChild(new monimi.monitor.add.HttpCheck({title: 'HTTP', controller: this.controller, dialog:this.dialog}));             
//                this.tabContainer.addChild(new monimi.monitor.add.HttpsCheck({title: 'HTTPS', controller: this.controller, dialog:this.dialog}));           
//                this.tabContainer.addChild(new monimi.monitor.add.PingCheck({title: 'Ping', controller: this.controller, dialog:this.dialog}));             
//                this.tabContainer.addChild(new monimi.monitor.add.DnsCheck({title: 'DNS', controller: this.controller, dialog:this.dialog}));               
//                this.tabContainer.resize();
        },

        addContact: function() {
            console.log("addc");
            console.log(this.name.attr('value'));
            mocoserver.follow(this.name.attr('value'));
            this.dialog.hide();
        }
});
