dependencies = {
        layers: [
                {       
                        name: "moco-login.js",
                        dependencies: [
                                'dijit.form.Button',
                                'moco.Server'
                        ]
                },
                {       
                        name: "moco.js",
                        dependencies: [
                                'dijit.form.Button',
                                'dijit.form.CheckBox',
                                'dijit.layout.AccordionContainer',
                                'dijit.form.ValidationTextBox',
                                'moco.Server'
                        ]
                }
        ],
        
        prefixes: [
                [ "dijit", "../../dojo-release-1.3.2-src/dijit" ],
                [ "dojox", "../../dojo-release-1.3.2-src/dojox" ],
                [ "moco", "../../moco" ]                                                                               
        ]                                                                                                                  
}
