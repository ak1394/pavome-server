#!/bin/sh
java -Xmx64m -classpath ../shrinksafe/shrinksafe.jar:../shrinksafe/js.jar org.mozilla.javascript.tools.shell.Main build.js profileFile=$HOME/server/etc/dojo-build.profile.js action=release copyTests=false optimize=shrinksafe cssOptimize=comments cssImportIgnore=../dijit.css releaseDir=../../../ releaseName=dojo-custom
