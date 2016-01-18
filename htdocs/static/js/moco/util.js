dojo.provide("moco.util");

moco.util = {
	datesDiff: function(date_earlier, date_later) {
		if(date_earlier == null || date_later == null) {
			return null;
		}
		var diff = date_later - date_earlier;
		var milliseconds = Math.floor(diff % 1000);   
		    diff = diff/1000;            
		var seconds = Math.floor(diff % 60);
		    diff = diff/60;
		var minutes = Math.floor(diff % 60);
		    diff = diff/60;
		var hours = Math.floor(diff % 24);
		    diff = diff/24;
		var days = Math.floor(diff);
		return {ms:milliseconds, seconds:seconds, minutes:minutes, hours:hours, days:days};
	},
	
	dateFromUtcTimestamp: function(ts) {
        //var offset = (new Date()).getTimezoneOffset() * 60 * 1000;
		//return new Date((ts * 1000) + offset);	
		return new Date(ts * 1000);	
	},
	
    
    ps: function(n, w) {
        if(n == 1) return n + " " + w; 
        else return n + " " + w + "s"; 
    },

    formatTimestamp: function(ts) {
        var dd = moco.util.datesDiff(moco.util.dateFromUtcTimestamp(ts), new Date());
        if(dd.days) {
            return moco.util.ps(dd.days, "day") + " ago";
        } else if(dd.hours) {
            return moco.util.ps(dd.hours, "hour") + " ago";
        } else if(dd.minutes) {
            return moco.util.ps(dd.minutes, "minute") + " ago";
        } else {
            return moco.util.ps(dd.seconds, "second") + " ago";
        }
    }
}
