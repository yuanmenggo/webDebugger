var tplSocket = (function() {
  var host = document.location.host;
  var socks = {};
  
  var onopen = function() {
   console.log("tpl socket opened");
  }

  var visit = function(href) {
  }

  var type = function(v) {
    var t = typeof v;
    if(t == "object") {
      if(v['_type'] != undefined) {
        t = v['_type'];
      }
    }
    return t;
  }
	
  var update = function(k, data) {
    var el;
    if(typeof k == 'object') {
      el = k;
    } else {
      el = $('#'+k);
    }
	

	if(k =="processes_snapshot")
	{
		Debugger.update_snap_shot_html(data);
	}

	if(k =="debugger_break")
	{
		console.log("<pre class='dbdump'>"+JSON.stringify(data, null, 4)+"</pre>");
		Debugger.set_debugger_data(data);
		$("#debug_info_p").html("数据已更新");
		window.setTimeout(function(){ $("#debug_info_p").html("");}, 3000);
	}
	
    el.triggerHandler('onupdate', data);
  }
  var onjson = function(json) {
    for(var k in json) {   
      update(k, json[k]);
    }
  }

  var onmessage = function(msg) {
	  // console.log("<pre class='dbdump'>"+JSON.stringify(msg, null, 4)+"</pre>");		
    onjson($.parseJSON(msg.data));
  }

  var onclose = function() {
	  console.log("tpl socket closed");
  }

  var fetch = function(url) {
    $.getJSON(url, onjson);
  }

  var close_socket = function(to)
  {
	  if(socks[to]!=undefined) socks[to].close(); 
  }
  
  return {
    visit: visit,
    update: update,
    connect: function(to) {
      socks[to] = new WebSocket("ws://"+host+to);
      socks[to].onopen = onopen;
      socks[to].onmessage = onmessage;
      socks[to].onclose = onclose;
    },
	close_socket:close_socket,
    fetch: fetch
  }
})();