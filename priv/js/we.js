// JavaScript Document
We = (function() {

   var node_name ='nonode@nohost';
   var socket_list = [];

  (function() {
  	reset_height();
  	clear_content();
	$(window).resize(function(){
 		reset_height();
		reset_width();
	}); 

	clear_content();		
	tplSocket.connect('/web/stream/debugger');
	Debugger.gen_source_tree();

	$.getJSON('/debugger/start/snapshot', function(json) {														
	});
	$.getJSON('/debugger/breaks', function(json) {
		if(json == "not_loaded") return;
		Debugger.do_break_html(json, undefined);														
	});
	$("#debugger_tree").show();
	$('#debugger').show();
  })();
	

	function clear_content()
	{
		$('#debugger').hide();
		$("#debugger_tree").hide();
	}

	
	function reset_height()
	{
		var height = $(window).height()-45;
		
		$("#left_content").height(height);
		$("#right_content").height(height);
		$("#code_editor").height(height-300);
	}

	function reset_width()
	{
		var width = $(window).width();
		$("#content #right_content .map_div").width(width-230);
	}

  return {
  	clear_content:clear_content
  };
  
})();
