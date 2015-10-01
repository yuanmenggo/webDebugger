// JavaScript Document
var Debugger = (function() {	
	var gen_source_tree = function()
	{
		$("#source_tree").treeview({url: "/debugger/tree"})	
	}
	var current_mod_name = "";
	var get_current_mod_name = function()
	{
		return current_mod_name;	
	}
	
	var set_current_mod_name = function(mod_name)
	{
		current_mod_name = mod_name;
		$("#current_source_mod").html(mod_name);
	}
	var compile_array = new Array;
	var compile_source_file = function(id)
	{
		var index = compile_array.indexOf(id);
		if(index==-1)
		{	
			compile_array.push(id);
			debug_compile(id);
		}
		else
		{
			change_state(id, "<a href='#' name='"+id+"' onClick='javascript:read_interpret_file(\""+id+"\")'><font color='#090'>打开</font></a>&nbsp;&nbsp;<a href='#' onClick='javascript:del_interpret(\""+id+"\")'><font color='#f00'>删除</font></a>");	
		}	
	}
	
	var add_compile_file = function(id)
	{
		var index = compile_array.indexOf(id);
		if(index==-1)
		{	
			compile_array.push(id);
			debug_compile(id);
		}		
	}
	
	var debug_compile = function(id)
	{	
		// $("#debugger_files #state_"+id).html("<font color='#ff0000'>编译中</font>");
		// $.getJSON('/debugger/file/'+id, function(json) 
		// {
		// 	tip_show(JSON.stringify(json, null, 4).replace(/\\n/g,function(l,i){return "<br/>"}));
		// 	var tip = JSON.stringify(json, null, 4).replace(/\\n/g,function(l,i){return "<br/>"})
		// 	change_state(id, "<font color='#090'>编译完成</font>");
		// 	window.setTimeout(function a(t){read_debug_beam(t)}, 2000, id);
		// });
		read_debug_beam(id);
		
	}
	
	var read_debug_beam = function(id)
	{
		// $("#debugger_files #state_"+id).html("<font color='#090'>读取数据</font>");
		$.getJSON('/debugger/chunks/'+id, function(json) {
			tip_show(JSON.stringify(json["data"], null, 4).replace(/\\n/g,function(l,i){return "<br/>"}));
			change_state(id, "<a href='#' name='"+id+"' onClick='javascript:read_interpret_file(\""+id+"\")'><font color='#090'>打开</font></a>&nbsp;&nbsp;<a href='#' onClick='javascript:del_interpret(\""+id+"\")'><font color='#f00'>删除</font></a>");			
		});											   
	}

	var change_state = function(id, str){ $("#debugger_files #state_"+id).html(str);}

	var tip_show = function(str)
	{
		$("#debugger_tishi #debugger_content").html(str);
		if($("#debugger_tishi").css("display") == "none")
		{ 
			$("#debugger_tishi").slideDown();	
			window.setTimeout(function(){ $("#debugger_tishi").slideUp();}, 2000);
		}

	}
	
	var do_break_list = function(mod, line, is_all, obj)
	{
		$.getJSON('/debugger/break/'+mod+'/'+ line+"/"+is_all, function(json) {												
			do_break_html(json, obj);														
		});
	}
	
	var break_line_json = {};
	var current_debugger_module = "";
	var current_debugger_line = -1;
	var current_debugger_pid = "";
	
	var set_current_debugger_module = function(mod_name)
	{
		var current_txt_mod = $("#current_source_mod").html()
		console.log(current_txt_mod+"/"+mod_name);
		if(current_txt_mod!="" && current_txt_mod !=undefined && mod_name != current_txt_mod){
			var mod_name_file = mod_name+".erl";
			$("#debugger_files li a").each(function(){ 
				if($(this).html()==mod_name_file){
					var new_open_mod = $(this).attr("name");
					read_interpret_file(new_open_mod);
				}})
		}
		current_debugger_module = mod_name;
	}
	
	var set_current_debugger_line = function(line)
	{
		current_debugger_line = line;	
		
	}
	
	var set_current_debugger_pid = function(pid)
	{
		current_debugger_pid = pid;	
	}
	
	
	var get_current_debugger_pid = function()
	{
		return current_debugger_pid;
	}
	
	var do_break_html =function(json, obj)
	{
		if(json == "not_loaded") {
			tip_show("文件未能加载!!");
			if(obj !=undefined) $(obj).removeClass("debug_line");
			return;
		}
		var html=" ";
		var html2 =""
		for(var k in json)
		{
			var mod_name = json[k]["name"];
			if(!break_line_json[mod_name]) break_line_json[mod_name]=[];
			break_line_json[mod_name].push(json[k]["line"]);
			html+="<tr id='break_tr_"+mod_name+"_"+json[k]["line"]+"' class='"+mod_name+"'><td width='160'>"+mod_name+"</td><td>"+json[k]["line"]+"</td><td>"+json[k]["status"]+"</td><td>"+json[k]["action"]+"</td><td><a onClick='javascript:position_line_by_module(\""+json[k]["name"]+"\","+json[k]["line"]+")' href='#'>定位</a></td><td ><a href='#' onClick='javascript:del_break_by_module(\""+json[k]["name"]+"\","+json[k]["line"]+")'>移除</a></td></tr>";
			if(compile_array.indexOf(mod_name) ==-1)
			{
				compile_array.push(mod_name);
				html2 += '<li id="debug_'+json[k]["path"]+'"><a  name="'+json[k]["path"]+'" onclick="javascript:open_interpret(&quot;'+json[k]["path"]+'&quot;)" href="#">'+mod_name+'.erl</a><span id="state_'+json[k]["path"]+'"><a href="#" onclick="javascript:read_interpret_file(&quot;'+json[k]["path"]+'&quot;)"><font color="#090">打开</font></a>&nbsp;&nbsp;<a href="#" onclick="javascript:del_interpret(&quot;'+json[k]["path"]+'&quot;)"><font color="#f00">删除</font></a></span></li>';
			}
		}
		$("#break_tab").html(html);
		$("#debugger_files ul").append(html2);
		
	}
	
	var attach_array = new Array;
	
	var add_attach = function(attach_id)
	{
		attach_array.push(attach_id);
		$("#snap_tr_"+attach_id).addClass("attach_pid");
	}
	
	var update_snap_shot_html = function(json)
	{
		console.log('<pre class="dbdump">' + JSON.stringify(json, null, 4) + '</pre>');
		var html=" ";
		var i=0;
		for(var k in json)
		{
			i++;
			var status = json[k]["status"];
			var msg = json[k]["msg"];
			var attached = json[k]["attached"];
			var pid = json[k]["pid"]["data"].replace("\<","").replace("\>","").replace(/\./g,"_");
			var current_select = current_debugger_pid==""?"":"current_select";
			
			var detail = attached ==true?"<a href='#' onClick='javascript:pid_break_by_module(\""+json[k]["pid"]["data"]+"\")'><font color='#F00'>数据</font></a>" :"<a href='#' onClick='javascript:pid_attach_by_module(\""+json[k]["pid"]["data"]+"\")'>链接</a>";
			check_cur_process_is_idle(json[k]["pid"]["data"], status);
			var attach_pid = status =="break"?"attached_pid":attach_array.indexOf(pid)>=0?"attach_pid":"";
			html+="<tr id='snap_tr_"+pid+"' class='"+json[k]["mod"]+" "+attach_pid+"'><td width='40' id='order_"+pid+"' class='order_pid "+current_select+"'>"+i+"</td><td id='"+pid+"_mod_fun' width='240'><font color='#000'>"+json[k]["mod"]+"</font>:<font color='#000'>"+json[k]["funs"]+"</font></td><td id='"+pid+"_status' width='50'>"+json[k]["status"]+"</td><td><a href='#' id='"+pid+"_msg' onClick='javascript:open_debug_source_win(\""+json[k]["pid"]["data"]+"\",\""+json[k]["msg"]["data"]+"\")'>"+json[k]["msg"]["data"]+"</a></td><td width='60' id='"+pid+"_detail'>"+detail+"</td></tr>";
		}

		$("#snapshot_tab").html(html);
	}

	function check_cur_process_is_idle(pid, status){
		if(current_debugger_pid == pid && status == "idle"){
			$("#code_editor div.line").removeClass("debug_code_line");
		}
	}
	
	function read_file(id)
	{
		$.getJSON('/debugger/read/'+id, function(json) {
			$("#code_editor").html(JSON.stringify(json, null, 4).replace(/(^"*)|("*$)/g,'').replace(/\\r/g,''));
			var brush = new SyntaxHighlighter.brushes.Erland(),
				code = document.getElementById('code_editor').innerHTML,
				html
				;
			var paths = id.split("__");
			var mod_name = paths[paths.length-1];
			set_current_mod_name(mod_name);
			brush.init({ toolbar: false, id:mod_name, break_json: break_line_json, debug_line:current_debugger_line});
			html = brush.getHtml(code);
	
			$("#code_editor").html(html.replace(/\\t/g,"").replace(/\\/g,"").replace(/<code class="plain">(%.*?)<\/code>/g,'<code class="comments">$1<\/code>'));									
		});
	}
	
	var set_debugger_data = function(json)
	{
		var pid = json["pid"]["data"].replace("\<","").replace("\>","").replace(/\./g,"_");
		if(json["status"])
		{ 
			$("#"+pid+"_status").html(json["status"]);
			
		}
		if(json["attached"])
		{	
			$("#"+pid+"_detail").html("<a href='#' onClick='javascript:pid_break_by_module(\""+json["pid"]["data"]+"\")'><font color='#F00'>数据</font></a>");
		}
		else
		{
			$("#"+pid+"_detail").html("<a href='#' onClick='javascript:pid_attach_by_module(\""+json["pid"]["data"]+"\")'>链接</a>");	
		}
		
		if(json["mod"] && json["func"]){ $("#"+pid+"_mod_fun").html("<font color='#09F'>"+json["mod"]+"</font>/<font color='#A42206'>"+json["func"]+"</font>");}
		if(json["mod"] && json["line"]){ 
			set_current_debugger_line(json["line"]);
			set_current_debugger_module(json["mod"]);
		    $("#snap_tr_"+pid).addClass("attach_pid");
			$("#code_editor div.line").removeClass("debug_code_line").removeClass("debug_order_line");
			$("#code_editor #order_"+json["mod"]+"_"+json["line"]).addClass("debug_order_line");
			$("#code_editor #con_"+json["mod"]+"_"+json["line"]).addClass("debug_code_line");
			
		}
		if(json["bindings"]) show_bindings(json["bindings"]);
		scroll_to_line(current_debugger_module, current_debugger_line);
	}
	
	var scroll_to_line = function(mod, line)
	{
		
		if (mod=="" && current_debugger_module!="") 
			mod =current_debugger_module;
		else if($("#current_source_mod").html()!=="")
		{
			mod	=$("#current_source_mod").html();
		}
		else
		{
			alert("非当前模块，无法定位到指定位置!");
			return;
		}

		var top = $("#code_editor #con_"+mod+"_"+line).offset().top;
		var curTop = $("#highlighter_"+mod).scrollTop();

		if(top -curTop >10 || curTop - top >10) 
		{
			console.log($("#highlighter_"+mod));
			$("#highlighter_"+mod).stop().animate({scrollTop:top+curTop-120}, 1000);
		}
	}
	
	var pos_line = function(mod, line)
	{
		if(mod != current_debugger_module && mod!="" && $("#current_source_mod").html()=="") 
		{
			alert("非当前模块，无法定位到指定位置!");
			return;
		}
		scroll_to_line(mod, line);
	}
	
	var show_bindings = function(json)
	{
		var html="";
		for(var k in json)
		{
			html+="<li><span class='bind_key'>"+k+"</span><span class='bind_value'>"+get_erlang_source_color("",JSON.stringify(json[k], null, 4))+"</span></li>"
		}
		$("#binding_ul").html(html);	
	}
	
	function get_erlang_source_color(k,data)
	{
		var string_res = data.replace(/(^"*)|("*$)/g,'').replace(/,/g,',&nbsp;').replace(/\\n/g,function(l,i){return "^";}).replace(/#\w+/g,function(m,i){return "<font color='#900'>"+m+"</font>";}).replace(/[\[\]]+/g,function(n,i){return "<font color='#ff0000'> "+n+" </font>";}).replace(/[\{\}]+/g,function(l,i){return "<font color='#F90'> "+l+" </font>";}).replace(/(false|true)/g,'<font color=\'#06F\'>$1</font>').replace(k,"<font color='#54A767'><b>"+k+"</b></font>").replace(/\^/g,function(l,i){return "<br/>";}).replace(/(\w+) = (\w+)/g,'<font color=\'#F0F\'>$1</font> <font color=\'#096\'>=</font> <font color=\'#93C\'>$2</font>').replace(/ok./g,'');
		return string_res;
	}
	
    return {
		gen_source_tree:gen_source_tree,
		compile_source_file:compile_source_file,
		do_break_list:do_break_list,
		do_break_html:do_break_html,
		get_current_mod_name:get_current_mod_name,
		set_current_mod_name:set_current_mod_name,
		update_snap_shot_html:update_snap_shot_html,
		add_attach:add_attach,
		add_compile_file:add_compile_file,
		set_current_debugger_module:set_current_debugger_module,
		set_current_debugger_line:set_current_debugger_line,
		set_current_debugger_pid:set_current_debugger_pid,
		get_current_debugger_pid:get_current_debugger_pid,
		set_debugger_data:set_debugger_data,
		show_bindings:show_bindings,
		get_erlang_source_color:get_erlang_source_color,
		pos_line:pos_line,
		read_file:read_file
    }
})();


$("body").delegate("#source_tree li", "click", function(e)
{	
	var len = $(this).children("ul").length;
	if(len==0)
	{
		$("#source_tree li span").removeClass("file_selected");
	    $(this).children("span").addClass("file_selected");
	}
	//e.stop()
});

$("body").delegate("#source_tree li", "dblclick", function(e)
{	
	var len = $(this).children("ul").length;
	if(len==0)
	{
		if (!$(this).hasClass("debugger_file"))
		{
			var name = $(this).children("span").text();
			var id = $(this).attr("id");
			$(this).addClass("debugger_file");
			$("#debugger_files ul").append("<li id='debug_"+id+"'><a name='"+id+"' onClick='javascript:open_interpret(\""+id+"\")' href='#'>"+name+"</a><span  id='state_"+id+"'><a href='#' onClick='javascript:del_interpret(\""+id+"\")'>移除</a></span></li>");
			//Debugger.add_compile_file(name.substring(0,name.length-4));
		}
		else
		{
			alert("已添加此文件!");	
		}
	}
	//e.stop()
});

function del_interpret(id)
{	
	$.getJSON('/debugger/del/interpreted/'+id, function(json) {})
	$("#debugger_files #debug_"+id).remove();
	$("#"+id).removeClass("debugger_file");
	$("#"+id).children("span").removeClass("file_selected");
}

function open_interpret(id)
{
	Debugger.compile_source_file(id);
}

function read_interpret_file(id)
{
	Debugger.read_file(id);
}



$("body").delegate("#code_editor .gutter .line", "click", function(e)
{	
		var is_checked = $("input[name='break_check_all']").attr("checked");
		var id_str = $(this).attr("id");		
		var tmps = id_str.split("_");
		var line = tmps[tmps.length-1];
		var id = tmps.slice(1,tmps.length-1).join("_");
		var is_all = "self";
		if(is_checked==undefined) is_all = "all";
		if (!$(this).hasClass("debug_line"))
		{
			$(this).addClass("debug_line");		
			var result = Debugger.do_break_list(id,line,is_all, this);
		}
		else
		{
			$(this).removeClass("debug_line");
			$.getJSON('/debugger/break/del/'+id+'/'+ line, function(json) {$("#break_tr_"+id+"_"+line).remove();})
		}
});



function del_break_by_module( module, line)
{
	$.getJSON('/debugger/break/del/'+module+'/'+ line, function(json) {
		$("#break_tr_"+module+"_"+line).remove();	
		$("#order_"+module+"_"+line).removeClass("debug_line");
	})
}

$(function(){
	$("#break_check_all").bind("click",function(){
		var is_checked = $("input[name='break_check_all']").attr("checked");
		var mod_name = Debugger.get_current_mod_name();
		if(mod_name != "" && is_checked=="checked")
		{
			$("#break_tab tr").each(function(){
				
				if($(this).hasClass(mod_name)) 
					$(this).show();
				else
					$(this).hide();
			})
		}
		else
		{
			$("#break_tab tr").each(function(){
				 $(this).show();				 
			})
		}
	})
	
	$("#break_status_all").bind("click", function(){
		var is_checked = $("input[name='break_status_all']").attr("checked");
		var mod_name = Debugger.get_current_mod_name();
		if(mod_name != "" && is_checked=="checked")
		{
			$("#snapshot_tab tr").each(function(){
				
				if($(this).hasClass(mod_name)) 
					$(this).show();
				else
					$(this).hide();
			})
		}
		else
		{
			$("#break_tab tr").each(function(){
				 $(this).show();				 
			})
		}											  
	})
	
	$("#debug_step").bind("click",function(){
		var pid = Debugger.get_current_debugger_pid();
		if(pid=="") 
		{
			alert("请先选择一个调试跟踪进程!")	
			return;
		}
		$.getJSON('/debugger/step/'+pid, function(json) {});									   
	})
	$("#debug_next").bind("click",function(){
		var pid = Debugger.get_current_debugger_pid();
		if(pid=="") 
		{
			alert("请先选择一个调试进程!")	
			return;
		}
		$.getJSON('/debugger/next/'+pid, function(json) {});												   
	})
	$("#debug_finish").bind("click",function(){
		var pid = Debugger.get_current_debugger_pid();
		if(pid=="") 
		{
			alert("请先选择一个调试跟踪进程!")	
			return;
		}
		$.getJSON('/debugger/finish/'+pid, function(json) {});											 
	})
	
	$("#debug_clear").bind("click",function(){
		$.getJSON('/debugger/clear', function(json) {
			alert("已清理所有僵尸进程!")										 
		});										
	})
	
	$("#debug_stop").bind("click", function(){
		$.getJSON('/debugger/stop', function(json) {
			alert("已终止所有断点调试!")										 
		});												
	 })
	
	$("#debug_more").bind("click",function()
	{
		if($("#debugger_funs").css('display') =='block') 
		{
			$("#debugger_funs").hide();
			$(this).children("a").removeClass("selected_btn");
			return;
		}
		
		var mod_name = Debugger.get_current_mod_name();
		if(mod_name=="") 
		{
			alert("请先选择一个调试模块!")	
			return;
		}
		$(this).children("a").addClass("selected_btn");
		$.getJSON('/debugger/funs/'+mod_name, function(json) {
			//alert('<pre class="dbdump">' + JSON.stringify(json, null, 4) + '</pre>');
			var html="<ul>";
			for(var k in json)
			{
				html+="<li><a href='#' onClick='position_line_by_module(\"\","+json[k]["data"][1]+")'>"+json[k]["data"][0]+"/"+json[k]["data"][2]+"</a></li>"
			}
			html+="</ul>"
			$("#debugger_funs_list").html(html);
			$("#debugger_funs").show();
		});											 
	})
})


function pid_attach_by_module(pid)
{
	$.getJSON('/debugger/attach/pid_info/'+pid+'/', function(json) {
		var pid2 = pid.replace("\<","").replace("\>","").replace(/\./g,"_");
		Debugger.add_attach(pid2);
	})		
}

function pid_break_by_module(pid)
{
	$.getJSON('/debugger/break/pid_info/'+pid+'/', function(json) {Debugger.show_bindings(json);})	
}
//.replace(/{(.+?),(.+?)},\\n/g,'<li><span class=\'bind_key\'>$1</span><span class=\'bind_value\'>$2</span></li>').

	
function open_debug_source_win(pid, msg)
{
	var debug = msg.split(",");
	var mod_name = debug[0];
	var line = debug[1];
	$.getJSON('/debugger/path/'+mod_name, function(json) {
		Debugger.read_file(JSON.stringify(json, null, 4).replace(/(^"*)|("*$)/g,'') );
		Debugger.set_current_debugger_module(mod_name);
		Debugger.set_current_debugger_line(line);
		Debugger.set_current_debugger_pid(pid);
		$("#snapshot_tab .order_pid").removeClass("current_select");
		$("#snapshot_tab #order_"+pid).addClass("current_select");
	});
}

function position_line_by_module(mod, line)
{
	Debugger.pos_line(mod, line);	
	
}
