// JavaScript Document
var Client = function(){  
    //呈现引擎  
    var engine = {  
        ie : 0,  
        gecko : 0,  
        webkit : 0,  
        khtml : 0,  
        opera : 0,  
        ver : null 
    };  
    //浏览器  
    var browser = {  
        //主要浏览器  
        ie : 0,  
        firefox : 0,  
        konq : 0,  
        opera : 0,  
        chrome : 0,  
        safari : 0,  
        //版本号  
        ver : null 
    };  
        
    var system = {  
        win : false,  
        mac : false,  
        x11 : false,  
        //移动设备  
        iphone : false,  
        ipod : false,  
        nokiaN  : false,  
        winMobile : false,  
        macMobile : false,  
        //游戏系统  
        wii : false,  
        ps　:false 
    };  
        
        
    //检测呈现引擎和浏览器  
    var ua  = navigator.userAgent;  
    if(window.opera){  
        engine.ver = browser.ver = window.opera.version();  
        engine.opera = browser.opera = parseFloat(engine.ver);  
    }else if(/AppleWebKit\/(\S+)/.test(ua)){  
        engine.ver = RegExp["$1"];  
        engine.webkit = parseFloat(engine.ver);  
            
        if(/Chrome\/(\S+)/.test(ua)){  
            browser.ver = RegExp["$1"];  
            browser.chrome = parseFloat(browser.ver);  
        }else if(/Version\/(\S+)/.test(ua)){  
            browser.ver = RegExp["$1"];  
            browser.safari = parseFloat(browser.ver);  
        }else{  
            var safariVersion = 1;  
            if(engine.webkit < 100){  
                safariVersion = 1;  
            }else if(engine.webkit < 312){  
                safariVersion = 1.2;  
            }else if(engine.webkit < 412){  
                safariVersion = 1.3;  
            }else{  
                safariVersion = 2;  
            }  
            browser.safari = browser.ver = safariVersion;  
        }  
    }else if(/KHTML\/(\S+)/.test(ua) || /Konqueror\/([^;]+)/.test(ua)){  
        engine.ver = browser.ver = RegExp["$1"];  
        engine.khtml = parseFloat(engine.ver);  
    }else if(/rv:([^\)]+)\) Gecko\/\d{8}/.test(ua)){  
        engine.ver = browser.ver = RegExp["$1"];  
        engine.gecko = parseFloat(engine.ver);  
            
        //  
        if(/Firefox\/(\S+)/.test(ua)){  
            browser.ver = RegExp["$1"];  
            browser.firefox = parseFloat(browser.ver);  
        }  
    }else if(/MSIE ([^;]+)/.test(ua)){  
        engine.ver = browser.ver = RegExp["$1"];  
        engine.ie = browser.ie = parseFloat(browser.ver);  
    }  
        
    browser.ie = engine.ie;  
    browser.opera = engine.opera;  
        
    //检测平台        
    var p = navigator.platform;  
    system.win = p.indexOf("Win") == 0;  
    system.mac = p.indexOf("Mac") == 0;  
    system.x11 = (p == "x11") || (p.indexOf("Linux") == 0);  
        
    //检测windows平台  
    if(system.win){  
        if(/Win(?:dows )?([^do]{2})\s?(\d+\.\d+)?/.test(ua)){  
            if(RegExp["$1"] == "NT"){  
                switch(RegExp["$2"]){  
                    case "5.0" :   
                        system.win = "2000";  
                        break;  
                    case "5.1" :  
                        system.win = "XP";  
                        break;  
                    case "6.0" :   
                        system.win = "Vista";  
                        break;  
                    default :  
                        system.win = "NT";  
                        break;                            
                }  
            }else if(RegExp["$1"] == "9x"){  
                system.win = "ME";  
            }else{  
                system.win = RegExp["$1"];  
            }  
        }  
    }  
        
    system.iphone = ua.indexOf("iPhone") > -1;  
    system.ipod = ua.indexOf("iPod") > -1;  
    system.nokiaN = ua.indexOf("nokiaN") > -1;  
    system.winMobile = (system.win =="CE");  
    system.macMobile = (system.iphone || system.ipod);  
        
    system.wii = ua.indexOf("Wii") > -1;  
    system.ps = /playstation/i.test(ua);  
        
    return {  
        engine : engine,  
        browser : browser,  
        system : system  
    };  
}();   
  
function check_client()
{
	var client_result = Client;
	if(client_result["browser"]["chrome"] ==0)
	{
		var client_name = get_client_name(client_result);
	  $("#content").html("<div id='client_check'><p class='client_title'>您的浏览器为<span>"+client_name+"</span></p><p class='client_alert'>本系统只支持Chrome浏览器，请选择Chrome进行访问!</p><p class='down_chrome'><span><a href='http://www.google.cn/intl/zh-CN/chrome/browser/?installdataindex=chinabookmarkcontrol&brand=CHUN'>下载Chrome浏览器</a></span></p></div>");
	 
	}
}

function get_client_name(client_result)
{
	for(var k in client_result["browser"])
	{
		if(client_result["browser"][k]>0)
		{
			return k;
		}
	}
}
check_client();
//alert('<pre class="dbdump">' + JSON.stringify(Client, null, 4) + '</pre>');