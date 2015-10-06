#WeDebugger-Web版Erlang代码断点调试工具

**weDebugger**是关于Erlang代码断点调试的工具，使用时将该项目做为目标项目依赖下载至deps目录中，然后在项目启动时调用we:start()以启动断点调试服务。

####运行项目
该项目使用rebar进行编译，具体运行方法如下：

1. **配置**：在目标项目rebar.config中添加如下依赖项以获取weDebugger：

``` erlang
	{weDebugger, ".*", {git, "https://git.coding.net/yuanmenggo/WeDebugger.git", "master"}}
```
             调试项目时，需要添加debug_info信息，rebar.config需要添加如下配置：

``` erlang
        {erl_opts, [debug_info]}.
```

2. **启动**：在目标项目启动入口添加如下调用以启动webDebugger调试服务：

``` erlang
	we:start().
```

3. **依赖**：目标项目运行./rebar get-deps下载依赖
4. **编译**：目标项目运行./rebar compile编译依赖
5. **运行**：运行目标项目，会在控制台打印如下提示，为启动服务成功，然后可在浏览器中访问该地址即可

```erlang
	weDebugger listening on http://127.0.0.1:20130/
```

####操作指南
打开浏览器后，访问地址： http://127.0.0.1:20130/， 按如下操作：
1. 在左侧栏**【文件列表】**中选择需要断点调试的文件，并双击，即可将文件加入左下方**【调试文件】**列表中；
2. 点击已加入**【调试文件】**列表中**【文件名超链接】**，启动文件；
3. 调试文件加载完成后，点击**【打开】**链接，即可在右边代码区域高亮显示文件内容；
4. 在代码区域**【行号栏】**上单击，即可给该模块加断点，再次点击该行号，取消该处断点；
5. **【进程列表】**中显示所有进程当前状态，点击**【链接】**选择需调试的进程开始连接；
6. 当已链接进程状态变为break时，点击进程**【信息栏超链接】**，打开调试模块文件，通过**【Step】**、**【Next】**、**【Finish】**进行断点调试。

####说明
为了更好的操作体验，现只开放对**chrome**浏览器的支持，请使用chrome来访问

#### 反馈与建议
- 邮箱：<shoumuyushan@gmail.com>,<debuggerbaby@gmail.com>,<649373694@qq.com>,<812445367@qq.com>
