#if macro
import haxe.macro.PositionTools;
import haxe.macro.Context;

using sys.FileSystem;
using haxe.io.Path;

private typedef CoroPluginApi = {
	/** This method should be executed at initialization macro time */
	function run():Void;
}

class PluginLoadingException extends haxe.Exception {}
#end

class Coro {

#if macro
	static public var plugin(get,never):CoroPluginApi;
	static var _plugin:CoroPluginApi;
	static function get_plugin():CoroPluginApi {
		#if (haxe_ver < '4.0.0')
		throw new PluginLoadingException('Haxe >= 4.0.0 is required to load Coro plugin.');
		#else
		if(_plugin == null) {
			try {
				_plugin = eval.vm.Context.loadPlugin(getPluginPath());
			} catch(e:Dynamic) {
				throw new PluginLoadingException(Std.string(e));
			}
		}
		#end
		return _plugin;
	}

	static public function getPluginPath():String {
		var pos = Context.getPosInfos(PositionTools.here());
		var srcDir = pos.file.directory().directory();
		var path = Path.join([srcDir, 'ml', 'coro_plugin.cmxs']); //development path
		//if development binary does not exist, use pre built one
		if(!path.exists()) {
			path = Path.join([srcDir, 'bin', Sys.systemName(), 'coro_plugin.cmxs']);
		}
		return path;
	}

	static public function register() {
		#if display
			return;
		#end
		if(Context.defined('display')) {
			return;
		}

		//make sure `coro.Coroutine` is loaded
		Context.getType('coro.Coroutine');

		try {
			plugin.run();
		} catch(e:PluginLoadingException) {
			trace('Failed to load plugin: ${e.message}');
		}
	}
#end
}