import coro.Coroutine;

class TransformCase extends BaseCase {
	var markers = [];
	var pushSuspend:Suspend<(x:Int)->Int>;

	@:suspend
	public function suspend<T>(?marker:Null<Int>):T {
		if(marker != null) markers.push(marker);
		return null;
	}

	public function setup() {
		markers = [];
		pushSuspend = markers.push;
	}
}