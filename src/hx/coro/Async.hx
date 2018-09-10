package coro;

import coro.Coroutine;

class AsyncException extends haxe.Exception {}

@:access(coro.Async)
abstract AsyncValue<T>(Async) to Async {
	@:from
	static inline function fromAsync(async:Async):AsyncValue<Void> {
		return cast async;
	}

	public inline function new(coroutine:Coroutine<()->T>, ?generated:Generated<(sm:Async, resumeValue:Any)->Int>) {
		this = new Async(coroutine, generated);
	}

	/**
	 * Suspend context execution until this Async is finished
	 */
	@:suspend
	public function await(?context:Async):T {
		if(this.isFinished()) {
			context.state = context.nextState;
		}
		this.start()
			.catchError(context.proceedWithThrow)
			._then(context.proceed);
		return null;
	}

	/**
	 * Add a callback to execute when this Async is finished successfully.
	 */
	public inline function then(callback:(result:T)->Void):AsyncValue<T> {
		this._then(callback);
		return cast this;
	}

	/**
	 * Add a callback to execute when this Async is finished by throwing an unhandled exception.
	 */
	public inline function catchError(callback:(error:Any)->Void):AsyncValue<T> {
		this.catchError(callback);
		return cast this;
	}

	/**
	 * Execute callback when this Async if finished or failed.
	 */
	public inline function finally(callback:()->Void):AsyncValue<T> {
		this.finally(callback);
		return cast this;
	}

	/**
	 * Start executing this coroutine.
	 * Does nothing if execution was started already.
	 */
	public inline function start():AsyncValue<T> {
		this.start();
		return cast this;
	}
}

class Async extends ThrowingStateMachine {
	var resume:(sm:Async, resumeValue:Any)->Int;
	var thenCallbacks:Null<Array<(result:Dynamic)->Void>>;
	var errorCallbacks:Null<Array<(error:Any)->Void>>;

	/**
	 * Wrap another asynchronous approach into Async
	 * E.g. for `js.Promise`:
	 * ```
	 * var async = Async.wrap((resolve, reject) -> promise.then(resolve).catchError(reject))
	 * async.await();
	 * ```
	 */
	static public function wrap<T>(wrapper:(resolve:(T)->Void,reject:(Any)->Void)->Void):AsyncValue<T> {
		var async = new AsyncValue<T>(null);
		wrapper((async:Async).completeWithValueOnce, (async:Async).completeWithExceptionOnce);
		return async;
	}

	/**
	 * Wrap another asynchronous approach into Async.
	 */
	static public function wrapVoid(wrapper:(resolve:()->Void,reject:(Any)->Void)->Void):Async {
		var async = new Async(null);
		wrapper(async.completeWithValueOnce.bind(null), async.completeWithExceptionOnce);
		return async;
	}

	/**
	 * Returns finished Async with the `value` as the result.
	 */
	static public function resolve<T>(value:T):AsyncValue<T> {
		var async = new AsyncValue(null);
		(async:Async).completeWithValue(value);
		return async;
	}

	/**
	 * Returns finished Async which failed because of `error`.
	 */
	static public function reject(error:Any):Async {
		var async = new Async(null);
		async.completeWithException(error);
		return async;
	}

	/**
	 * Returns Async which will be finished in `durationMs` milliseconds.
	 */
	static public function delay(durationMs:Int):Async {
		var timer = new Async(null);
		haxe.Timer.delay(timer.completeWithValue.bind(null), durationMs);
		return timer;
	}

	/**
	 * Returns Async which will be finished or failed once any of `any` is finished or failed.
	 */
	static public function race(any:Array<Async>):AsyncValue<Any> {
		var result = new Async(null);
		for(async in any) {
			async.catchError(result.completeWithExceptionOnce)
				._then(result.completeWithValueOnce);
		}
		return cast result;
	}

	/**
	 * Returns Async which will be finished as soon as all Asyncs in `all` are finished.
	 * Returned Async will fail as soon as any of Asyncs in `all` is failed.
	 */
	static public function all(all:Array<Async>, ?context:Async):Async {
		var result = new Async(null);
		var then = _ -> {
			if(result.isFinished()) return;
			for(async in all) {
				if(!async.isFinished()) return;
			}
			result.completeWithValue(null);
		}
		for(async in all) {
			async.catchError(result.completeWithExceptionOnce)
				._then(then);
		}
		return result;
	}

	public function new(coroutine:Coroutine<()->Void>, ?generated:Generated<(sm:Async, resumeValue:Any)->Int>) {
		super();
		resume = generated;
	}

	/**
	 * Suspend context execution until this Async is finished
	 */
	@:suspend
	public function await(?context:Async):Void {
		if(isFinished()) {
			context.state = context.nextState;
		}
		start()
			.catchError(context.proceedWithThrow)
			._then(context.proceed);
	}

	/**
	 * Add a callback to execute when this Async is finished successfully.
	 */
	public function then(callback:()->Void):Async {
		_then(_ -> callback());
		return this;
	}

	inline function _then(callback:(result:Dynamic)->Void) {
		if(isFinished()) {
			if(state == Finished) {
				callback(returnValue);
			}
		} else {
			if(thenCallbacks == null) {
				thenCallbacks = [];
			}
			thenCallbacks.push(callback);
		}
	}

	/**
	 * Add a callback to execute when this Async is finished by throwing an unhandled exception.
	 */
	public function catchError(callback:(error:Any)->Void):Async {
		if(isFinished()) {
			if(state == Interrupted) {
				callback(throwOnResume);
			}
		} else {
			if(errorCallbacks == null) {
				errorCallbacks = [];
			}
			errorCallbacks.push(callback);
		}
		return this;
	}

	/**
	 * Execute callback when this Async if finished or failed.
	 */
	public function finally(callback:()->Void):Async {
		catchError(_ -> callback());
		_then(_ -> callback());
		return this;
	}

	/**
	 * Start executing this coroutine.
	 * Does nothing if execution was started already.
	 */
	public function start():Async {
		if(state == 0) {
			proceed(null);
		}
		return this;
	}

	inline function proceed<TResume>(resumeValue:Null<TResume>) {
		//Do nothing if already finished or no coroutine provided
		if(isFinished() || resume == null) {
			return;
		}
		var threw = false;
		try {
			resume(this, resumeValue);
		} catch(e:Dynamic) {
			completeWithException(e);
			threw = true;
		}
		if(!threw && isFinished()) {
			completeWithValue(returnValue);
		}
	}

	inline function proceedWithThrow(error:Any) {
		if(isFinished()) return;
		throwOnResume = error;
		proceed(null);
	}

	inline function isFinished():Bool {
		return state < 0;
	}

	inline function completeWithValue<T>(value:T) {
		state = nextState = Finished;
		returnValue = value;

		if(thenCallbacks != null) {
			for(cb in thenCallbacks) {
				cb(value);
			}
		}
		disposeListeners();
	}

	function completeWithValueOnce<T>(value:T) {
		if(isFinished()) return;
		completeWithValue(value);
	}

	inline function completeWithException(error:Any) {
		state = nextState = Interrupted;
		throwOnResume = error;

		if(errorCallbacks != null) {
			for(cb in errorCallbacks) {
				cb(error);
			}
		}
		disposeListeners();
	}

	function completeWithExceptionOnce(error:Any) {
		if(isFinished()) return;
		completeWithException(error);
	}

	inline function disposeListeners() {
		thenCallbacks = null;
		errorCallbacks = null;
	}
}