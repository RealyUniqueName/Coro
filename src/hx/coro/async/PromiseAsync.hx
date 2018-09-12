package coro.async;

import js.Promise;
import coro.Async;

class PromiseAsync {
	@:suspend
	static public function await<T>(promise:Promise<T>, ?context:Async):T {
		return Async.wrap((resolve, reject) -> {
			promise.then(resolve).catchError(reject);
		}).await(context);
	}

	static public function promise<T>(async:AsyncValue<T>):Promise<T> {
		return new Promise((resolve, reject) -> {
			async.start().then(resolve).catchError(reject);
		});
	}
}