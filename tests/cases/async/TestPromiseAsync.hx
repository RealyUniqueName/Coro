package cases.async;

#if js
import utest.Assert;
import haxe.Timer;
import js.Promise;
import coro.Async;

using coro.Async;
#end

class TestPromiseAsync extends BaseCase {
#if js
	public function testAwaitPromise() {
		var delayMs = 100;
		var promise = new Promise((resolve, reject) -> Timer.delay(resolve.bind(100), delayMs));
		var async = new AsyncValue(() -> {
			var promiseResult = promise.await();
			return promiseResult * 2;
		});

		var startTime = Timer.stamp();
		var done = Assert.createAsync(
			() -> {
				var durationMs = (Timer.stamp() - startTime) * 1000;
				Assert.isTrue(durationMs >= delayMs);
				async.then(result -> Assert.equals(200, result));
			},
			3000
		);
		async.then(_ -> done()).catchError(e -> throw e).start();
	}

	public function testAsyncToPromise() {
		var async = Async.resolve(100);
		var promise = async.promise();

		var done = Assert.createAsync(
			() -> promise.then(result -> Assert.equals(100, result)),
			3000
		);
		promise.then(_ -> done()).catchError(e -> throw e);
	}
#end
}