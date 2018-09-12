package cases;

import coro.Async;
import utest.Assert;
import haxe.Timer;

class TestAsync extends BaseCase {
	public function testDelay() {
		var delayMs = 50;
		var startTime = Timer.stamp();
		var async = new AsyncValue<Int>(() -> {
			Async.delay(delayMs).await();
			return 20;
		});

		var done = Assert.createAsync(
			() -> {
				var durationMs = (Timer.stamp() - startTime) * 1000;
				Assert.isTrue(durationMs >= delayMs);
				async.then(result -> Assert.equals(20, result));
			},
			3000
		);
		async.then(_ -> done()).catchError(e -> throw e).start();
	}

	public function testExceptionPropagation() {
		var caught = false;
		var error = "Terrible error";
		var throwString = new Async(() -> {
			Async.delay(10).await();
			throw error;
		});
		var async = new Async(function():Void {
			try {
				throwString.await();
			} catch(e:String) {
				caught = e == error;
			}
		});

		var done = Assert.createAsync(() -> Assert.isTrue(caught), 3000);
		async.start().then(() -> done()).catchError(e -> throw e);
	}

	public function testResolve() {
		var async = new AsyncValue(() -> Async.resolve(100).await());
		async.start()
			.catchError(e -> throw e)
			.then(v -> Assert.equals(100, v));
	}

	public function testReject() {
		var msg = 'Terrible error';

		var async = new AsyncValue(() -> {
			var error = Async.reject(msg);
			try {
				error.await();
			} catch(e:String) {
				return e;
			}
			throw 'Unexpected';
		});

		async.start()
			.catchError(e -> throw e)
			.then(v -> Assert.equals(msg, v));
	}

	public function testFinally() {
		var fail = new Async(() -> throw 'Terrible error');
		var success = new AsyncValue(() -> 10);

		var onFail = false;
		var onSuccess = false;
		fail.start().finally(() -> onFail = true);
		success.start().finally(() -> onSuccess = true);

		Assert.isTrue(onFail);
		Assert.isTrue(onSuccess);
	}

	public function testRace_success() {
		var race = Async.race([Async.delay(100), Async.resolve(10)]);

		race.catchError(e -> throw e)
			.then(v -> Assert.equals(10, v));
	}

	public function testRace_fail() {
		var error = 'Terrible error';
		var race = Async.race([Async.delay(100), Async.reject(error)]);

		race.catchError(e -> Assert.equals(error, e))
			.then(v -> Assert.fail());
	}

	public function testAll_success() {
		var startTime = Timer.stamp();
		var delay = Async.delay(100);
		var all = Async.all([Async.resolve(10), delay]);

		var done = Assert.createAsync(
			() -> {
				var duration = Math.round((Timer.stamp() - startTime) * 1000);
				Assert.isTrue(duration >= 100);
			},
			1000
		);
		all.start()
			.catchError(e -> throw e)
			.then(done);
	}

	public function testAll_fail() {
		var error = 'Terrible error';
		var all = Async.all([Async.delay(1000), Async.reject(error)]);

		var done = Assert.createAsync(
			() -> {
				all.catchError(e -> Assert.equals(e, error));
				all.then(() -> Assert.fail());
			},
			1000
		);
		all.start()
			.finally(done);
	}

	public function testWrap() {
		var complete:(Int)->Void = null;
		var async = Async.wrap((resolve, reject) -> complete = resolve);
		async.then(v -> Assert.equals(10, v))
			.catchError(e -> Assert.fail());
		complete(10);

		var fail = null;
		var async = Async.wrap((resolve, reject) -> fail = reject);
		async.catchError(e -> Assert.equals('error', e))
			.then(v -> Assert.fail());
		fail('error');
	}

	public function testWrapVoid() {
		var complete:()->Void = null;
		var async = Async.wrapVoid((resolve, reject) -> complete = resolve);
		async.then(() -> Assert.pass())
			.catchError(e -> Assert.fail());
		complete();

		var fail = null;
		var async = Async.wrap((resolve, reject) -> fail = reject);
		async.catchError(e -> Assert.equals('error', e))
			.then(v -> Assert.fail());
		fail('error');
	}
}