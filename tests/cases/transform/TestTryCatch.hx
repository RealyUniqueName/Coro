package cases.transform;

import utest.Assert;
import coro.Coroutine;

class TestTryCatch extends TransformCase {

	public function testTrySuspends_noException() {
		var coroutine = new Dummy<Void>(() -> {
				markers.push(1);
				try {
					markers.push(2);
					suspend(3);
					markers.push(4);
				} catch(e:Int) {
					Assert.fail();
				}
				markers.push(5);
			}
		);
		coroutine.run();
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 4, 5], markers);
	}

	public function testTrySuspends_exception() {
		var condition = true;
		var coroutine = new Dummy<Void>(() -> {
				markers.push(1);
				try {
					markers.push(2);
					suspend(3);
					if(condition) throw 4;
					markers.push(5);
				} catch(e:Int) {
					markers.push(e);
				}
				markers.push(6);
			}
		);
		coroutine.run();
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 4, 6], markers);
	}

	public function testCatchSuspends_noException() {
		var coroutine = new Dummy<Void>(() -> {
				markers.push(1);
				try {
					markers.push(2);
				} catch(e:Int) {
					markers.push(3);
					suspend(4);
					markers.push(5);
				}
				markers.push(6);
			}
		);
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 6], markers);
	}

	public function testCatchSuspends_exception() {
		var condition = true;
		var coroutine = new Dummy<Void>(() -> {
				markers.push(1);
				try {
					markers.push(2);
					if(condition) throw 3;
					markers.push(4);
				} catch(e:Int) {
					markers.push(5);
					suspend(e);
					markers.push(6);
				}
				markers.push(7);
			}
		);
		coroutine.run();
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 5, 3, 6, 7], markers);
	}

	public function testCatch_throwUnexpectedExceptionType() {
		var condition = true;
		var coroutine = new Dummy<Void>(() -> {
				markers.push(1);
				try {
					markers.push(2);
					if(condition) throw "error";
					markers.push(4);
				} catch(e:Int) {
					markers.push(5);
					suspend(e);
					markers.push(6);
				}
				markers.push(7);
			}
		);

		Assert.raises(() -> coroutine.run(), String);
		Assert.equals(Interrupted, coroutine.getState());
		Assert.same([1, 2], markers);
	}

	public function testMultipleCatch() {
		var condition = true;
		var coroutine = new Dummy<Void>(() -> {
				markers.push(1);
				try {
					suspend(2);
					if(condition) throw "error";
					markers.push(3);
				} catch(e:Int) {
					markers.push(4);
				} catch(e:String) {
					markers.push(5);
				}
				markers.push(6);
			}
		);
		coroutine.run();
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 5, 6], markers);
	}

	public function testNestedTry() {
		var condition = true;
		var coroutine = new Dummy<Void>(() -> {
				markers.push(1);
				try {
					suspend(2);
					try {
						if(condition) throw 3;
						markers.push(4);
					} catch(e:Int) {
						suspend(e);
						if(condition) throw "error";
						markers.push(6);
					}
					markers.push(7);
				} catch(e:String) {
					markers.push(8);
				}
				markers.push(9);
			}
		);
		coroutine.run();
		coroutine.run();
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 8, 9], markers);
	}
}