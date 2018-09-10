package cases.transform;

import utest.Assert;

class TestBlock extends TransformCase {

	public function testNoSuspension() {
		var coroutine = new Dummy<Void>(() -> {
			markers.push(1);
		});
		coroutine.run();

		Assert.same([1], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testNonVoidLastExpr() {
		var coroutine = new Dummy<Void>(() -> {
			markers.push(1);
			pushSuspend(2);
			markers.push(3);
		});
		coroutine.run();
		markers.push(4);
		coroutine.run();

		Assert.same([1, 2, 4, 3], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testVoidLastExpr() {
		var pushVoid:(Int)->Void = markers.push;

		var coroutine = new Dummy<Void>(() -> {
			markers.push(1);
			pushSuspend(2);
			pushVoid(3);
		});
		coroutine.run();
		markers.push(4);
		coroutine.run();

		Assert.same([1, 2, 4, 3], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testVoidFunction() {
		var coroutine = new Dummy<Void>(function():Void {
			markers.push(1);
			pushSuspend(2);
			markers.push(3);
		});
		coroutine.run();
		markers.push(4);
		coroutine.run();

		Assert.same([1, 2, 4, 3], markers);
		Assert.isTrue(coroutine.isFinished());
	}
}