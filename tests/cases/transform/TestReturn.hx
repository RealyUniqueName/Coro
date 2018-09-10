package cases.transform;

import utest.Assert;

class TestReturn extends TransformCase {

	public function testVoid() {
		var condition = true;
		var coroutine = new Dummy<Void>(function() {
			if(condition) return;
			suspend();
		});
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
	}

	public function testNonVoid() {
		var condition = true;
		var coroutine = new Dummy<Int>(() -> {
			if(condition) return 10;
			suspend();
			return 20;
		});
		coroutine.run();

		Assert.same(10, coroutine.getReturnValue());
		Assert.isTrue(coroutine.isFinished());
	}
}