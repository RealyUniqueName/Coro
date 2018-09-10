package cases.transform;

import utest.Assert;

class TestUnaryOperators extends TransformCase {

	public function testPrefix() {
		var coroutine = new Dummy<Bool>(() -> !suspend());
		coroutine.run();
		coroutine.run(false);

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(true, coroutine.getReturnValue());
	}

	public function testPostfix() {
		var data = {v:0};
		var coroutine = new Dummy<Int>(() -> (suspend():{v:Int}).v++);
		coroutine.run();
		coroutine.run(data);

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(0, coroutine.getReturnValue());
		Assert.equals(1, data.v);
	}
}