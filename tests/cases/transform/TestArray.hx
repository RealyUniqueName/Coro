package cases.transform;

import utest.Assert;

class TestArray extends TransformCase {
	public function testAccess() {
		var coroutine = new Dummy<Int>(() -> suspend()[suspend()]);
		coroutine.run();
		coroutine.run([1, 2]);
		coroutine.run(1);

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(2, coroutine.getReturnValue());
	}

	public function testDeclaration() {
		var value = 999;
		var coroutine = new Dummy<Array<Int>>(() -> [suspend(1), value, suspend(2)]);
		coroutine.run();
		value = 20;
		coroutine.run(10);
		value = 999;
		coroutine.run(30);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2], markers);
		Assert.same([10, 20, 30], coroutine.getReturnValue());
	}
}