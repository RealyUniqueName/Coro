package cases.transform;

import utest.Assert;

class TestField extends TransformCase {
	public function testField() {
		var coroutine = new Dummy<Int>(() -> suspend().value);
		coroutine.run();
		coroutine.run({value:10});

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(10, coroutine.getReturnValue());
	}

	public function testSubField() {
		var coroutine = new Dummy<Int>(() -> suspend().field.sub);
		coroutine.run();
		coroutine.run({field:{sub:10}});

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(10, coroutine.getReturnValue());
	}
}