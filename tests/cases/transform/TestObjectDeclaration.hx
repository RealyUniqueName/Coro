package cases.transform;

import utest.Assert;
import coro.Coroutine;

class TestObjectDeclaration extends TransformCase {

	public function testDeclaration() {
		var value = 999;
		var coroutine = new Dummy<{field1:Int, field2:Int, field3:Int}>(() -> {
			field1:suspend(1),
			field2:value,
			field3:suspend(2)
		});
		coroutine.run();
		value = 20;
		coroutine.run(10);
		value = 999;
		coroutine.run(30);

		Assert.same({field1:10, field2:20, field3:30}, coroutine.getReturnValue());
		Assert.same([1, 2], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testEmptyObject() {
		var coroutine = new Dummy<{}>(() -> {});
		coroutine.run();

		Assert.same({}, coroutine.getReturnValue());
		Assert.isTrue(coroutine.isFinished());
	}
}