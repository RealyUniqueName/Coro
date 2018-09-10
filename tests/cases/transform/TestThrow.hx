package cases.transform;

import utest.Assert;
import coro.Coroutine;

class TestThrow extends TransformCase {
	public function testThrow() {
		var coroutine = new Dummy<Void>(() -> throw suspend(1));
		coroutine.run();
		try {
			coroutine.run(new DummyException('error'));
			Assert.fail();
		} catch(e:DummyException) {}

		Assert.equals(Interrupted, coroutine.getState());
		Assert.same([1], markers);
	}
}

private class DummyException extends haxe.Exception {}