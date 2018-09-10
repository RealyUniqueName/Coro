package cases.transform;

import utest.Assert;
import coro.Coroutine;

class TestParenthesis extends TransformCase {

	public function testParenthesis() {
		var coroutine = new Dummy<Int>(() -> (suspend(1) + 2) * 2);
		coroutine.run();
		coroutine.run(2);

		Assert.equals(8, coroutine.getReturnValue());
		Assert.same([1], markers);
		Assert.isTrue(coroutine.isFinished());
	}
}