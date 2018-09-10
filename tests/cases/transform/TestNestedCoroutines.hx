package cases.transform;

import utest.Assert;
import coro.Coroutine;

class TestNestedCoroutines extends TransformCase {
	public function testNested() {
		var nestedIsFinished;
		var coroutine = new Dummy<Void>(() -> {
			markers.push(1);
			var nested = new Dummy<Void>(() -> {
				markers.push(2);
				suspend(3);
				markers.push(4);
			});
			markers.push(5);
			nested.run();
			suspend(6);
			nested.run();
			markers.push(7);
			nestedIsFinished = nested.isFinished();
		});

		coroutine.run();
		coroutine.run();

		Assert.isTrue(nestedIsFinished);
		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 5, 2, 3, 6, 4, 7], markers);
	}
}