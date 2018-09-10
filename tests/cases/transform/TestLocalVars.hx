package cases.transform;

import utest.Assert;

class TestLocalVars extends TransformCase {

	public function testDeclaration() {
		var coroutine = new Dummy<Void>(() -> {
			var v = pushSuspend(1);
			markers.push(v);
		});
		coroutine.run();
		coroutine.run(2);

		Assert.same([1, 2], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testAcrossStates() {
		var coroutine = new Dummy<Void>(() -> {
			var v = 1;
			pushSuspend(v);
			markers.push(v + 1);
		});
		coroutine.run();
		coroutine.run();

		Assert.same([1, 2], markers);
		Assert.isTrue(coroutine.isFinished());
	}
}