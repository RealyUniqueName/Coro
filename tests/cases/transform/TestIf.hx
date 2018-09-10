package cases.transform;

import utest.Assert;

class TestIf extends TransformCase {

	public function testCondition() {
		var coroutine = new Dummy<Void>(() -> {
			if(suspend()) {
				markers.push(1);
			}
			if(suspend()) {
				markers.push(2);
			}
		});
		coroutine.run();
		coroutine.run(false);
		coroutine.run(true);

		Assert.same([2], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testIfBlock() {
		var condition = true;
		var coroutine = new Dummy<Void>(() -> {
			if(condition) {
				suspend();
				markers.push(1);
			}
			if(!condition) {
				suspend();
				markers.push(2);
			}
			markers.push(3);
		});
		coroutine.run();
		coroutine.run();

		Assert.same([1, 3], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testElseBlock() {
		var condition = false;
		var coroutine = new Dummy<Void>(() -> {
			if(condition) {
				markers.push(1);
			} else {
				suspend();
				markers.push(2);
			}
			if(!condition) {
				markers.push(3);
			} else {
				suspend();
				markers.push(4);
			}
			markers.push(5);
		});
		coroutine.run();
		coroutine.run();

		Assert.same([2, 3, 5], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testAsValue_elseSuspendsButNotExecuted() {
		var condition = true;
		var coroutine = new Dummy<Bool>(() -> if(condition) true else suspend(1));
		coroutine.run();

		Assert.same([], markers);
		Assert.isTrue(coroutine.getReturnValue());
		Assert.isTrue(coroutine.isFinished());
	}

	public function testAsValue_ifSuspendsButNotExecuted() {
		var condition = false;
		var coroutine = new Dummy<Bool>(() -> if(condition) suspend(1) else true);
		coroutine.run();

		Assert.same([], markers);
		Assert.isTrue(coroutine.getReturnValue());
		Assert.isTrue(coroutine.isFinished());
	}
}