package cases.transform;

import utest.Assert;

class TestLoops extends TransformCase {
	public function testWhile_conditionSuspends() {
		var coroutine = new Dummy<Void>(() -> {
			while(suspend(1)) {
				markers.push(2);
			}
			markers.push(3);
		});
		coroutine.run();
		coroutine.run(true);
		coroutine.run(false);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 1, 3], markers);
	}

	public function testWhile_bodySuspends() {
		var condition = true;
		var coroutine = new Dummy<Void>(() -> {
			markers.push(1);
			while(condition) {
				suspend(2);
				markers.push(3);
			}
			markers.push(4);
		});
		coroutine.run();
		coroutine.run();
		condition = false;
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 2, 3, 4], markers);
	}

	public function testWhile_bothConditionAndBodySuspend() {
		var coroutine = new Dummy<Void>(() -> {
			while(suspend(1)) {
				suspend(2);
				markers.push(3);
			}
			markers.push(4);
		});
		coroutine.run();
		coroutine.run(true);
		coroutine.run();
		coroutine.run(false);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 1, 4], markers);
	}

	public function testDoWhile_bodySuspends() {
		var condition = true;
		var coroutine = new Dummy<Void>(() -> {
			do {
				suspend(1);
				markers.push(2);
			} while(condition);
			markers.push(3);
		});
		coroutine.run();
		coroutine.run();
		condition = false;
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 1, 2, 3], markers);
	}

	public function testDoWhile_conditionSuspends() {
		var coroutine = new Dummy<Void>(() -> {
			do {
				markers.push(1);
			} while(suspend(2));
			markers.push(3);
		});
		coroutine.run();
		coroutine.run(true);
		coroutine.run(false);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 1, 2, 3], markers);
	}

	public function testDoWhile_bothBodyAndConditionSuspend() {
		var coroutine = new Dummy<Void>(() -> {
			markers.push(1);
			do {
				suspend(2);
				markers.push(3);
			} while(suspend(4));
			markers.push(5);
		});
		coroutine.run();
		coroutine.run();
		coroutine.run(true);
		coroutine.run();
		coroutine.run(false);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 4, 2, 3, 4, 5], markers);
	}

	public function testFor() {
		var coroutine = new Dummy<Void>(() -> {
			markers.push(1);
			for(i in 0...suspend(2)) {
				suspend(3 + i);
			}
		});
		coroutine.run();
		coroutine.run(2);
		coroutine.run();
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 4], markers);
	}

	public function testBreak() {
		var coroutine = new Dummy<Void>(() -> {
			while(suspend(1)) {
				markers.push(2);
				if(suspend(3)) break;
				markers.push(4);
			}
		});
		coroutine.run();
		coroutine.run(true);
		coroutine.run(false);
		coroutine.run(true);
		coroutine.run(true);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 4, 1, 2, 3], markers);
	}

	public function testContinue_normalWhile() {
		var coroutine = new Dummy<Void>(() -> {
			while(suspend(1)) {
				markers.push(2);
				if(suspend(3)) continue;
				markers.push(4);
			}
		});
		coroutine.run();
		coroutine.run(true);
		coroutine.run(true);
		coroutine.run(true);
		coroutine.run(false);
		coroutine.run(false);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 1, 2, 3, 4, 1], markers);
	}

	public function testContinue_doWhile() {
		var coroutine = new Dummy<Void>(() -> {
			do {
				markers.push(1);
				if(suspend(2)) continue;
				markers.push(3);
			} while(suspend(4));
		});
		coroutine.run();
		coroutine.run(true);
		coroutine.run(true);
		coroutine.run(false);
		coroutine.run(false);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 4, 1, 2, 3, 4], markers);
	}

	public function testNestedLoops() {
		var coroutine = new Dummy<Void>(() -> {
			do {
				markers.push(1);
				while(true) {
					if(suspend(2)) break;
					markers.push(3);
				}
				markers.push(4);
			} while(suspend(5));
		});
		coroutine.run();
		coroutine.run(false);
		coroutine.run(true);
		coroutine.run(false);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 2, 4, 5], markers);
	}

	public function testClosureVarCapturing() {
		var callbacks = [];
		var expected = [];
		var cnt = 3;
		var coroutine = new Dummy<Void>(() -> {
			for(i in 0...cnt) {
				expected.push(i);
				callbacks.push(() -> i);
			}
		});
		coroutine.run();
		var actual = callbacks.map(fn -> fn());

		Assert.isTrue(coroutine.isFinished());
		Assert.same(expected, actual);
	}
}