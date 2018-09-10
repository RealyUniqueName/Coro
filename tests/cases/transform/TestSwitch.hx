package cases.transform;

import utest.Assert;

private enum DummyEnum {
	One(s:String);
	Two(i:Int);
}

class TestSwitch extends TransformCase {
	public function testConditionSuspends() {
		var coroutine = new Dummy<Void>(() -> {
			switch(suspend(1)) {
				case One(_): markers.push(2);
				case Two(_): markers.push(3);
			}
		});
		coroutine.run();
		coroutine.run(Two(0));

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 3], markers);
	}

	public function testCaseSuspends_goThroughNonSuspendingCase() {
		var value = Two(0);
		var coroutine = new Dummy<Void>(() -> {
			switch(value) {
				case One(_):
					markers.push(1);
				case Two(_):
					suspend(2);
					markers.push(3);
			}
		});
		coroutine.run();
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([2, 3], markers);
	}

	public function testCaseSuspends_goThroughSuspendingCase() {
		var value = One('');
		var coroutine = new Dummy<Void>(() -> {
			switch(value) {
				case One(_):
					markers.push(1);
				case Two(_):
					suspend(2);
					markers.push(3);
			}
		});
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1], markers);
	}

	public function testDefaultSuspends_goThroughDefault() {
		var value = Two(0);
		var coroutine = new Dummy<Void>(() -> {
			switch(value) {
				case Two(i) if(i == 1):
					markers.push(1);
				default:
					suspend(2);
					markers.push(3);
			}
		});
		coroutine.run();
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([2, 3], markers);
	}

	public function testDefaultSuspends_goThroughNonSuspendingCase() {
		var value = Two(1);
		var coroutine = new Dummy<Void>(() -> {
			switch(value) {
				case Two(i) if(i == 1):
					markers.push(1);
				default:
					suspend(2);
					markers.push(3);
			}
		});
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1], markers);
	}

	public function testEverythingSuspends_goThroughCase() {
		var coroutine = new Dummy<Void>(() -> {
			switch(suspend(1)) {
				case Two(i) if(i == 1):
					suspend(2);
					markers.push(3);
				default:
					suspend(4);
					markers.push(5);
			}
			markers.push(6);
		});
		coroutine.run();
		coroutine.run(Two(1));
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3, 6], markers);
	}

	public function testEverythingSuspends_goThroughDefault() {
		var coroutine = new Dummy<Void>(() -> {
			switch(suspend(1)) {
				case Two(i) if(i == 1):
					suspend(2);
					markers.push(3);
				default:
					suspend(4);
					markers.push(5);
			}
			markers.push(6);
		});
		coroutine.run();
		coroutine.run(Two(0));
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 4, 5, 6], markers);
	}
}