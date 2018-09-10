package cases.transform;

import utest.Assert;
import coro.Coroutine;

class TestThrowingStateMachine extends TransformCase {
	public function testThrowOnResume() {
		var coroutine = new ThrowingDummy(() -> {
			markers.push(1);
			suspend(2);
			markers.push(3);
		});

		coroutine.run();
		coroutine.setError(new DummyException('Terrible error!'));
		try {
			coroutine.run();
		} catch(e:DummyException) {
			Assert.same([1, 2], markers);
			Assert.equals(Interrupted, coroutine.getState());
			return;
		}
		Assert.fail();
	}
}

private class DummyException extends haxe.Exception {}

private class ThrowingDummy extends ThrowingStateMachine {
	public var run(default,null):()->Int;

	public function new(userDefined:Coroutine<()->Void>, ?generated:Generated<(sm:ThrowingDummy)->Int>) {
		super();
		run = () -> generated(this);
	}

	public function setError(error:Any) {
		throwOnResume = error;
	}

	public function getState():Int {
		return state;
	}
}