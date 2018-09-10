package cases.transform;

import utest.Assert;
import coro.Coroutine;

class TestCall extends TransformCase {

	@:suspend
	function suspendArgs(i1:Int, i2:Int):Void {
		markers.push(i1);
		markers.push(i2);
	}

	public function testNonSuspendingArguments() {
		var coroutine = new Dummy<Void>(() -> suspendArgs(1, 2));
		coroutine.run();
		coroutine.run();

		Assert.same([1, 2], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testSuspendingArguments() {
		var suspendFn:Suspend<(i:Int)->Suspend<(i1:Int, i2:Int)->Void>> = i -> {
			markers.push(i);
			return null;
		}
		var coroutine = new Dummy<Void>(() -> suspendFn(1)(suspend(2), suspend(3)));
		coroutine.run();
		coroutine.run(suspendArgs);
		coroutine.run(4);
		coroutine.run(5);
		coroutine.run();

		Assert.same([1, 2, 3, 4, 5], markers);
		Assert.isTrue(coroutine.isFinished());
	}

	public function testSuspendingCall_hasStateMachineOptionalArgument() {
		var smPassed = false;
		var coroutine:Dummy<Void> = null;
		var suspend:Suspend<(?sm:StateMachine)->Void> = (?sm) -> smPassed = (sm != null && sm == coroutine);
		coroutine = new Dummy<Void>(() -> suspend());
		coroutine.run();
		coroutine.run();

		Assert.isTrue(smPassed);
	}

	function dummy(userDefined, ?generated):Dummy<Void> {
		return new Dummy<Void>(userDefined, generated);
	}

	public function testCoroutineAtCall() {
		var coroutine = dummy(() -> suspend(1));
		coroutine.run();
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1], markers);
	}
}