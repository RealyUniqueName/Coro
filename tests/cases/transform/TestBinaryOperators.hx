package cases.transform;

import coro.Coroutine.Suspend;
import utest.Assert;

class TestBinaryOperators extends TransformCase {

	public function testLeftHandSuspend() {
		var leftAdd = 999;
		var coroutine = new Dummy<Bool>(() -> suspend(1) + leftAdd == 10);
		coroutine.run();
		leftAdd = 5;
		coroutine.run(5);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1], markers);
		Assert.isTrue(coroutine.getReturnValue());
	}

	public function testRightHandSuspend() {
		var left = 10;
		var rightAdd = 5;
		var coroutine = new Dummy<Bool>(() -> left == rightAdd + suspend(1));
		coroutine.run();
		left = 999;
		rightAdd = -999;
		coroutine.run(5);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1], markers);
		Assert.isTrue(coroutine.getReturnValue());
	}

	public function testExecutionOrder_bothSidesSuspend() {
		var leftAdd = 5;
		var rightAdd = 999;
		var coroutine = new Dummy<Bool>(() -> leftAdd + suspend(1) + leftAdd == rightAdd + suspend(2) + rightAdd);
		coroutine.run();
		leftAdd = 1;
		rightAdd = 5;
		coroutine.run(5);
		rightAdd = 2;
		coroutine.run(4);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2], markers);
		Assert.isTrue(coroutine.getReturnValue());
	}

	public function testAssign_localVar() {
		var value = 999;
		var coroutine = new Dummy<Void>(() -> value = suspend());
		coroutine.run();
		coroutine.run(10);

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(10, value);
	}

	public function testAssign_arrayIndex() {
		var value = [999];
		var suspendArray:Suspend<(Int)->Array<Int>> = i -> {
			markers.push(i);
			return value;
		}
		var coroutine = new Dummy<Void>(() -> suspendArray(1)[suspend(2)] = suspend(3));
		coroutine.run();
		coroutine.run(value);
		coroutine.run(0);
		coroutine.run(10);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3], markers);
		Assert.equals(10, value[0]);
	}

	public function testAssign_instanceField() {
		var value = new Inst();
		var coroutine = new Dummy<Void>(() -> value.field = suspend());
		coroutine.run();
		coroutine.run(10);

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(10, value.field);
	}

	public function testAssign_staticField() {
		var coroutine = new Dummy<Void>(() -> Inst.staticField = suspend());
		coroutine.run();
		coroutine.run(10);

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(10, Inst.staticField);
	}

	public function testAssignOp_staticField() {
		Inst.staticField = 5;
		var coroutine = new Dummy<Void>(() -> Inst.staticField += suspend());
		coroutine.run();
		Inst.staticField = 999;
		coroutine.run(5);

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(10, Inst.staticField);
	}

	public function testAssignOp_instanceField() {
		var value = new Inst();
		value.field = 5;
		var coroutine = new Dummy<Void>(() -> suspend(1).field += suspend(2));
		coroutine.run();
		coroutine.run(value);
		value.field = 999;
		coroutine.run(5);

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(10, value.field);
	}

	public function testAssignOp_arrayIndex() {
		var value = [5];
		var coroutine = new Dummy<Void>(() -> suspend(1)[suspend(2)] += suspend(3));
		coroutine.run();
		coroutine.run(value);
		coroutine.run(0);
		value[0] = 999;
		coroutine.run(5);
		Assert.isTrue(coroutine.isFinished());
		Assert.same([1, 2, 3], markers);
		Assert.equals(10, value[0]);
	}

	public function testAssignOp_localVar() {
		var value = 6;
		var coroutine = new Dummy<Int>(() -> value += suspend());
		coroutine.run();
		value = 999;
		coroutine.run(4);

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(10, value);
	}

	public function testRightHandSuspend_leftIsLocalVarModifiedInRight() {
		var value = 20;
		var coroutine = new Dummy<Bool>(() -> {
			var a = value;
			//    20 == 10       + 4         +  3          + 3
			return a == (a = 10) + suspend() + (a = value) + a;
		});
		coroutine.run();
		value = 3;
		coroutine.run(4);

		Assert.isTrue(coroutine.isFinished());
		Assert.isTrue(coroutine.getReturnValue());
	}

	public function testOr_leftTrueRightSuspends_suspendNotExecuted() {
		var value = true;
		var coroutine = new Dummy<Bool>(() -> value || suspend(1));
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(true, coroutine.getReturnValue());
		Assert.same([], markers);
	}

	public function testAnd_leftFalseRightSuspends_suspendNotExecuted() {
		var value = false;
		var coroutine = new Dummy<Bool>(() -> value && suspend(1));
		coroutine.run();

		Assert.isTrue(coroutine.isFinished());
		Assert.equals(false, coroutine.getReturnValue());
		Assert.same([], markers);
	}
}

private class Inst {
	static public var staticField:Int = 0;
	public var field:Int = 0;
	public function new() {}
}