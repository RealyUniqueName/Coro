package cases.transform;

import utest.Assert;

class TestNew extends TransformCase {
	public function testNew() {
		var value = 10;
		var coroutine = new Dummy<Constr>(() -> new Constr(value, suspend(1)));
		coroutine.run();
		value = 999;
		coroutine.run(20);

		Assert.isTrue(coroutine.isFinished());
		Assert.same([1], markers);
		Assert.equals(10, coroutine.getReturnValue().arg1);
		Assert.equals(20, coroutine.getReturnValue().arg2);
	}
}

private class Constr {
	public var arg1:Int;
	public var arg2:Int;

	public function new(arg1:Int, arg2:Int) {
		this.arg1 = arg1;
		this.arg2 = arg2;
	}
}