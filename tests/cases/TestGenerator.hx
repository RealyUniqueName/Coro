package cases;

import coro.Generator;
import utest.Assert;
import coro.Generator.*;

class TestGenerator extends BaseCase {

	public function testYield() {
		var generator = new Generator<Int>(yield -> {
			yield(10);
			yield(20);
		});
		var result = [for(i in generator) i];

		Assert.same([10, 20], result);
	}

	public function testYieldAll() {
		var generator = new Generator<Int>(yield -> {
			yieldAll([10, 20]);
		});
		var result = [for(i in generator) i];

		Assert.same([10, 20], result);
	}

	public function testFib() {
		var result = [];
		for(n in fibonacci(6)) {
			result.push(n);
		}
		Assert.same([1, 1, 2, 3, 5, 8], result);
	}

	static function fibonacci(iterations:Int) return new Generator<Int>(yield -> {
		var current = 1;
		var previous = 0;
		while(iterations-- > 0) {
			yield(current);
			var next = current + previous;
			previous = current;
			current = next;
		}
	});
}