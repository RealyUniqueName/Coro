package cases;

import coro.Pipe;
import utest.Assert;

class TestPipe extends BaseCase {

	public function testReplExample() {
		var repl = new Pipe(yield -> {
			var parser = ~/^(\d+) \+ (\d+)$/;
			var counter = 0;
			var command = yield(0);
			while(command != 'exit') {
				++counter;
				if(parser.match(command)) {
					var eval = Std.parseInt(parser.matched(1)) + Std.parseInt(parser.matched(2));
					command = yield(eval);
				} else {
					throw 'Invalid command: $command';
				}
			}
			return counter;
		});

		var result = [repl.current()];
		var commands = ["2 + 3", "15 + 10", "exit"];
		for(cmd in commands) {
			try {
				result.push(repl.send(cmd));
			} catch(e:ClosedPipeException) {
				break;
			}
		}

		Assert.equals(2, repl.getResult());
		Assert.same([0, 2 + 3, 15 + 10], result);
	}

	public function testGetResult_noResult_raises() {
		var pipe = new Pipe(yield -> {});
		Assert.raises(() -> pipe.getResult(), NoResultPipeException);
	}

	public function testHasResult() {
		var pipe = new Pipe<Int,Int,Int>(yield -> 999);
		Assert.isFalse(pipe.hasResult());

		pipe.sendVoid(0);
		Assert.isTrue(pipe.hasResult());
	}

	public function testIsActive() {
		var pipe = new Pipe(yield -> {
			var v = yield(10);
			v = yield(v);
			return v;
		});

		pipe.send(10);
		Assert.isTrue(pipe.isActive());

		pipe.sendVoid(20);
		Assert.isFalse(pipe.isActive());
	}
}