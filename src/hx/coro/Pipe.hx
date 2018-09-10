package coro;

import coro.Coroutine;

typedef YieldPipe<TYield,TSend> = Suspend<(value:TYield)->TSend>;
typedef PipeProgram<TYield,TSend,TResult> = Coroutine<(yield:YieldPipe<TYield,TSend>)->TResult>;
typedef PipeGenerated<TYield,TSend,TResult> = Generated<(yield:YieldPipe<TYield,TSend>, sm:Pipe<TYield,TSend,TResult>, value:TSend)->Int>;

class PipeException extends haxe.Exception {}
class ClosedPipeException extends PipeException {}
class NoResultPipeException extends PipeException {}

class Pipe<TYield,TSend,TResult> extends StateMachine {
	var resume:PipeGenerated<TYield,TSend,TResult>;
	var currentYieldValue:TYield;

	public function new(coroutine:PipeProgram<TYield,TSend,TResult>, ?generated:PipeGenerated<TYield,TSend,TResult>) {
		super();
		resume = generated;
	}

	/**
	 *  Send `value` to the Pipe.
	 *  Returns the latest yielded value.
	 *
	 *  If Pipe is not started yet:
	 *    1. Advance to the first `yield()` call;
	 *    2. That first `yield()` call returns `value` into the Pipe;
	 *    3. Advance to the next `yield()` call;
	 *  In this scenario the first yielded value is lost. If you need it, use `Pipe.current()` before the first `Pipe.send()`
	 *
	 *  If Pipe is already running:
	 *    1. Return `value` into the Pipe as the result of the current `yield()` call the Pipe has stopped at;
	 *    2. Advance to the next `yield()` call.
	 *
	 *  @throws ClosedPipeException if Pipe did not yield any value.
	 */
	public function send(value:TSend):TYield {
		sendVoid(value);
		if(state < 0) {
			throw new ClosedPipeException('Pipe is closed.');
		}
		return currentYieldValue;
	}

	/**
	 * The same as `.send()` except it does not return any value and does not throw `ClosedPipeException`
	 */
	public function sendVoid(value:TSend) {
		if(state == 0) {
			resume(yield, this, null);
		}
		if(state >= 0) {
			resume(yield, this, value);
		}
	}

	/**
	 *  Get the latest yielded value.
	 *  If Pipe is not started yet, then this method advances it to the first yield.
	 *
	 *  @throws ClosedPipeException if Pipe did not yield any value.
	 */
	public function current():TYield {
		if(state == 0) {
			resume(yield, this, null);
		}
		if(state < 0) {
			throw new ClosedPipeException('Pipe is closed.');
		}
		return currentYieldValue;
	}

	/**
	 *  If Pipe finished with `return value` expression, then this method will return that value.
	 *
	 *  @throws NoResultPipeException if Pipe did not return any value.
	 */
	public function getResult():TResult {
		switch(state:StateExitCode) {
			case Finished:
				return returnValue;
			case _:
				throw new NoResultPipeException('Pipe did not return a value.');
		}
	}

	/**
	 *  Check if this Pipe finished with `return value;` expression.
	 */
	public inline function hasResult():Bool {
		return state == Finished;
	}

	/**
	 *  Check if this Pipe has no more code to run.
	 */
	public inline function isActive():Bool {
		return state >= 0;
	}

	function yield(value:TYield):TSend {
		currentYieldValue = value;
		//Actually this `return` does not pass a value to the stateMachine here.
		//The only purpose of this expression is to satisfy the type system of Haxe.
		return null;
	}
}