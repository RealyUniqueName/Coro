package coro;

import coro.Coroutine;
#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
#end

typedef Yield<T> = Suspend<(value:T)->Void>;

class Generator<T> extends StateMachine {
	var proceed:(yield:Yield<T>, sm:Generator<T>)->Int;
	var lastYieldValue:Null<T>;

	macro static public function yieldAll(collection:Expr) {
		return macro @:pos(collection.pos) for(item in $collection) yield(item);
	}

	public function new(
		coroutine:Coroutine<(yield:Yield<T>)->Void>,
		?generated:Generated<(yield:Yield<T>, sm:Generator<T>)->Int>
	) {
		super();
		proceed = generated;
	}

	function yield(value:Null<T>) {
		lastYieldValue = value;
	}

	/**
	 * Advance generator.
	 * Returns `true` if spotted a `yield`.
	 * Returns `false` if no more `yield`s left.
	 */
	public function hasNext():Bool {
		if(state >= 0) {
			proceed(yield, this);
		}
		return state >= 0;
	}

	/**
	 * Get the latest yielded value.
	 */
	public function next():Null<T> {
		return lastYieldValue;
	}
}