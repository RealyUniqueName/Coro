package coro;

import haxe.Constraints.Function;
import haxe.Exception;

typedef Coroutine<T:Function> = T;
typedef Generated<T:Function> = T;

typedef Suspend<T:Function> = T;

class CoroutineException extends Exception {}

class CoroutineStateException extends CoroutineException {
	public var state(default,null):Int;

	public function new(state:Int, ?msg:String) {
		super(msg == null ? 'Invalid state $state' : msg);
		this.state = state;
	}
}

enum abstract StateExitCode(Int) from Int to Int {
	/** Coroutine stopped working after `return` or after executing the last expression. */
	var Finished = -1;
	/** Coroutine was interrupted unexpectedly. E.g. because of an uncaught exception. */
	var Interrupted = -2;

	//These values are also hard-coded in coro_plugin.ml
}

class StateMachine {
	var state:Int = 0;
	/**
	 *  Assigned before executing a suspending call.
	 *  During the time coroutine is suspended `nextState` equals `state`.
	 *  Suspending calls are transformed like this:
	 *  ```
	 *  suspend();
	 *  ```
	 *  into
	 *  ```
	 *  sm.nextState = <state num>;
	 *  suspend();
	 *  return sm.state = sm.nextState;
	 *  ```
	 *  So if you want to advance the state machine to the next state right away in the suspending function,
	 *  you need to `sm.state = sm.nextState` before doing so.
	 */
	var nextState:Int = 0;
	var returnValue:Null<Any>;

	function new() {}
}

/**
 * This state machine will check `throwOnResume` on before running each state and throw it if it is not `null`.
 */
class ThrowingStateMachine extends StateMachine {
	var throwOnResume:Null<Any>;
}