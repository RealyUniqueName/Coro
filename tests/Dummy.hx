import coro.Coroutine;

class Dummy<TReturn> extends StateMachine {
	public var run(default,null):(?resumeValue:Any)->Void;

	public function new(
		userDefined:Coroutine<()->TReturn>,
		?generated:Generated<(sm:Dummy<TReturn>, resumeValue:Any)->Int>
	) {
		super();
		run = (?resumeValue) -> generated(this, resumeValue);
	}

	public function isFinished() {
		return switch(state:StateExitCode) {
			case Finished: true;
			case _: false;
		}
	}

	public function getReturnValue():TReturn {
		return returnValue;
	}

	public function getState() {
		return state;
	}
}