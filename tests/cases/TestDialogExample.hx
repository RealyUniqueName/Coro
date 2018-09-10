package cases;

import coro.Coroutine;
import utest.Assert;
import haxe.Timer;

/**
 * @see https://github.com/nadako/haxe-coroutines#motivation
 */
class TestDialogExample extends BaseCase {

	function greet() return new Dialog(() -> {
		speak("What's your name?");
		var name = listen();
		speak('Nice to meet you, $name!');
		wait(1000);
		explode();
	});

	public function testDialog() {
		var dialog = greet();

		var lastSpeech = null;
		var lastSpeechTime = 0.0;
		var waitDuration = 0;

		dialog.onSpeech = msg -> {
			if(msg == "What's your name?") {
				dialog.answer('Link');
				dialog.onSpeech = msg -> {
					lastSpeech = msg;
					lastSpeechTime = Timer.stamp();
				}
			}
		}

		dialog.onExplode = () -> waitDuration = Math.round((Timer.stamp() - lastSpeechTime) * 1000);

		var done = Assert.createAsync(
			() -> {
				Assert.equals('Nice to meet you, Link!', lastSpeech);
				Assert.isTrue(1000 <= waitDuration && waitDuration <= 1010); //1010 to give some time for inaccuracy
			},
			5000
		);

		dialog.onFinish = done;
		// dialog.start();
	}

	@:suspend
	function speak(msg:String, ?dialog:Dialog):Void {
		Timer.delay(
			() -> {
				dialog.onSpeech(msg);
				dialog.resume(null);
			},
			//Speech duration depends on the length of the message
			msg.length * 5
		);
	}

	@:suspend
	function listen(?dialog:Dialog):String {
		//This return is required just to satisfy Haxe type system.
		//The value of this return is never used. Whatever passed to the `dialog.resume(here)` is returned instead.
		return null;
	}

	@:suspend
	function wait(time:Int, ?dialog:Dialog):Void {
		Timer.delay(dialog.resume.bind(null), time);
	}

	@:suspend
	function explode(?dialog:Dialog):Void {
		dialog.onExplode();
		//Explosion duration: 100ms
		Timer.delay(dialog.onFinish, 100);
	}
}

private class Dialog extends StateMachine {
	@:allow(cases.TestDialogExample)
	var resume(default,null):(resumeValue:Any)->Int;

	public function new(coro:Coroutine<()->Void>, ?generated:Generated<(sm:Dialog, resumeValue:Any)->Int>) {
		super();
		resume = resumeValue -> generated(this, resumeValue);
		//start dialog right away
		resume(null);
	}

	public function answer(msg:String):Void {
		//Answer duration depends on the length of the message
		Timer.delay(resume.bind(msg), msg.length * 5);
	}

	public dynamic function onSpeech(msg:String):Void {}

	public dynamic function onExplode():Void {}

	public dynamic function onFinish():Void {}
}