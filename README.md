# Coro

This library is the Haxe compiler plugin which provides generic coroutines implementation.

The library also includes async/await and generators implementations on top of the plugin.

# Installation

To use this plugin you will need to setup Haxe for development (see [Building Haxe from source](https://haxe.org/documentation/introduction/building-haxe.html))
And then:
```
$ git clone https://github.com/RealyUniqueName/coro.git
$ cd coro
$ haxelib dev coro .
$ cd path/to/dev/haxe
$ make PLUGIN=path/to/coro/src/ml/coro_plugin plugin
```
Now to use Coro all you need to do is to add `-lib coro` to your compilation flags.

# Generator

```haxe
import coro.Generator;

class Test {
	static public function main() {
		for(n in fibonacci(5)) {
			trace(n); // 1, 1, 2, 3, 5
		}
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
```

# Async/await

`coro.Async` mimics the `js.Promise` api.

Additionally it has `Async.wrap()` and `Async.wrapVoid` methods which can be used to transition from the 3rd-party
asynchronous API to the `coro.Async`

```haxe
import coro.Async;

class Test {
	static public function main() {
		md5WebPage('http://example.com').start()
			.then(md5 -> trace(md5)) 		//print the calculated hash in case of success
			.catchError(msg -> trace(msg)); //print error message in case of failure
	}

	static function md5WebPage(url:String) return new AsyncValue(() -> {
		//Wait a second. Just because I can.
		Async.delay(1000).await();
		try {
			var contents = request(url).await();
			return haxe.crypto.Md5.encode(contents);
		} catch(error:String) {
			trace(error); //Log error message
			throw error; //rethrow
		}
	});

	static function request(url:String) return Async.wrap((resolve, reject) -> {
		var http = new haxe.Http(url);
		http.onData = resolve;
		http.onError = error -> reject(error);
		http.request();
	});
}
```

### Awaiting ES6 promises

```haxe
using coro.Async;

function greet(promise:js.Promise<String>) return new Async(() -> {
	var name = promise.await();
	trace('Hello, $name!');
});
```

### Converting Async to ES6 promise

```haxe
using coro.Async;

function delayGreet(name:String):Promise<String> {
	var async = new AsyncValue<String>(() -> {
		Async.delay(1000).await();
		return name;
	});

	//convert coroutine to promise
	return async.promise();
}
```


# Pipe

Pipe is the coroutine, which allows two-way communication between the coroutine caller and the coroutine itself.

```haxe
import coro.Pipe;

class Test {
	static public function main() {
		var repl = getRepl();
		while(true) {
			Sys.println('Type a command: ');
			var cmd = input();
			try {
				//send the command to REPL and get the result back
				var result = repl.send(cmd);
				Sys.println('Result: $result');
			} catch(e:ClosedPipeException) {
				//Pipe closed with the "exit" command
				break;
			} catch(e:Dynamic) {
				Sys.println('Error: $e');
				return;
			}
		}
		//Get the value returned with `return value` expression in the Pipe
		var cmdCount = repl.getResult();
		Sys.println('Total commands executed: $cmdCount');
	}

	static function getRepl() return new Pipe<Int,String,Int>(yield -> {
		var parser = ~/^(\d+)\s*\+\s*(\d+)$/; //Allows "123 + 456"
		var counter = 0;
		var command = yield(0);
		while(command != 'exit') {
			++counter;
			if(parser.match(command)) {
				var eval = Std.parseInt(parser.matched(1)) + Std.parseInt(parser.matched(2));
				//Send the evaluated value to the caller and wait for the next command
				command = yield(eval);
			} else {
				throw 'Invalid command: $command';
			}
		}
		return counter; //this value can be accessed with the `Pipe.getResult()` method
	});

	static function input():String {
		var result = '';
		var c = Sys.getChar(true);
		while(c != 13) {
			result += String.fromCharCode(c);
			c = Sys.getChar(true);
		}
		Sys.print('\n');
		return result;
	}
}
```

# Arbitrary coroutine example

Every call in this example suspends execution of the `greet()`:
```haxe
function greet() return new Dialog(() -> {
	speak("What's your name?");
	var name = listen();
	speak('Nice to meet you, $name!');
	wait(1000);
	explode();
});
```

See https://github.com/RealyUniqueName/Coro/blob/master/tests/cases/TestDialogExample.hx#L10

# Implementation details

The plugin operates on the Typed Syntax Tree. It is executed after typing step of the compiler and before the optimization step.

The plugin transforms local function declarations to state machines if passed to the argument
of `coro.Coroutine` type followed by the optional argument of `coro.Coroutine.Generated` type.
Transformed function is then passed to the argument of `Generated` type while the original function
is replaced with `null`.
Function arguments list gets appended with the one or two new arguments: `sm` or `sm, resumeValue`

E.g. if the signature of `generator` function is
```haxe
	function generator(
		userFunction:Coroutine<yield:YieldType->ReturnType>,
		?genFunction:Generated<(yield:YieldType, sm:StateMachineType, resumeValue:ResumeType)->Void>
	)
```
then this expression
```haxe
	generator(yield -> {/* body */});
```
is transformed to
```haxe
	generator(null, (yield, sm, resumeValue) -> {/* transformed body */});
```

Such approach was chosen to stay in the boundaries of the Haxe type system.

The benefits are

* Full compiler-based completion support;
* No macros;
* No auto generated types or fields;

The downside is impossibility to invoke super methods in a coroutine.

The coroutine body is split into states by suspending calls.
A function is considered suspending if the signature of a callee is `coro.Suspend<T:Function>` (e.g. `coro.Suspend<()->Void>`)
or if the callee is a method with `@:suspend` meta applied to it.

Now for example the fibonacci generator mentioned above is transformed to the following state machine:
```haxe
static function fibonacci(iterations:Int) {
	var previous;
	var current;
	return new coro.Generator(null, function(yield:coro.Suspend<(Int)->Void>, sm:coro.Generator<Int>) {
		var state = sm.state;
		//if an exception will raise, the state machine will be left in `Interrupted` state
		//which is `-2`. See `coro.Coroutine.StateExitCode` enum.
		sm.state = -2;
		while (true)
			if (state == 0) {
				current = 1;
				previous = 0;
				state = 1;
			} else if (state == 1) {
				if (iterations-- > 0) {
					sm.nextState = 2;
					yield(current);
					//"yield" is the suspending function, so the `return` is generated to suspend execution
					return sm.state = sm.nextState;
				} else
					state = 3;
			} else if (state == 2) {
				var next = current + previous;
				previous = current;
				current = next;
				state = 1;
			} else if (state == 3)
				return sm.state = sm.nextState = -1
			else
				throw new coro.CoroutineStateException(state);
	});
}
```

# Tests

To run test:

For eval: `$ haxe tests.hxml --interp`

For js: `$ haxe tests.hxml -js bin/test.js && node bin/test.js`

For java: `$ haxe tests.hxml -java bin/java && java -jar bin/java/Tests-Debug.jar`

etc.