import utest.UTest;

import utest.Runner;
import utest.ui.Report;
import utest.ui.common.HeaderDisplayMode;

class Tests {
	static public function main() {
		var runner = new Runner();
		// runner.addCase(new cases.TestAsync());
		runner.addCases('cases');
		var report = Report.create(runner);
		report.displaySuccessResults = NeverShowSuccessResults;
		report.displayHeader = AlwaysShowHeader;
		runner.run();
	}
}