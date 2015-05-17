test:
	sbt 'run --backend c --compile --test --genHarness --targetDir build'

verilog:
	sbt 'run --backend v --targetDir build'
