# Local CI or Regression Testing

all:
	test/test_all.py -a -c
	test/test_instrument.sh 2>&1 | tee test.instrument
	rm src/points.ins src/fun.o || true
	test/test_action.sh 2>&1 | tee test.all
	test/test_coreutils.sh 2>&1 | tee test.coreutils

clean:
	rm src/fl_calls || true
	rm src/fl || true
	find . -type f -name "*.help" -delete
	find . -type f -name "*.res" -delete
	find . -type f -name "*.out" -delete
	( cd src && ls | grep '\.info$$' | xargs -r rm -f )
	( cd src && ls | grep '\.disassemble$$' | xargs -r rm -f )
	( cd src && ls | grep '\.temp$$' | xargs -r rm -f )
	( cd src && ls | grep '\.error$$' | xargs -r rm -f )
	( cd src && ls | grep '\.s$$' | xargs rm )
	( cd src && ls | grep '\.txt$$' | xargs -r rm -f )
	( cd src && ls | grep '\.sym$$' | xargs -r rm -f )
	( cd src && find . -maxdepth 1 -type f -executable ! -name 'uroboros.py' -delete )
