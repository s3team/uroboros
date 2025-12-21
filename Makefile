# Local CI or Regression Testing

all:
	test/test_all.py -a -c || true
	test/test_instrument.sh 2>&1 | tee test.instrument || true
	rm src/fun.o || true
	find $(pwd)/src/points -maxdepth 1 ! -name '.gitignore' -type f -perm /111 -exec rm -v {} + || true
	test/test_action.sh 2>&1 | tee test.all || true
	test/test_coreutils.sh 2>&1 | tee test.coreutils

clean:
	rm src/fl_calls || true
	rm src/fl || true
	rm src/faddr.txt.0 || true
	rm src/faddr_old.txt.0 || true
	find . -type f -name "*.help" -delete || true
	find . -type f -name "*.res" -delete || true
	find . -type f -name "*.out" -delete || true
	( cd src && ls | grep '\.info$$' | xargs -r rm -f || true )
	( cd src && ls | grep '\.disassemble$$' | xargs -r rm -f || true )
	( cd src && ls | grep '\.temp$$' | xargs -r rm -f || true )
	( cd src && ls | grep '\.error$$' | xargs -r rm -f || true )
	( cd src && ls | grep '\.s$$' | xargs rm || true )
	( cd src && ls | grep '\.txt$$' | xargs -r rm -f || true )
	( cd src && ls | grep '\.sym$$' | xargs -r rm -f || true )
	( cd src && ls | grep '\.debug$$' | xargs -r rm -f || true )
	( cd src && ls | grep '\.arm32$$' | xargs -r rm -f || true )
	( cd src && ls | grep '\.thumb$$' | xargs -r rm -f || true )
	( cd src && find . -maxdepth 1 -type f -executable ! -name 'uroboros.py' ! -name 'arm_postprocess.py' -delete )
