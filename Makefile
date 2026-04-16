# Local CI or Regression Testing + Cleaning

intel:
	test/test_all.py -a -c --arch intel || true
	test/test_instrument.intel.sh 2>&1 | tee test.intel.instrument || true
	rm $(CURDIR)/src/fun.o || true
	find $(CURDIR)/src/points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} + || true
	test/test_action.sh intel 2>&1 | tee test.intel.handmade

intel.coreutils:
	test/test_coreutils.sh 2>&1 | tee test.intel.coreutils

arm.thumb:
	test/test_all.py -a -c --arch arm || true
	test/test_instrument.arm.thumb.sh 2>&1 | tee test.arm.thumb.instrument || true
	rm $(CURDIR)/src/fun.o || true
	find $(CURDIR)/src/points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +

arm.64:
	test/test_all.py -a -c --arch arm || true
	test/test_instrument.arm.64.sh 2>&1 | tee test.arm.64.instrument || true
	rm $(CURDIR)/src/fun.o || true
	find $(CURDIR)/src/points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +

arm.handmade:
	test/test_action.sh arm 2>&1 | tee test.arm.handmade

clean:
	rm src/fl_calls || true
	rm src/fl || true
	rm src/faddr.txt.0 || true
	rm src/faddr_old.txt.0 || true
	rm src/info.enc || true
	rm src/private.key || true
	rm src/public_key.h || true
	find . -type f -name "*.help" -delete || true
	find . -type f -name "*.res" -delete || true
	find . -type f -name "*.out" -delete || true
	find . -maxdepth 1 -type f -name "test.*" -delete || true
	find src -maxdepth 1 -name '*.result' -delete || true
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
	( cd src && find . -maxdepth 1 -type f -perm /111 ! -name 'uroboros.py' ! -name 'arm_postprocess.py' -delete )
