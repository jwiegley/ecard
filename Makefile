test-coverage:
	rm -f lcov.info
	UNDERCOVER_FORCE=true emacs -batch -L .	\
	    -l ecard-test-setup.el		\
	    -l ecard-test.el			\
	    -l ecard-carddav-map-test.el	\
	    -l ecard-carddav-test.el		\
	    -l ecard-compat-test.el		\
	    -l ecard-display-test.el		\
	    -l ecard-org-test.el		\
	    -l ecard-regression-test.el		\
	    -l ecard-sync-test.el		\
	    -l ecard-tools-test.el		\
	    -l ecard-widget-test.el		\
	    -f ert-run-tests-batch-and-exit
