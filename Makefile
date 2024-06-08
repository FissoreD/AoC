NB := $(shell (seq 1 25))

create: ok_year
	$(MAKE) create_src create_tests
	dune build

# A valid year is a two digit string between 15 and current_year included
ok_year:
	$(eval lt := $(shell test $(Y) -le $$(date +%y); echo $$?))
	$(eval bt := $(shell test $(Y) -ge 15; echo $$?))
	echo $(Y) $(bt) $(lt) | grep "[1-9][0-9] 0 0"

create_tests:
	$(eval TEST_FOLDER := test/y20$(Y))
	$(eval R := $(shell echo $$(seq 1 9)))
	$(eval dune_file := $(TEST_FOLDER)/source/dune)
	@mkdir -p $(TEST_FOLDER)/source
	@mkdir -p $(TEST_FOLDER)/txt
	> $(dune_file)
	for i in $(R); do \
		echo "(test\n (name day0$$i)\n (libraries y20$(Y) test_utils)\n (deps ../txt/day0$$i.txt)\n (modules day0$$i)\n)\n" >> $(dune_file); \
		echo "open Y20$(Y)\n\nlet _ = Test_utils.test_day $(Y) $$i Day0$$i.p1 Day0$$i.p2" > $(TEST_FOLDER)/source/day0$$i.ml; \
		$(eval TEST_FILE := $(TEST_FOLDER)/txt/day0) \
		if [ ! -e $(TEST_FILE)$$i.txt ]; then touch $(TEST_FILE)$$i.txt; fi; \
	done
	$(eval R := $(shell echo $$(seq 10 25)))
	for i in $(R); do \
		echo "(test\n (name day$$i)\n (libraries y20$(Y) test_utils)\n (deps ../txt/day$$i.txt)\n (modules day$$i)\n)\n" >> $(dune_file); \
		echo "open Y20$(Y)\n\nlet _ = Test_utils.test_day $(Y) $$i Day$$i.p1 Day$$i.p2" > $(TEST_FOLDER)/source/day$$i.ml; \
		$(eval TEST_FILE := $(TEST_FOLDER)/txt/day) \
		if [ ! -e $(TEST_FILE)$$i.txt ]; then touch $(TEST_FILE)$$i.txt; fi; \
	done

create_src:
	$(eval cnt := let p1 _ = \"\"\n\nlet p2 _ = \"\")
	$(eval folder := lib/y20$(Y)/)
	$(eval R := $(shell echo $$(seq 1 9)))
	mkdir -p lib/y20$(Y)
	for i in $(R); do \
		if [ ! -e $(folder)day0$$i.ml ]; then echo "$(cnt)" > $(folder)day0$$i.ml; fi; \
	done
	$(eval R := $(shell echo $$(seq 10 25)))
	for i in $(R); do \
		if [ ! -e $(folder)day$$i.ml ]; then echo "$(cnt)" > $(folder)day$$i.ml; fi; \
	done
	if [ ! -e $(folder)/dune ]; then \
	 	echo "(library\n (name y20$(Y))\n)" > $(folder)/dune; \
	fi


%:
	$(eval OK := $(shell echo $@ | sed 's/^[1-9][1-9]\/[0-9][0-9]$$/OK/'))
	if [ "$(OK)" != "OK" ]; \
		then echo "Invlid input" "Input should be YY/DD" && exit 1; \
	fi
	$(eval year := $(shell echo $@ | sed 's/^\([1-9][1-9]\)...$$/\1/'))
	$(eval day := $(shell echo $@ | sed 's/^...\([0-9][0-9]\)$$/\1/'))
	touch test/y20$(year)_test/day$(day).txt
	if [ ! -e lib/y2015/day$(day).ml ]; \
		then echo "let p1 _ = \"\"\n\nlet p2 _ = \"\"" > lib/y2015/day$(day).ml; \
	fi;
	dune build
	code lib/y2015/day$(day).ml
	code test/y20$(year)_test/day$(day).txt
