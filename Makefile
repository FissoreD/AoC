NB := $(shell (seq 1 25))

# make create Y=XX where XX is a two digit number
create:
	$(MAKE) create_src create_tests
	dune build

# A valid year is a two digit string between 15 and current_year included
ok_year:
	$(eval lt := $(shell test $(Y) -le $$(date +%y); echo $$?))
	$(eval bt := $(shell test $(Y) -ge 15; echo $$?))
	@(echo $(Y) $(bt) $(lt) | grep "[1-9][0-9] 0 0" > /dev/null) || (echo "Invalid year" $(Y) "; should be between 15 and current_year" && false)

# make create_tests Y=XX where XX is a two digit number
create_tests: ok_year
	$(eval TEST_FOLDER := test/y20$(Y))
	$(eval R := $(shell echo $$(seq 1 9)))
	$(eval dune_file := $(TEST_FOLDER)/source/dune)
	@mkdir -p $(TEST_FOLDER)/expected
	@mkdir -p $(TEST_FOLDER)/source
	@mkdir -p $(TEST_FOLDER)/txt
	> $(dune_file)
	for i in $(R); do $(call BUILD_TEST,"0",$$i) done
	$(eval R := $(shell echo $$(seq 10 25)))
	for i in $(R); do $(call BUILD_TEST,"",$$i) done

# make create_src Y=XX where XX is a two digit number
create_src: ok_year
	$(eval cnt := let p1 _ = \"\"\nlet p2 _ = \"\")
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

define BUILD_TEST
	$(eval DAY := $(1)$$(2)) \
	echo "$(call DUNE_TEST_RULE,${DAY})" >> $(dune_file); \
	echo "$(call DUNE_TEST_BODY,$(DAY))" > $(TEST_FOLDER)/source/day${DAY}.ml; \
	$(eval EXPECTED_FILE := $(TEST_FOLDER)/expected/day) \
	if [ ! -e $(TEST_FILE)${DAY}.txt ]; then touch $(TEST_FOLDER)/txt/day${DAY}.txt; fi; \
	if [ ! -e $(EXPECTED_FILE)${DAY}.p1.txt ]; then touch $(EXPECTED_FILE)${DAY}.p1.txt; fi; \
	if [ ! -e $(EXPECTED_FILE)${DAY}.p2.txt ]; then touch $(EXPECTED_FILE)${DAY}.p2.txt; fi;
endef

define DUNE_TEST_RULE
(test\
	(name day$1)\
	(libraries y20$(Y) test_utils)\
	(deps ../txt/day$1.txt ../expected/day$1.p1.txt ../expected/day$1.p2.txt)\
	(modules day$1))\

endef

define DUNE_TEST_BODY
open Y20$(Y)\n\
open Test_utils\n\
\n\
let _ = Test_utils.test_expected $(Y) $$i (module Day$1 : M)
endef

export DUNE_TEST_RULE
export DUNE_TEST_BODY

.SILENT:create_src create_tests