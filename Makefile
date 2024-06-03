%:
	$(eval OK := $(shell echo $@ | sed 's/^[1-9][1-9]\/[0-9][0-9]$$/OK/'))
	if [ "$(OK)" != "OK" ]; \
		then echo "Invlid input" "Input should be YY/DD" && exit 1; \
	fi
	$(eval year := $(shell echo $@ | sed 's/^\([1-9][1-9]\)...$$/\1/'))
	$(eval day := $(shell echo $@ | sed 's/^...\([0-9][0-9]\)$$/\1/'))
	touch test/y20$(year)_test/day$(day).txt
	if [ ! -e lib/y2015/day$(day).ml ]; \
		then echo "let p1 _ = ()\n\nlet p2 _ = ()" > lib/y2015/day$(day).ml; \
	fi;
	dune build
	code lib/y2015/day$(day).ml
	code test/y20$(year)_test/day$(day).txt

.SILENT:
