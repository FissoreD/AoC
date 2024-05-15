y2015:
	touch test/y2015_test/day$D.txt && touch lib/y2015/day$D.ml && \
	echo "let p1 _ = ()\n\nlet p2 _ = ()" > lib/y2015/day$D.ml && \
	dune build