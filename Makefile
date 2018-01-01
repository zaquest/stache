all: stachec

stachec:
	stack build

testphp:
	stack exec stachec -- php test/tile.tmpl.html > test/test.php
	cd test && php ./runtest.php

testjs:
	stack exec stachec -- js test/tile.tmpl.html > test/test.js
	cd test && node ./runtest.js

test: testphp testjs
