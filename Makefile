all: stachec

stachec:
	stack build

testphp: stachec test/template.html
	stack exec stachec -- php test/template.html > test/test.php
	cd test && php ./runtest.php

testjs: stachec test/template.html
	stack exec stachec -- js test/template.html > test/test.js
	cd test && node ./runtest.js

test: testphp testjs
