call := just_executable() + " --justfile=" + justfile()

cat:
	cat Justfile

test:
	fpm test

jconstants:
	fpm run --example example_jconstants

jconv:
	fpm run --example example_jconv

jstring:
	fpm run --example example_jstring

jstringbuffer:
	fpm run --example example_jstringbuffer

jsys:
	fpm run --example example_jsys -- aa bb cc dd

jtypes:
	fpm run --example example_jtypes

examples:
	{{call}} jconstants
	{{call}} jconv
	{{call}} jstring
	{{call}} jstringbuffer
	fpm run --example example_jsys -- aa bb cc dd
	{{call}} jtypes
