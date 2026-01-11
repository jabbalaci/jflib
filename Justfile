call := just_executable() + " --justfile=" + justfile()

cat:
	cat Justfile

test:
	fpm test

jconstants:
	fpm run --example example_jconstants

jconv:
	fpm run --example example_jconv

jfile:
	fpm run --example example_jfile

jintlist:
	fpm run --example example_jintlist

jstring:
	fpm run --example example_jstring

jstringbuffer:
	fpm run --example example_jstringbuffer

jsys:
	fpm run --example example_jsys -- aa bb cc dd

jtypes:
	fpm run --example example_jtypes

examples:
	fpm run --example "*"

examples2:
	rm -fr ./build
	fpm run --example "*"

clean:
	rm -f ./src/*.mod
