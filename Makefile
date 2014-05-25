all:
	rebar compile

test: compile_test_locale
	rebar eunit

clean: clean_test_locale
	rebar clean

compile_test_locale:
	msgfmt -c -o test/locale/se/LC_MESSAGES/default.mo test/locale/se/LC_MESSAGES/default.po

clean_test_locale:
	rm test/locale/se/LC_MESSAGES/default.mo
