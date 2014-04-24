all:
	@test -d ebin || mkdir ebin
	@erl -make

clean:
	@rm -rf ebin/*.beam erl_crash.dump
