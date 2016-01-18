ERL_SOURCES := $(wildcard src/*.erl)
ERL_OBJECTS := $(ERL_SOURCES:src/%.erl=ebin/%.beam)

ebin/%.beam: src/%.erl
	erlc +debug_info -o ebin/ $<

all: $(ERL_OBJECTS)
