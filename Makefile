CFLAGS=-Wall -Wstrict-aliasing -std=gnu11 -g -I. -O0
UTIL_OBJS=buffer.o dict.o encoding.o error.o file.o map.o path.o set.o vector.o
BUILD_DIR="$(shell pwd)"


%.o: %.c 8cc.h keyword.inc
	$(CC) $(CFLAGS) -DBUILD_DIR='$(BUILD_DIR)' -o $@ -c $<

8cc: main.o cpp.o debug.o gen.o lex.o parse.o $(UTIL_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)



#test/host_%.o: test/%.c
#	$(CC) $(CFLAGS) -w -o $@ -c $<
#
#test/%.bin: test/%.o test/testmain.o
#	$(CC) -o $@ $< test/testmain.o $(LDFLAGS)

TESTS := $(sort $(patsubst %.c,%,$(filter-out test/testmain.c,$(wildcard test/*.c))))

# Parts of the test suite must (necessarily) tangle with the ghosts of backwards
# compatibility.  These tests will trigger warnings from modern compilers.  Any
# test *not* explicitly marked is expected to compile cleanly without warnings.
#
# The host compiler may even refuse to compile certain tests (e.g. those which
# exercise C11 features) so we also need to skip those.

TESTS_HOST_SKIP=test/align
TESTS_HOST_WARNINGS=test/array test/union

testmain.o: test/testmain.c
	$(CC) $(CFLAGS) -o $@ -c $<

utiltest: utiltest.o $(UTIL_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)


# Compile tests with both 8cc and the host compiler and compare the results;
# if they differ, we have discovered an error in (one of) 8cc, the test, or
# the host compiler.  Since the host compiler is unlikely to be at fault,
# this enables us to "test the tests".
#
# Currently, 8cc cannot perform linking, so we invoke the host compiler to
# link the 8cc-generated object files.

test/%: test/%.c testmain.o 8cc
	@echo "Test: $*..."
	@for t in $(filter-out $@,$(TESTS_HOST_SKIP)); do \
	  $(CC) $(if $(filter $@,$(TESTS_HOST_WARNINGS)),-w,-Wall -Wextra -Werror -O3) -o test/$*.host testmain.o $<; \
	  ./test/$*.host >/dev/null || echo "Failed test with $(CC)!"; \
	done
	./8cc -o test/$*.8cc.o -c $<
	$(CC) -o test/$*.8cc test/$*.8cc.o testmain.o
	./test/$*.8cc >/dev/null || exit

test: $(TESTS)

#test: 8cc utiltest $(TESTS)
#	./utiltest
#	./test/ast.sh
#	./test/negative.py
#	echo $(TESTS)

#runtests:
#	@for test in $(TESTS); do  \
#	    ./$$test || exit;      \
#	done

#stage1:
#	$(MAKE) cleanobj
#	[ -f 8cc ] || $(MAKE) 8cc
#	mv 8cc stage1

#stage2: stage1
#	$(MAKE) cleanobj
#	$(MAKE) CC=./stage1 ECC=./stage1 CFLAGS= 8cc
#	mv 8cc stage2

#stage3: stage2
#	$(MAKE) cleanobj
#	$(MAKE) CC=./stage2 ECC=./stage2 CFLAGS= 8cc
#	mv 8cc stage3

# Compile and run the tests with the default compiler.
#testtest:
#	$(MAKE) clean
#	$(MAKE) $(TESTS)
#	$(MAKE) runtests

#fulltest: testtest
#	$(MAKE) stage1
#	$(MAKE) CC=./stage1 ECC=./stage1 CFLAGS= test
#	$(MAKE) stage2
#	$(MAKE) CC=./stage2 ECC=./stage2 CFLAGS= test
#	$(MAKE) stage3
#	cmp stage2 stage3

clean: cleanobj
	rm -f 8cc stage?

cleanobj:
	rm -f *.o *.s test/*.host test/*.8cc.o test/*.8cc utiltest

all: 8cc

.PHONY: clean cleanobj test runtests fulltest self all
