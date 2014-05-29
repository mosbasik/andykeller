CFLAGS=-O3
# linux and cygwin (see README)
#LIB=-llpack
# mac (lapack via atlas is called "veclib")
LIB=-framework veclib

moviebin2userbin: moviebin2userbin.o basic.o
	$(CC) -o $@ $^

utest0b1: utest.o global.o basic.o ubaseline1.o mix2.o
	$(CC) $(LIB) -L/pt/lib -L/usr/local/lib -o $@ $^

usvdbkw1: utest.o basic.o usvdbkw1.o weight.o global.o mix2.o
	$(CC) $(LIB) -o $@ $^

utest10: utest.o basic.o usvdns1b.o global.o mix2.o
	$(CC) $(LIB) -o $@ $^

clean:
	rm *.o *.stackdump moviebin2userbin uuserbin2moviebin utest[0-9] *.exe
