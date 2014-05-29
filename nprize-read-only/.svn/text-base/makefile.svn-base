CFLAGS=-O3

moviebin2userbin: moviebin2userbin.o basic.o
	$(CC) -o $@ $^

utest0b1: utest.o global.o basic.o ubaseline1.o mix2.o -llapack
	$(CC) -o $@ $^

usvdbkw1: utest.o basic.o usvdbkw1.o weight.o global.o mix2.o -llapack
	$(CC) -o $@ $^

utest10: utest.o basic.o usvdns1b.o global.o mix2.o -llapack
	$(CC) -o $@ $^

clean:
	rm *.o *.stackdump moviebin2userbin uuserbin2moviebin utest[0-9] *.exe
