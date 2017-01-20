all:
		gcc -g -o test.o -c test.c
		gcc -g -o test test.o -rdynamic -llua -ldl -lm
		gcc -g -o testusrdata.o -c testusrdata.c
		gcc -g -o testusrdata testusrdata.o -rdynamic -llua -ldl -lm

clean:
		rm test test.o
