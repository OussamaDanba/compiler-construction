#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct {
	uintptr_t fst;
	uintptr_t snd;
} _tuple;

typedef struct {
	uintptr_t hd;
	uintptr_t tl;
} _list;

int square(int);
void spl_main();

bool isEmpty(_list* x) {
	return x->tl == 0;
}

int RuntimeErr() {
	exit(-1);
}

int square(int x) {
int _awnwDdC7F = 0;
int _BlEHMraB4 = (int) x;
int _iBaFtxZRu = (int) x;
_awnwDdC7F = _BlEHMraB4 * _iBaFtxZRu;
return _awnwDdC7F;
}
