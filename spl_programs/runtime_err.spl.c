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

void spl_main();

bool isEmpty(_list* x) {
	return x->tl == 0;
}

int RuntimeErr() {
	exit(-1);
}

void spl_main() {
_list* _C3O0Ybt5U = malloc(sizeof(_list));
_C3O0Ybt5U->tl = 0;
int _KUBXDRB4o = 0;
bool _zBsy4GX42 = 0;
int _zlHAUn2ay = (int) _KUBXDRB4o;
int _NQkGzC0mS = 10;
_zBsy4GX42 = _zlHAUn2ay < _NQkGzC0mS;
while(_zBsy4GX42) {
_list* _9JX0ZxGQt = 0;
int _XPi0EVnug = (int) _KUBXDRB4o;
_list* _KWVt8dXWd = (_list*) _C3O0Ybt5U;
_9JX0ZxGQt = malloc(sizeof(_list));
_9JX0ZxGQt->hd = (uintptr_t) _XPi0EVnug;
_9JX0ZxGQt->tl = (uintptr_t) _KWVt8dXWd;
uintptr_t _pEIg7rjeL = (uintptr_t) (_list*) _C3O0Ybt5U;
_C3O0Ybt5U = (uintptr_t) _9JX0ZxGQt;
int _LXptI1dG2 = 0;
int _19hqvZI2p = (int) _KUBXDRB4o;
int _6NlUJgVbX = 1;
_LXptI1dG2 = _19hqvZI2p + _6NlUJgVbX;
uintptr_t _kHSUZpfCe = (uintptr_t) (int) _KUBXDRB4o;
_KUBXDRB4o = (uintptr_t) _LXptI1dG2;
_zBsy4GX42 = 0;
int _Glp51BxIn = (int) _KUBXDRB4o;
int _zaSHustQk = 10;
_zBsy4GX42 = _Glp51BxIn < _zaSHustQk;
}
bool _kv81iIFLh = true;
while(_kv81iIFLh) {
int _jYymMeiZa = (int) (((_list*) _C3O0Ybt5U)->tl == 0 ? RuntimeErr() : ((_list*) _C3O0Ybt5U)->hd);
printf("%i", _jYymMeiZa);
printf("\n");
_list* _YbUX7FaqC = (_list*) (((_list*) _C3O0Ybt5U)->tl == 0 ? RuntimeErr() : ((_list*) _C3O0Ybt5U)->tl);
uintptr_t _AqdhPvjWC = (uintptr_t) (_list*) _C3O0Ybt5U;
_C3O0Ybt5U = (uintptr_t) _YbUX7FaqC;
_kv81iIFLh = true;
}
}

int main() {
spl_main();
}

