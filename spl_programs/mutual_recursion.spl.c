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

bool is_even(int);
bool is_odd(int);
void spl_main();

bool isEmpty(_list* x) {
	return x->tl == 0;
}

int RuntimeErr() {
	exit(-1);
}

bool is_even(int n) {
bool _ufVK4ZDE0 = 0;
int _T0safl1Rn = (int) n;
int _ivXKnTzac = 0;
_ufVK4ZDE0 = _T0safl1Rn == _ivXKnTzac;
if(_ufVK4ZDE0) {
bool _76BKIVuiX = true;
return _76BKIVuiX;
} else {
bool _qZ2juZN5p = 0;
int _8tsjFUv4v = 0;
int _XZ5JmpxDs = (int) n;
int _ezMsTOK2s = 1;
_8tsjFUv4v = _XZ5JmpxDs - _ezMsTOK2s;
_qZ2juZN5p = is_odd(_8tsjFUv4v);
return _qZ2juZN5p;
}
}

bool is_odd(int n) {
bool _hu0T5nGNz = 0;
int _2sCQUNALt = (int) n;
int _URMKMo9H3 = 0;
_hu0T5nGNz = _2sCQUNALt == _URMKMo9H3;
if(_hu0T5nGNz) {
bool _B18Gj8QsA = false;
return _B18Gj8QsA;
} else {
bool _eRBJEQuh0 = 0;
int _9fEvpzEAD = 0;
int _2MwGIRI2Q = (int) n;
int _FFFbPdNl3 = 1;
_9fEvpzEAD = _2MwGIRI2Q - _FFFbPdNl3;
_eRBJEQuh0 = is_even(_9fEvpzEAD);
return _eRBJEQuh0;
}
}

void spl_main() {
bool _1wMrv6xEt = 0;
int _iiaECPu8x = 10;
_1wMrv6xEt = is_even(_iiaECPu8x);
if(_1wMrv6xEt) {
printf("True");
} else {
printf("False");
}
printf("\n");
}

int main() {
spl_main();
}

