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
bool _N0xEXvii8 = 0;
int _qdiUgiNCa = (int) n;
int _th3BH9Ame = 0;
_N0xEXvii8 = _qdiUgiNCa == _th3BH9Ame;
if(_N0xEXvii8) {
bool _gOVfWvHXS = true;
return _gOVfWvHXS;
} else {
bool _4muFL1dtf = 0;
int _UDi2nye8s = 0;
int _uzR8v8TtC = (int) n;
int _kHLuEOgPf = 1;
_UDi2nye8s = _uzR8v8TtC - _kHLuEOgPf;
_4muFL1dtf = is_odd(_UDi2nye8s);
return _4muFL1dtf;
}
}

bool is_odd(int n) {
bool _1i5DI9dx5 = 0;
int _KhpQcn0GE = (int) n;
int _TKUQUJS4L = 0;
_1i5DI9dx5 = _KhpQcn0GE == _TKUQUJS4L;
if(_1i5DI9dx5) {
bool _uKD8gFzc0 = false;
return _uKD8gFzc0;
} else {
bool _mVJcVEo0T = 0;
int _VNB7cTWes = 0;
int _Qp70fVXv6 = (int) n;
int _A7tAQ9xby = 1;
_VNB7cTWes = _Qp70fVXv6 - _A7tAQ9xby;
_mVJcVEo0T = is_even(_VNB7cTWes);
return _mVJcVEo0T;
}
}

void spl_main() {
bool _8LeBB1wWL = 0;
int _QkXpjbwQI = 10;
_8LeBB1wWL = is_even(_QkXpjbwQI);
if(_8LeBB1wWL) {
printf("True");
} else {
printf("False");
}
printf("\n");
}

int main() {
spl_main();
}

