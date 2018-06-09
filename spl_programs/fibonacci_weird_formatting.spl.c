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
int fibonacci(int);

bool isEmpty(_list* x) {
	return x->tl == 0;
}

int RuntimeErr() {
	exit(-1);
}

void spl_main() {
int _wfFmCqMSH = 0;
int _hOBQMcMGG = 7;
_wfFmCqMSH = fibonacci(_hOBQMcMGG);
printf("%i", _wfFmCqMSH);
printf("\n");
}

int fibonacci(int x) {
bool _PZoaZbYZ0 = 0;
int _dOC4f3GVy = (int) x;
int _sCq4c2fVK = 0;
_PZoaZbYZ0 = _dOC4f3GVy == _sCq4c2fVK;
if(_PZoaZbYZ0) {
int _0TLuDSYlV = 0;
return _0TLuDSYlV;
} else {
}
bool _kS4ta4edW = 0;
int _k7SBHRXA0 = (int) x;
int _8GW4Og4F6 = 1;
_kS4ta4edW = _k7SBHRXA0 == _8GW4Og4F6;
if(_kS4ta4edW) {
int _ht8Si2hHv = 1;
return _ht8Si2hHv;
} else {
}
int _ibGO6Cis9 = 0;
int _Z7eM0Qhw8 = 0;
int _RSB8UuhPx = 0;
int _gGfjYXHWT = (int) x;
int _nCVAxiyGQ = 1;
_RSB8UuhPx = _gGfjYXHWT - _nCVAxiyGQ;
_Z7eM0Qhw8 = fibonacci(_RSB8UuhPx);
int _WRbU2Tgz5 = 0;
int _PWdtouzJo = 0;
int _enz9x1Zt4 = (int) x;
int _0UdqnQE9r = -2;
_PWdtouzJo = _enz9x1Zt4 + _0UdqnQE9r;
_WRbU2Tgz5 = fibonacci(_PWdtouzJo);
_ibGO6Cis9 = _Z7eM0Qhw8 + _WRbU2Tgz5;
return _ibGO6Cis9;
}

int main() {
spl_main();
}

