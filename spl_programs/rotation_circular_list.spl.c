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

void printNumElements(_list*,int);
_list* rotateRight(int,_list*);
void spl_main();

bool isEmpty(_list* x) {
	return x->tl == 0;
}

int RuntimeErr() {
	exit(-1);
}

void printNumElements(_list* list,int n) {
bool _kYxbR4gS4 = 0;
bool _cVmdMtued = 0;
_list* _RCaYgnSHO = (_list*) list;
_cVmdMtued = isEmpty(_RCaYgnSHO);
_cVmdMtued = !_cVmdMtued;
bool _2e8ZxWyvP = 0;
int _BDaoQwpeX = (int) n;
int _GYWp26Fj7 = 0;
_2e8ZxWyvP = _BDaoQwpeX > _GYWp26Fj7;
_kYxbR4gS4 = _cVmdMtued && _2e8ZxWyvP;
while(_kYxbR4gS4) {
int _ZyuXDgmmB = (int) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->hd);
printf("%i", _ZyuXDgmmB);
printf("\n");
_list* _cxrXOmeqq = (_list*) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->tl);
uintptr_t _aoec2LKHc = (uintptr_t) (_list*) list;
list = (uintptr_t) _cxrXOmeqq;
int _1q2Ap5QBB = 0;
int _qovOtdux9 = (int) n;
int _QqhTkSxNq = 1;
_1q2Ap5QBB = _qovOtdux9 - _QqhTkSxNq;
uintptr_t _fbv4Oandc = (uintptr_t) (int) n;
n = (uintptr_t) _1q2Ap5QBB;
_kYxbR4gS4 = 0;
bool _5h9qc0pw1 = 0;
_list* _5selg0fJb = (_list*) list;
_5h9qc0pw1 = isEmpty(_5selg0fJb);
_5h9qc0pw1 = !_5h9qc0pw1;
bool _btrhjEFny = 0;
int _KmhpAIpbr = (int) n;
int _H7UCXVm5h = 0;
_btrhjEFny = _KmhpAIpbr > _H7UCXVm5h;
_kYxbR4gS4 = _5h9qc0pw1 && _btrhjEFny;
}
}

_list* rotateRight(int x,_list* list) {
int _6enjqOWhg = 0;
bool _mnwfRmwec = 0;
int _ehRgZQHo9 = (int) _6enjqOWhg;
int _A35ubspuH = (int) x;
_mnwfRmwec = _ehRgZQHo9 < _A35ubspuH;
while(_mnwfRmwec) {
_list* _k0RlBkCFE = (_list*) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->tl);
uintptr_t _znb2eaEL4 = (uintptr_t) (_list*) list;
list = (uintptr_t) _k0RlBkCFE;
int _W2FIggIgt = 0;
int _cKAEOtKbP = (int) _6enjqOWhg;
int _fByasD7cv = 1;
_W2FIggIgt = _cKAEOtKbP + _fByasD7cv;
uintptr_t _Bsik1tRKY = (uintptr_t) (int) _6enjqOWhg;
_6enjqOWhg = (uintptr_t) _W2FIggIgt;
_mnwfRmwec = 0;
int _m1sDsVLCf = (int) _6enjqOWhg;
int _dYTDsQaux = (int) x;
_mnwfRmwec = _m1sDsVLCf < _dYTDsQaux;
}
_list* _O2llMNGlL = (_list*) list;
return _O2llMNGlL;
}

void spl_main() {
_list* _DZUAx3vqw = 0;
int _qBRkC88km = 1;
_list* _00bPS27Gh = 0;
int _TU6T3fCz5 = 2;
_list* _fbBNEOBh2 = 0;
int _nciajX0VD = 3;
_list* _OVEIL6jzx = 0;
int _A6ORkAa09 = 4;
_list* _0wQ1p9Af1 = malloc(sizeof(_list));
_0wQ1p9Af1->tl = 0;
_OVEIL6jzx = malloc(sizeof(_list));
_OVEIL6jzx->hd = (uintptr_t) _A6ORkAa09;
_OVEIL6jzx->tl = (uintptr_t) _0wQ1p9Af1;
_fbBNEOBh2 = malloc(sizeof(_list));
_fbBNEOBh2->hd = (uintptr_t) _nciajX0VD;
_fbBNEOBh2->tl = (uintptr_t) _OVEIL6jzx;
_00bPS27Gh = malloc(sizeof(_list));
_00bPS27Gh->hd = (uintptr_t) _TU6T3fCz5;
_00bPS27Gh->tl = (uintptr_t) _fbBNEOBh2;
_DZUAx3vqw = malloc(sizeof(_list));
_DZUAx3vqw->hd = (uintptr_t) _qBRkC88km;
_DZUAx3vqw->tl = (uintptr_t) _00bPS27Gh;
_list* _Dqjkircop = (_list*) _DZUAx3vqw;
uintptr_t _cZwa1b9Zq = (uintptr_t) (_list*) (((_list*) (((_list*) (((_list*) (((_list*) _DZUAx3vqw)->tl == 0 ? RuntimeErr() : ((_list*) _DZUAx3vqw)->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) _DZUAx3vqw)->tl == 0 ? RuntimeErr() : ((_list*) _DZUAx3vqw)->tl))->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) (((_list*) _DZUAx3vqw)->tl == 0 ? RuntimeErr() : ((_list*) _DZUAx3vqw)->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) _DZUAx3vqw)->tl == 0 ? RuntimeErr() : ((_list*) _DZUAx3vqw)->tl))->tl))->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) (((_list*) (((_list*) _DZUAx3vqw)->tl == 0 ? RuntimeErr() : ((_list*) _DZUAx3vqw)->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) _DZUAx3vqw)->tl == 0 ? RuntimeErr() : ((_list*) _DZUAx3vqw)->tl))->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) (((_list*) _DZUAx3vqw)->tl == 0 ? RuntimeErr() : ((_list*) _DZUAx3vqw)->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) _DZUAx3vqw)->tl == 0 ? RuntimeErr() : ((_list*) _DZUAx3vqw)->tl))->tl))->tl))->tl);
((_list*)((_list*)((_list*)((_list*)_DZUAx3vqw)->tl)->tl)->tl)->tl = (uintptr_t) _Dqjkircop;
_list* _TyB5O84Cn = 0;
int _iTj1OIvTw = 37;
_list* _RMqZp1ZqC = (_list*) _DZUAx3vqw;
_TyB5O84Cn = rotateRight(_iTj1OIvTw,_RMqZp1ZqC);
uintptr_t _cKvqXKmCs = (uintptr_t) (_list*) _DZUAx3vqw;
_DZUAx3vqw = (uintptr_t) _TyB5O84Cn;
_list* _VImEcxyDG = (_list*) _DZUAx3vqw;
int _389UTu7Gf = 21;
printNumElements(_VImEcxyDG,_389UTu7Gf);
}

int main() {
spl_main();
}

