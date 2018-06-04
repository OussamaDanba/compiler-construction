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
bool _J0l41hWjd = 0;
bool _medhxKAyv = 0;
_list* _G6GXub3As = (_list*) list;
_medhxKAyv = isEmpty(_G6GXub3As);
_medhxKAyv = !_medhxKAyv;
bool _Jje5q0VBH = 0;
int _lOkdj1FfP = (int) n;
int _MTHxThYSw = 0;
_Jje5q0VBH = _lOkdj1FfP > _MTHxThYSw;
_J0l41hWjd = _medhxKAyv && _Jje5q0VBH;
while(_J0l41hWjd) {
int _OTEpPEVI3 = (int) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->hd);
printf("%i", _OTEpPEVI3);
printf("\n");
_list* _OkLqdLKah = (_list*) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->tl);
uintptr_t _Lk7hUq4m6 = (uintptr_t) (_list*) list;
list = (uintptr_t) _OkLqdLKah;
int _Ii8ktPO7U = 0;
int _1EiUoiOpS = (int) n;
int _7hL0TbSnV = 1;
_Ii8ktPO7U = _1EiUoiOpS - _7hL0TbSnV;
uintptr_t _tWOZW1qoX = (uintptr_t) (int) n;
n = (uintptr_t) _Ii8ktPO7U;
_J0l41hWjd = 0;
bool _7PZtP6lwh = 0;
_list* _SDdU2nKnM = (_list*) list;
_7PZtP6lwh = isEmpty(_SDdU2nKnM);
_7PZtP6lwh = !_7PZtP6lwh;
bool _HfXqspckd = 0;
int _jfIX8TjbQ = (int) n;
int _cgqHIt88S = 0;
_HfXqspckd = _jfIX8TjbQ > _cgqHIt88S;
_J0l41hWjd = _7PZtP6lwh && _HfXqspckd;
}
}

_list* rotateRight(int x,_list* list) {
int _bDjnkxPEn = 0;
bool _hsTjj1c8n = 0;
int _A8jNpx71L = (int) _bDjnkxPEn;
int _HDsbzDxs0 = (int) x;
_hsTjj1c8n = _A8jNpx71L < _HDsbzDxs0;
while(_hsTjj1c8n) {
_list* _SvYBGuFrS = (_list*) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->tl);
uintptr_t _vQWWvV5gQ = (uintptr_t) (_list*) list;
list = (uintptr_t) _SvYBGuFrS;
int _6tmzk5OYW = 0;
int _26ieFjvwh = (int) _bDjnkxPEn;
int _cWs23fFYc = 1;
_6tmzk5OYW = _26ieFjvwh + _cWs23fFYc;
uintptr_t _uzdUSY8u2 = (uintptr_t) (int) _bDjnkxPEn;
_bDjnkxPEn = (uintptr_t) _6tmzk5OYW;
_hsTjj1c8n = 0;
int _lpebc0oep = (int) _bDjnkxPEn;
int _GPwBt60fd = (int) x;
_hsTjj1c8n = _lpebc0oep < _GPwBt60fd;
}
_list* _8zhN4e34L = (_list*) list;
return _8zhN4e34L;
}

void spl_main() {
_list* _PdP2C88V2 = 0;
int _QE4XQKh4O = 1;
_list* _JGAxZJvQd = 0;
int _WGW3y16je = 2;
_list* _UJBI0NC6X = 0;
int _7qHrAfpUt = 3;
_list* _8UkAfDj1M = 0;
int _K0kxQcdI5 = 4;
_list* _TBcTz09Km = malloc(sizeof(_list));
_TBcTz09Km->tl = 0;
_8UkAfDj1M = malloc(sizeof(_list));
_8UkAfDj1M->hd = (uintptr_t) _K0kxQcdI5;
_8UkAfDj1M->tl = (uintptr_t) _TBcTz09Km;
_UJBI0NC6X = malloc(sizeof(_list));
_UJBI0NC6X->hd = (uintptr_t) _7qHrAfpUt;
_UJBI0NC6X->tl = (uintptr_t) _8UkAfDj1M;
_JGAxZJvQd = malloc(sizeof(_list));
_JGAxZJvQd->hd = (uintptr_t) _WGW3y16je;
_JGAxZJvQd->tl = (uintptr_t) _UJBI0NC6X;
_PdP2C88V2 = malloc(sizeof(_list));
_PdP2C88V2->hd = (uintptr_t) _QE4XQKh4O;
_PdP2C88V2->tl = (uintptr_t) _JGAxZJvQd;
_list* _bQVbvr2nb = (_list*) _PdP2C88V2;
uintptr_t _K98qAbvUM = (uintptr_t) (_list*) (((_list*) (((_list*) (((_list*) (((_list*) _PdP2C88V2)->tl == 0 ? RuntimeErr() : ((_list*) _PdP2C88V2)->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) _PdP2C88V2)->tl == 0 ? RuntimeErr() : ((_list*) _PdP2C88V2)->tl))->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) (((_list*) _PdP2C88V2)->tl == 0 ? RuntimeErr() : ((_list*) _PdP2C88V2)->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) _PdP2C88V2)->tl == 0 ? RuntimeErr() : ((_list*) _PdP2C88V2)->tl))->tl))->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) (((_list*) (((_list*) _PdP2C88V2)->tl == 0 ? RuntimeErr() : ((_list*) _PdP2C88V2)->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) _PdP2C88V2)->tl == 0 ? RuntimeErr() : ((_list*) _PdP2C88V2)->tl))->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) (((_list*) _PdP2C88V2)->tl == 0 ? RuntimeErr() : ((_list*) _PdP2C88V2)->tl))->tl == 0 ? RuntimeErr() : ((_list*) (((_list*) _PdP2C88V2)->tl == 0 ? RuntimeErr() : ((_list*) _PdP2C88V2)->tl))->tl))->tl))->tl);
((_list*)((_list*)((_list*)((_list*)_PdP2C88V2)->tl)->tl)->tl)->tl = (uintptr_t) _bQVbvr2nb;
_list* _cozodKvmB = 0;
int _R3CK0Cyrg = 37;
_list* _3adnUUU8y = (_list*) _PdP2C88V2;
_cozodKvmB = rotateRight(_R3CK0Cyrg,_3adnUUU8y);
uintptr_t _xF5178Tv5 = (uintptr_t) (_list*) _PdP2C88V2;
_PdP2C88V2 = (uintptr_t) _cozodKvmB;
_list* _2h2ElSHvD = (_list*) _PdP2C88V2;
int _LpF1mLAdL = 21;
printNumElements(_2h2ElSHvD,_LpF1mLAdL);
}

int main() {
spl_main();
}

