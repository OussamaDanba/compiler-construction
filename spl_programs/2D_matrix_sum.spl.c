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

_list* x;
int list_sum(_list*);
int matrix_sum(_list*);
void spl_main();

bool isEmpty(_list* x) {
	return x->tl == 0;
}

int RuntimeErr() {
	exit(-1);
}

int list_sum(_list* list) {
int _X13OpwPIk = 0;
bool _t44zpUnbY = 0;
_list* _JPos4oOPO = (_list*) list;
_t44zpUnbY = isEmpty(_JPos4oOPO);
_t44zpUnbY = !_t44zpUnbY;
while(_t44zpUnbY) {
int _mq4uNZIaD = 0;
int _NnB1wHCSO = (int) _X13OpwPIk;
int _PExIwpF5T = (int) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->hd);
_mq4uNZIaD = _NnB1wHCSO + _PExIwpF5T;
uintptr_t _oT3g03JpI = (uintptr_t) (int) _X13OpwPIk;
_X13OpwPIk = (uintptr_t) _mq4uNZIaD;
_list* _nMZ8njNIo = (_list*) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->tl);
uintptr_t _am4eMcH3c = (uintptr_t) (_list*) list;
list = (uintptr_t) _nMZ8njNIo;
_t44zpUnbY = 0;
_list* _i55lTGepn = (_list*) list;
_t44zpUnbY = isEmpty(_i55lTGepn);
_t44zpUnbY = !_t44zpUnbY;
}
int _9BW8jfXKo = (int) _X13OpwPIk;
return _9BW8jfXKo;
}

int matrix_sum(_list* list) {
int _zQwQgL7su = 0;
bool _F8B4P5Jtv = 0;
_list* _7b5v3SlSu = (_list*) list;
_F8B4P5Jtv = isEmpty(_7b5v3SlSu);
_F8B4P5Jtv = !_F8B4P5Jtv;
while(_F8B4P5Jtv) {
int _ycP5sSE6x = 0;
int _qjtaV7qzE = (int) _zQwQgL7su;
int _VCznTaP4l = 0;
_list* _nM2j81Cvn = (_list*) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->hd);
_VCznTaP4l = list_sum(_nM2j81Cvn);
_ycP5sSE6x = _qjtaV7qzE + _VCznTaP4l;
uintptr_t _OO8LxtAYP = (uintptr_t) (int) _zQwQgL7su;
_zQwQgL7su = (uintptr_t) _ycP5sSE6x;
_list* _MyYL36dBI = (_list*) (((_list*) list)->tl == 0 ? RuntimeErr() : ((_list*) list)->tl);
uintptr_t _0oplPFAP2 = (uintptr_t) (_list*) list;
list = (uintptr_t) _MyYL36dBI;
_F8B4P5Jtv = 0;
_list* _MYt9sb9t9 = (_list*) list;
_F8B4P5Jtv = isEmpty(_MYt9sb9t9);
_F8B4P5Jtv = !_F8B4P5Jtv;
}
int _BL3JW14hn = (int) _zQwQgL7su;
return _BL3JW14hn;
}

void spl_main() {
int _d1hqZplKy = 0;
_list* _SOxzcenNG = (_list*) x;
_d1hqZplKy = matrix_sum(_SOxzcenNG);
int _RddNUBVUS = (int) _d1hqZplKy;
printf("%i", _RddNUBVUS);
printf("\n");
}

int main() {
x = 0;
_list* _xZLTb8zdM = 0;
int _H4nBf54rw = 0;
_list* _7R89AH3eQ = 0;
int _hRsLN1L29 = 1;
_list* _hktvnlFdv = 0;
int _xeTkf7mMM = 2;
_list* _dGJafjUVW = malloc(sizeof(_list));
_dGJafjUVW->tl = 0;
_hktvnlFdv = malloc(sizeof(_list));
_hktvnlFdv->hd = (uintptr_t) _xeTkf7mMM;
_hktvnlFdv->tl = (uintptr_t) _dGJafjUVW;
_7R89AH3eQ = malloc(sizeof(_list));
_7R89AH3eQ->hd = (uintptr_t) _hRsLN1L29;
_7R89AH3eQ->tl = (uintptr_t) _hktvnlFdv;
_xZLTb8zdM = malloc(sizeof(_list));
_xZLTb8zdM->hd = (uintptr_t) _H4nBf54rw;
_xZLTb8zdM->tl = (uintptr_t) _7R89AH3eQ;
_list* _40kfnRXDB = 0;
_list* _wSM7RoHpj = 0;
int _GpnQn2AgF = 3;
_list* _gwtpUg0tE = 0;
int _hf53yQF88 = 4;
_list* _k6yVoT63w = 0;
int _KEEbRnQ6v = 5;
_list* _JGGaXZIyP = malloc(sizeof(_list));
_JGGaXZIyP->tl = 0;
_k6yVoT63w = malloc(sizeof(_list));
_k6yVoT63w->hd = (uintptr_t) _KEEbRnQ6v;
_k6yVoT63w->tl = (uintptr_t) _JGGaXZIyP;
_gwtpUg0tE = malloc(sizeof(_list));
_gwtpUg0tE->hd = (uintptr_t) _hf53yQF88;
_gwtpUg0tE->tl = (uintptr_t) _k6yVoT63w;
_wSM7RoHpj = malloc(sizeof(_list));
_wSM7RoHpj->hd = (uintptr_t) _GpnQn2AgF;
_wSM7RoHpj->tl = (uintptr_t) _gwtpUg0tE;
_list* _gVqaDLaMr = 0;
_list* _w0b4qzomc = 0;
int _FqKbr9OXe = 6;
_list* _n6SBH6j7R = 0;
int _hqD4V7kEE = 7;
_list* _afeBGqTH5 = 0;
int _fUPvCoHUr = 8;
_list* _BTPhC03QM = malloc(sizeof(_list));
_BTPhC03QM->tl = 0;
_afeBGqTH5 = malloc(sizeof(_list));
_afeBGqTH5->hd = (uintptr_t) _fUPvCoHUr;
_afeBGqTH5->tl = (uintptr_t) _BTPhC03QM;
_n6SBH6j7R = malloc(sizeof(_list));
_n6SBH6j7R->hd = (uintptr_t) _hqD4V7kEE;
_n6SBH6j7R->tl = (uintptr_t) _afeBGqTH5;
_w0b4qzomc = malloc(sizeof(_list));
_w0b4qzomc->hd = (uintptr_t) _FqKbr9OXe;
_w0b4qzomc->tl = (uintptr_t) _n6SBH6j7R;
_list* _QCHxUYgzj = malloc(sizeof(_list));
_QCHxUYgzj->tl = 0;
_gVqaDLaMr = malloc(sizeof(_list));
_gVqaDLaMr->hd = (uintptr_t) _w0b4qzomc;
_gVqaDLaMr->tl = (uintptr_t) _QCHxUYgzj;
_40kfnRXDB = malloc(sizeof(_list));
_40kfnRXDB->hd = (uintptr_t) _wSM7RoHpj;
_40kfnRXDB->tl = (uintptr_t) _gVqaDLaMr;
x = malloc(sizeof(_list));
x->hd = (uintptr_t) _xZLTb8zdM;
x->tl = (uintptr_t) _40kfnRXDB;
spl_main();
}

