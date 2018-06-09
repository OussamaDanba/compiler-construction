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

int call_count;
void spl_main();
int ackermann(int,int);

bool isEmpty(_list* x) {
	return x->tl == 0;
}

int RuntimeErr() {
	exit(-1);
}

void spl_main() {
int _XJtqBL90l = 0;
int _BPMTbGdBe = 3;
int _GTho5ujsc = 2;
_XJtqBL90l = ackermann(_BPMTbGdBe,_GTho5ujsc);
printf("%i", _XJtqBL90l);
printf("\n");
int _tSJccSOo8 = (int) call_count;
printf("%i", _tSJccSOo8);
printf("\n");
}

int ackermann(int x,int y) {
int _c9Xvz50M0 = 0;
int _VXNi0O2oK = (int) call_count;
int _MZMwW1EYw = 1;
_c9Xvz50M0 = _VXNi0O2oK + _MZMwW1EYw;
uintptr_t _T8XyeFMET = (uintptr_t) (int) call_count;
call_count = (uintptr_t) _c9Xvz50M0;
bool _6IIhEinBb = 0;
bool _Qiw2kDjlu = 0;
int _cRvwePOnR = (int) x;
int _b7BiqVjET = 0;
_Qiw2kDjlu = _cRvwePOnR < _b7BiqVjET;
bool _oXUeRfZsR = 0;
int _eKuwMQFkU = (int) y;
int _zsoeFVoE6 = 0;
_oXUeRfZsR = _eKuwMQFkU < _zsoeFVoE6;
_6IIhEinBb = _Qiw2kDjlu || _oXUeRfZsR;
if(_6IIhEinBb) {
int _Atl9FrfkO = -1;
return _Atl9FrfkO;
} else {
}
bool _vMbfwKvRp = 0;
int _oZnd5oJHH = (int) x;
int _LbxhUBqvL = 0;
_vMbfwKvRp = _oZnd5oJHH == _LbxhUBqvL;
if(_vMbfwKvRp) {
int _Qi0Gyfpdm = 0;
int _73mEZQYSF = (int) y;
int _s1pT176Xx = 1;
_Qi0Gyfpdm = _73mEZQYSF + _s1pT176Xx;
return _Qi0Gyfpdm;
} else {
}
bool _0VzumqKal = 0;
int _3sLRlSj50 = (int) y;
int _fqejTdBU8 = 0;
_0VzumqKal = _3sLRlSj50 == _fqejTdBU8;
if(_0VzumqKal) {
int _H0yqTSCPs = 0;
int _Ij1AC6z8d = 0;
int _ceRhsbDU3 = (int) x;
int _odBH2C7HQ = 1;
_Ij1AC6z8d = _ceRhsbDU3 - _odBH2C7HQ;
int _FdFmVNXeM = 1;
_H0yqTSCPs = ackermann(_Ij1AC6z8d,_FdFmVNXeM);
return _H0yqTSCPs;
} else {
}
int _bxg6SX0Qi = 0;
int _HlmsuKKgw = 0;
int _4pZTcnJmf = (int) x;
int _31hoi3Yoq = 1;
_HlmsuKKgw = _4pZTcnJmf - _31hoi3Yoq;
int _JhKCP1wAM = 0;
int _XIwfKSkNc = (int) x;
int _JN8NzreZE = 0;
int _wvmJXhgBw = (int) y;
int _dsLTeto9Z = 1;
_JN8NzreZE = _wvmJXhgBw - _dsLTeto9Z;
_JhKCP1wAM = ackermann(_XIwfKSkNc,_JN8NzreZE);
_bxg6SX0Qi = ackermann(_HlmsuKKgw,_JhKCP1wAM);
return _bxg6SX0Qi;
}

int main() {
call_count = 0;
spl_main();
}

