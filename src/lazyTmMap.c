#include <R.h>
#include <Rdefines.h>

SEXP copyCorpus(SEXP x, SEXP y) {
    copyVector(x, y);
    R_do_slot_assign(x, mkString("CMetaData"), R_do_slot(y, mkString("CMetaData")));
    return x;
}
