#include <R.h>
#include <Rdefines.h>

SEXP copyCorpus(SEXP x, SEXP y) {
    copyVector(x, y);
    setAttrib(x, install("CMetaData"), getAttrib(y, install("CMetaData")));
    return x;
}
