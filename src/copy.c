#include <Rinternals.h>

SEXP _tm_copyCorpus(SEXP x, SEXP y)
{
    copyVector(x, y);
    return R_NilValue;
}
