#include <Rinternals.h>

void _tm_copyCorpus(SEXP x, SEXP y)
{
    copyVector(x, y);
}
