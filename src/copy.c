#include <Rinternals.h>

void tm_copyCorpus(SEXP x, SEXP y)
{
    copyVector(x, y);
}
