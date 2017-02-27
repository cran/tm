#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void tm_copyCorpus(SEXP x, SEXP y);
SEXP tm_tdm(SEXP stringsSEXP, SEXP remove_digitsSEXP, SEXP stopwordsSEXP, SEXP dictionarySEXP, SEXP min_term_freqSEXP, SEXP max_term_freqSEXP, SEXP min_word_lengthSEXP, SEXP max_word_lengthSEXP); 

static const R_CallMethodDef CallEntries[] = {
    {"tm_copyCorpus", (DL_FUNC) &tm_copyCorpus, 2},
    {"tm_tdm", (DL_FUNC) &tm_tdm, 8},
    {NULL, NULL, 0}
};

void R_init_tm(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
