#include <R.h>
#include <Rdefines.h>
#include <ctype.h>

SEXP _tm_remove_chars(SEXP x, SEXP which) {
    SEXP y, this;
    int n, i, w;
    const char *s;
    char c, *t, *p;
    cetype_t e;

    int (*test) () = ispunct;

    if(LENGTH(which) > 0) {
	PROTECT(this = AS_INTEGER(which));
	w = INTEGER(this)[0];
	if(w == 1)
	    test = isdigit;
	UNPROTECT(1);
    }

    PROTECT(x = AS_CHARACTER(x));
    n = LENGTH(x);
    
    PROTECT(y = NEW_CHARACTER(n));
    for(i = 0; i < n; i++) {
	this = STRING_ELT(x, i);
	if(this == NA_STRING) {
	    SET_STRING_ELT(y, i, NA_STRING);
	    continue;
	}
	e = getCharCE(this);
	s = CHAR(this);
	t = p = (char *) R_alloc(strlen(s) + 1, sizeof(char));
	while((c = *s++) != '\0') {
	    if(!test(c)) *t++ = c;
	}
	*t = '\0';
	SET_STRING_ELT(y, i, mkCharCE(p, e));
    }

    UNPROTECT(2);
    return y;
}
