#include <R.h>
#include <Rdefines.h>

/*
#include <ctype.h>

static int is_ascii_digit(int c) {
    return(isdigit(c) && isascii(c));
}

static int is_ascii_punct(int c) {
    return(ispunct(c) && isascii(c));
}
*/

static int is_ascii_digit(int c) {
    static const char *s = "0123456789";
    return strchr(s, c) == NULL ? 0 : 1;
}

static int is_ascii_punct(int c) {
    static const char *s = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
    return strchr(s, c) == NULL ? 0 : 1;
}

SEXP _tm_remove_chars(SEXP x, SEXP which) {
    SEXP y, this;
    int n, i, w;
    const char *s;
    char c, *t, *p;
    cetype_t e;

    int (*test) () = is_ascii_punct;

    if(LENGTH(which) > 0) {
	PROTECT(this = AS_INTEGER(which));
	w = INTEGER(this)[0];
	if(w == 1)
	    test = is_ascii_digit;
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
	    if(!test(c))
		*t++ = c;
	}
	*t = '\0';
	SET_STRING_ELT(y, i, mkCharCE(p, e));
    }
    setAttrib(y, R_NamesSymbol, getAttrib(x, R_NamesSymbol));

    UNPROTECT(2);
    return y;
}
