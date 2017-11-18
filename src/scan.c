#include <R.h>
#include <Rdefines.h>
#include <ctype.h>

int is_space_or_punct(int c) {
    return(isspace(c) || ispunct(c));
}

SEXP _tm_scan(SEXP x, SEXP which) {
    SEXP y, this;
    Rboolean skip;
    int size = 256, i, j, nb = 0, ne = 0, u, v, w;
    int *beg, *end;
    const char *s;
    char c, *t, *p;
    cetype_t e;

    int (*test) () = isspace;

    if(LENGTH(which) > 0) {
	PROTECT(this = AS_INTEGER(which));
	w = INTEGER(this)[0];
	if(w == 1)
	    test = is_space_or_punct;
	UNPROTECT(1);
    }

    if(LENGTH(x) < 1)
	error("invalid '%s' argument", "x");

    PROTECT(x = AS_CHARACTER(x));

    this = STRING_ELT(x, 0);
    if(this == NA_STRING) {
	UNPROTECT(1);
	return NA_STRING;
    }

    beg = Calloc(size, int);
    end = Calloc(size, int);

    e = getCharCE(this);
    s = CHAR(this);
    i = 0;
    skip = TRUE;
    while((c = *s++) != '\0') {
	if(skip && !test(c)) {
	    skip = FALSE; 
	    if(nb >= size) {
		if(size > INT_MAX / 2)
		    error("too many items");
		size *= 2;
		beg = Realloc(beg, size, int);
		end = Realloc(end, size, int);
	    }
	    beg[nb] = i;
	    nb++;
	}
	else if(!skip && test(c)) {
	    skip = TRUE;
	    end[ne] = i - 1;
	    ne++;
	}
	i++;
    }
    if(ne < nb) 
	end[ne] = i - 1;
    
    PROTECT(y = NEW_CHARACTER(nb));
    s = CHAR(this);
    v = -1;
    for(i = 0; i < nb; i++) {
	u = beg[i];
	s += (u - v - 1);
	v = end[i];
	w = v - u + 1;
	p = t = (char *) R_alloc(w + 1, sizeof(char));
	for(j = 0; j < w; j++) {
	    *t++ = *s++;
	}
	*t = '\0';
	SET_STRING_ELT(y, i, mkCharCE(p, e));
    }

    Free(beg);
    Free(end);

    UNPROTECT(2);

    return y;
}
