#include <R.h>
#include <Rdefines.h>

/*
#include <ctype.h>

static int is_ascii_space(int c) {
    return (isspace(c) && isascii(c));
}

static int is_space_or_ascii_punct(int c) {
    return(isspace(c) || (ispunct(c) && isascii(c)));
}
*/

static int is_ascii_space(int c) {
    static const char *s = " \f\n\r\t\v";
    return strchr(s, c) == NULL ? 0 : 1;
}

static int is_ascii_space_or_punct(int c) {
    static const char *s = " \f\n\r\t\v!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";
    return strchr(s, c) == NULL ? 0 : 1;
}

static SEXP tm_scan_one(SEXP this, int (*test) ()) {
    SEXP y;
    Rboolean skip;
    int size = 256, i, j, nb = 0, ne = 0, u, v, w;
    int *beg, *end;
    const char *s;
    char c, *t, *p;
    cetype_t e;

    if(this == NA_STRING) {
	return ScalarString(NA_STRING);
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

    UNPROTECT(1);

    return y;
}


SEXP _tm_scan(SEXP x, SEXP which) {

    SEXP y, z, this;
    R_xlen_t i, j, k, nx, ny;
    int w;

    int (*test) () = is_ascii_space;

    if(LENGTH(which) > 0) {
	PROTECT(this = AS_INTEGER(which));
	w = INTEGER(this)[0];
	if(w == 1)
	    test = is_ascii_space_or_punct;
	UNPROTECT(1);
    }

    nx = LENGTH(x);

    if(nx < 1)
	return NEW_CHARACTER(0);

    if(nx == 1)
	return tm_scan_one(STRING_ELT(x, 0), test);

    PROTECT(z = NEW_LIST(nx));
    ny = 0;
    for(i = 0; i < nx; i++) {
	this = tm_scan_one(STRING_ELT(x, i), test);
	SET_VECTOR_ELT(z, i, this);
	ny += LENGTH(this);
    }
    // Now unlist.
    k = 0;
    PROTECT(y = NEW_STRING(ny));
    for(i = 0; i < nx; i++) {
	this = VECTOR_ELT(z, i);
	for(j = 0; j < LENGTH(this); j++, k++)
	    SET_STRING_ELT(y, k, STRING_ELT(this, j));
    }

    UNPROTECT(2);

    return y;
}
