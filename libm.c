#include <stdio.h>
#include <math.h>

double c_log1p(double x) {
    return (log1p(x));
}

// cc -shared smallc.c -o libsmall.so