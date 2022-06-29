#include <stdio.h>
#include <math.h>

double c_log1p(double x) {
    return (log1p(x));
}

double c_expm1(double x) {
    return (expm1(x));
}

// cc -shared smallc.c -o libsmall.so