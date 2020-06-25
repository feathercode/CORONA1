#include <stdlib.h>
void xfr_foo(int *nin, double *x) {
	int i, n= nin[0];
	for (i=0; i<n; i++) x[i]= x[i]*x[i];
}
