#ifndef COMMON_ARRAY_H
#define COMMON_ARRAY_H

#define swap(type, a, b) do{type tmp=a; a=b; b=tmp;}while(0)

double abs_max_array(double* x, int begin, int end);
void scale_array(double* x, int begin, int end, double factor);

#endif
