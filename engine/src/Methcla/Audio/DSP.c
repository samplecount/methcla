#include "Methcla/Audio/DSP.h"

void accumulate(float* restrict a, const float* restrict b, int n)
{
    for (int i=0; i < n; i++) {
        a[i] += b[i];
    }
}
