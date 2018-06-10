#include <stdio.h>
#include <stdlib.h>
#include "lfsr.h"

/* 
 * This is a x^16 + x^14 + x^13 + x^11 + 1 Fibonacci LFSR.
 * See the Wikipedia article: https://en.wikipedia.org/wiki/Linear-feedback_shift_register#Fibonacci_LFSRs 
 */

// #define DEBUG __DEBUG__

state_t* 
new_lfsr()
{
    state_t* p = malloc(sizeof(state_t));
    p->state = 0;
#   ifdef DEBUG
    printf("new\n");
#   endif
    return p;
}

void
delete_lfsr(state_t* s)
{
    if(s)
        free(s);
}

void 
set_lfsr(state_t* s, uint32_t value) { 
    s->state = value; 
#   ifdef DEBUG
    printf("set %d\n", value);
#   endif
}

uint32_t
get_lfsr(state_t* s) { 
#   ifdef DEBUG
    printf("get -> %d\n", s->state);
#   endif
    return s->state; 
}


/* One step. Taps: 16 14 13 11; feedback polynomial: x^16 + x^14 + x^13 + x^11 + 1 */
void 
step_lfsr(state_t* s)
{
    uint32_t lfsr = s->state;
    uint32_t bit  = ((lfsr >> 0) ^ (lfsr >> 2) ^ (lfsr >> 3) ^ (lfsr >> 5) ) & 1;
    lfsr =  (lfsr >> 1) | (bit << 15);
    s->state = lfsr;
#   ifdef DEBUG
    printf("step (%d)\n", lfsr);
#   endif
}

/* multiple steps */
void
repeat_lfsr(state_t* s, int n) {
    for(int i=0; i<n; step_lfsr(s), i++);
}

uint32_t
step_get_lfsr(state_t* s) {
    step_lfsr(s);
    return get_lfsr(s);
}

double avg(state_t* s, int n) {
    double sum = 0;
    for(int i=0; i<n; i++, sum += step_get_lfsr(s));
    return sum;
}