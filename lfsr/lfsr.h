#include <stdint.h>

typedef struct {
    uint32_t state;     // carries the LFSR state
 } state_t;

/* constructor */
state_t* 
new_lfsr();

/* destructor */
void 
delete_lfsr(state_t*);

/* setter */
void 
set_lfsr(state_t* s, uint32_t value);

/* getter */
uint32_t
get_lfsr(state_t* s);

/* One step. Taps: 16 14 13 11; feedback polynomial: x^16 + x^14 + x^13 + x^11 + 1 */
void 
step_lfsr(state_t* s);

/* multiple steps */
void
repeat_lfsr(state_t* s, int n);

/* calculate average over n iterations */
double 
avg(state_t* s, int n);