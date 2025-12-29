#include <stddef.h>
#include "HsFFI.h"

void pi_dsl_init() {
  hs_init(NULL, NULL);
}

void pi_dsl_exit() {
  hs_exit();
}
