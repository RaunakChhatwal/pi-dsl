#include <stddef.h>
#include "HsFFI.h"

// Initialize the Haskell runtime system
void pi_dsl_init() {
  hs_init(NULL, NULL);
}

// Shut down the Haskell runtime system
void pi_dsl_exit() {
  hs_exit();
}