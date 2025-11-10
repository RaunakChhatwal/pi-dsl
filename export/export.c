#include <stddef.h>
#include "HsFFI.h"

void pi_forall_init() {
  hs_init(NULL, NULL);
}

void pi_forall_exit() {
  hs_exit();
}
