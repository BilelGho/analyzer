// PARAM: --set ana.activated[+] expsplit
#include <stddef.h>
#include <assert.h>
#include <goblint.h>

int main() {
  int r; // rand
  int x, y;
  int *p;

  __goblint_split_begin(p);
  if (r) {
    x = 1;
    p = &y;
  }
  else {
    x = 2;
    p = NULL;
  }

  assert((x == 1 && p == &y) || (x == 2 && p == NULL));

  __goblint_split_end(p);

  assert((x == 1 && p == &y) || (x == 2 && p == NULL)); // UNKNOWN (intentionally)

  return 0;
}