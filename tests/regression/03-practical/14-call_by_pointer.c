#include <assert.h>

/**
 * foo /migh/ call the argument function
 */
extern void foo(void (*)(void));

int glob;

void reset_glob(void)
{
  int n;
  glob = n;
}

int main()
{
  glob = 0;
  foo(reset_glob);
  assert_unknown(glob);
  return 0;
}
