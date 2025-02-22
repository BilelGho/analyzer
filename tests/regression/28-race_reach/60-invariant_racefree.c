#include<pthread.h>
#include "racemacros.h"

int x = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  if (x == 0) {
    assert_racefree(x);
    pthread_mutex_unlock(&mutex);
  } else {
    pthread_mutex_unlock(&mutex);
    access(x);
  }
  return NULL;
}

int main(void) {
  create_threads(t);
  pthread_mutex_lock(&mutex);
  access(x);
  assert_racefree(x);
  pthread_mutex_unlock(&mutex);
  join_threads(t);
  return 0;
}
