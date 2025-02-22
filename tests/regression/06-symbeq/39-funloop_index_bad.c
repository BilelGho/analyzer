// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"  --set ana.activated[+] "'symb_locks'"
// copy of 06/02 with additional index accesses (that are wrong)
#include<pthread.h>
#include<stdio.h>

struct cache_entry {
  int refs;
  pthread_mutex_t refs_mutex;
} cache[10];

void cache_entry_addref(struct cache_entry *entry) {
  pthread_mutex_lock(&entry->refs_mutex);
  entry->refs++; // RACE!
  (*entry).refs++; // RACE!
  entry[1].refs++; // RACE!
  pthread_mutex_unlock(&entry->refs_mutex);
}

void *t_fun(void *arg) {
  int i;
  for(i=0; i<9; i++)
    cache_entry_addref(&cache[i]); // NORACE
  return NULL;
}

int main () {
  for (int i = 0; i < 10; i++)
    pthread_mutex_init(&cache[i].refs_mutex, NULL);

  int i;
  pthread_t t1;
  pthread_create(&t1, NULL, t_fun, NULL);
  for(i=0; i<9; i++)
    cache_entry_addref(&cache[i]); // NORACE
  return 0;
}
