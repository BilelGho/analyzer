extern int printf();

int abi1(int x) {
  return x + 48;
}

int abi2(int x) {
  return x + 64;
}

void proov(char** c) {
  printf("Korras.");
}

int main() {
  int a = abi1(0);
  int b = abi2(0);
  char c = 'A';
  return b - a - 16;
}

