// https://aosabook.org/en/v1/llvm.html
unsigned add1(unsigned a, unsigned b) { return a + b; }

// Perhaps not the most efficient way to add two numbers.
unsigned add2(unsigned a, unsigned b) {
  if (a == 0)
    return b;
  return add2(a - 1, b + 1);
}
