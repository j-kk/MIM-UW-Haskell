# Bing

Język **Bing** wzoruje się na budowie języka latte.
Wzbogacony jest o konstrukcje for, zmienne read only (const),
elif (zapozyczenie z pythona).
### Kompilacja
```
make 
```
### Korzystanie
Interpreter przetwarza kolejne pliki podane w argumentach przy uruchamianiu. W przypadku braku czyta program ze standardowego wejścia.

Flaga `--help` daje podgląd parametrów programu/ 
### Przykłady:

Arithmetic operatos
```
// int

void main () {
    int a = 1;
    int b = 2;
    int c = 3;
    int d = a * b + c;
    print(d);
}
```
Expected output: 5


If, elif, else
```
void main2() {
    int a = 1;
    int b = 2;
    int c = 3;
    int d = a * b + c;
    bool var_true = True;

    if (a == 2) {
        println(a);
    } elif (b % 3 == 0) {
        println(b);
    } else {
        println(c);
    }

    print(d);
}
```
Expected output:
```
3
5
```
String operations

```
// string

void main() {
    string s = "abc";
    println(s);
}
```
Expected output:
```
abc
```
Recursion, loops and for loops
```
// factorial

int factorial_with_loop (int x) {
    int ilo = 1;
    int i = 1;
    while (i < x) {
        i = i + 1;
        ilo = ilo * i;
    }
    return ilo;
}

int factorial_recursive (int x) {
    if (x > 1) {
        return x * factorial_recursive(x-1);
    } else {
        return 1;
    }
}

int factorial_with_loop2 (int x) {
    int ilo = 1;
    for (i = 1 to x) {
        ilo = ilo * i;
    }
    return ilo;
}

void main() {
    println(factorial_with_loop(4));
    println(factorial_with_loop2(4));
    println(factorial_recursive(4));
}
```

Expected output
```
24
24
24
```

Const (with failing example) 
```
void ok() {
    int i = 3;
    i++;
    print(i);
}
void wrong() {
    const int i = 3;
    i++;
    print(i);
}
void main() {
    ok();
    wrong();
}

```
Expected output:
```
4Attempt to write to const variable: Ident "i"
```
Variable shadowing:
```
void main() {
    int a = 0;
    string b = "abc";
    {
        int a = 100;
        a++;
        bool b = False;
        println(a);
        println(b);
    }
    a++;
        println(a);
        println(b);
}

```
Expected output:
```
101
True
1
abc
```
Scopes: 
```
