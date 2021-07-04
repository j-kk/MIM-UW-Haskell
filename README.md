# MIM-UW-Haskell

Custom language interpreter made for Programming languages and paradigms class on MIM UW.
Interprets lanugage bing with syntax simmilar to Latte language.
Implements some extra functionalities like:

- static binding and shadowing 
- static type check
- const variables
- elif sturucture (based on python)

Both Latte and Bing grammar are attached to the repository in .cf files.


## Installation

In order to compile interpreter, first you must install following dependencies:
- alex
- happy
- bnfc

They are used to generate parser for given grammar.

Provided that, type `make` in Bing directory to build the interpreter.

### Usage

Interpreter processes all files provided in execution arguments. 
In case none are provided, it reads program from stdin.
`--help` flag provides detailed information how to use program flags.

### Examples:

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
