# enteros.py - Me entero mejor que en racket...
cero = lambda f: lambda x: x
suc = lambda n: lambda f: lambda x: f((n(f))(x))
comprobar = lambda n: n(lambda x: x + 1)(0)
suma = lambda n: lambda m: n(lambda x: suc(x))(m)
prod = lambda n: lambda m: lambda f: lambda x: m(n(f))(x)
