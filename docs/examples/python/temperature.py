def c_to_f(c):
    return c * 9 / 5 + 32

def f_to_c(f):
    return (f - 32) * 5 / 9

boiling = c_to_f(100)
freezing = f_to_c(32)
print(boiling)
print(freezing)
