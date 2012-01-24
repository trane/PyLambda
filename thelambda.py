""" Church Boolean """
# tru = λt.λf.t
tru = lambda t: lambda f: t
# fls = λt.λf.f
fls = lambda t: lambda f: f
# test = λl.λm.λn. l m n
test = lambda l: lambda m: lambda n: (l)(m)(n)
# and = λb.λc. b c fls
And = lambda b: lambda c: (b)(c)(fls)
# or = λb.λc. b tru c
Or = lambda b: lambda c: (b)(tru)(c)
# not = λb. b fls tru
Not = lambda b: (b)(fls)(tru)
# pair = λf.λs.λb. b f s
pair = lambda f: lambda s: lambda b: (b)(f)(s)
# fst = λp. p tru
fst = lambda p: (p)(tru)
# snd = λp. p fls
snd = lambda p: (p)(fls)

""" Church Numerals """
# scc = λn. λs. λz. s (n s z)
scc = lambda n: lambda s: lambda z: (s)((n)(s)(z))
c0 = lambda s: lambda z: z
c1 = scc(c0)
c2 = scc(c1)
c3 = scc(c2)
# plus = λm. λn. λs. λz. m s (n s z)
plus = lambda m: lambda n: lambda s: lambda z: (m)(s)((n)(s)(z))
# times = λm. λn. m (plus n) c0
times = lambda m: lambda n: (m)(plus(n))(c0)
# exp = λm. λn. n (times m) c1
exp = lambda m: lambda n: (n)(times(m))(c1)
# zz = pair c0 c0
zz = pair(c0)(c0)
# ss = λp. pair (snd p) (plus c1 (snd p))
ss = lambda p: pair(snd(p))(plus(c1)(snd(p)))
# prd = λm. fst (m ss zz)
prd = lambda m: fst((m)(ss)(zz))
# sub = λm. λn. n prd m
sub = lambda m: lambda n: (n)(prd)(m)

""" Equality """
# iszro = λm. m (λx. fls) tru
iszro = lambda m: (m)(lambda x: fls)(tru)
# equal = λm. λn. and (iszro (m prd n))(iszro (n prd m))
equal = lambda m: lambda n: And(iszro((m)(prd)(n)))(iszro((n)(prd)(m)))
# gt = λm. λn. and (iszro (m prd n))(not iszro(n prd m))
gt = lambda m: lambda n: And(iszro((m)(prd)(n)))(Not(iszro((n)(prd)(m))))
# lt = λm. λn. and (iszro (n prd m))(not iszro(m prd n))
lt = lambda m: lambda n: And(iszro((n)(prd)(m)))(Not(iszro((m)(prd)(n))))

""" Lists """
# nil = λc. λn. n
nil = lambda c: lambda n: n
# cons = λh. λt. λc. λn . c h (t c n)
cons = lambda h: lambda t: lambda c: lambda n: ((c)(h))((t)(c)(n))
# isnil = λl. l (λh. λt. fls) tru
isnil = lambda l: (l)(lambda h: lambda t: fls)(tru)
# head = λl. l (λh. λt.  h) fls
head = lambda l: (l)(lambda h: lambda t: h)(fls)
# tail = λl.
#          fst (l (λx. λp. pair (snd p)(cons x (snd p)))
#                 (pair nil nil))
tail = lambda l: fst((l)(lambda x: lambda p: pair((snd(p))(cons(x)(snd(p))))(pair(nil)(nil))))

""" Convert λ<->λNB """
# realbool = λb. b true false
realbool = lambda b: (b)(True)(False)
# churchbool = λb. if b then tru else fls
churchbool = lambda b: (tru) if b else (fls)
# realeq = λm. λn. (equal m n) true false
realeq = lambda m: lambda n: (equal(m)(n))(True)(False)
# churcheq = λm. λn. if equal m n then tru else fls
churcheq = lambda m: lambda n: (tru) if m == n else (fls)
# realnat = λm. m (λn. succ n) 0
realnat = lambda m: (m)(lambda n: n + 1)(0)

""" Recursion """
# Z = λf. (λx. f (λy. x x y))(λx. f (λy. x x y))
Z = lambda f: (lambda x: f(lambda y: (x)(x)(y)))(lambda x: f(lambda y: (x)(x)(y)))
# g = λfct. λn. if realeq n c0 then c1 else (times n (fct (prd n))) ;
g = lambda fct: lambda n: (c0) if realeq(n)(c0) else (times(n)(fct((prd)(n))))
gNB = lambda fct: lambda n: 1 if n == 0 else (n * (fct(n-1)))
# factorial = Z g
factorial = Z(g)
factorialNB = Z(gNB)

