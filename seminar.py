from cgi import test
from vepar import *


class T(TipoviTokena):
    PLUS, OTV, ZATV, EQ, ZAREZ = '+()=,'

    class Z(Token):
        literal = "Z" 

    class SC(Token):
        literal = "Sc" 

    class NUM(Token):
        pass
    
    class NULA(NUM):
        literal = '0'

    class JEDAN(NUM):
        literal = '1'

    class IME(Token):
        def _asdict(self):
            return {'': self.sadržaj}


def lexer(lex):
    for znak in lex:
        if znak.isspace():
            lex.zanemari()
        elif znak.isalpha():
            lex.zvijezda(str.isalnum)
            if lex.sadržaj == 'S' and lex.pogledaj() == 'c':
                lex.čitaj()
                yield lex.literal(T.SC)
            yield lex.literal(T.IME)
        elif znak.isdecimal():
            lex.prirodni_broj(znak, nula=True)
            if lex.sadržaj == '0':
                yield lex.literal(T.NULA)
            elif lex.sadržaj == '1':
                yield lex.literal(T.JEDAN)
            else:
                yield lex.token(T.NUM)
        else:
            yield lex.literal(T)

# (B)KG
# program -> program funkcija | funkcija
# funkcija -> primRek | IME OTV argumenti ZATV EQ izraz
# primRek -> baza korak
# baza  -> IME OTV argumenti ZAREZ NULA ZATV EQ izraz
# korak -> IME OTV argumenti PLUS JEDAN ZATV EQ izraz
# argumenti -> argumenti ZAREZ IME | IME
# izraz -> SC OTV izraz ZATV | NUM | IME | IME OTV izraz (ZAREZ izraz)* ZATV


class P(Parser):
    imeF = None
    argumentiLista = None

    def program(self):
        self.funkcije = Memorija()
        self.bazaPR = None
        self.PR = False
        self.mjesnost = None
        self.lastMjesnost = None
        while not self > KRAJ:
            self.lastImeF = self.imeF if self.imeF else None
            self.lastMjesnost = self.mjesnost if self.mjesnost != None else None
            self.imeF = self >> T.IME
            self.imeF = self.imeF.sadržaj
            self >> T.OTV
            self.argumentiLista, self.mjesnost = self.argumentiIMjesnost()
            self >> T.ZATV
            self >> T.EQ
            self.trenutnaFunkcija = self.izraz()
            if self.PR and self.bazaPR != None and self.lastImeF == self.imeF and self.lastMjesnost + 2 == self.mjesnost:
                korak = self.trenutnaFunkcija
                prekurzija = PRekurzija(self.bazaPR, korak)
                if (self.imeF, self.mjesnost - 1) in self.funkcije: raise SemantičkaGreška("Redeklaracija funkcije " + self.imeF + "^" + str(self.mjesnost - 1))
                self.funkcije[self.imeF, self.mjesnost - 1] = prekurzija
                self.PR = False
                self.bazaPR = None
            else:
                if self.PR and not self.bazaPR:
                    if self.mjesnost == 0 : 
                        if isinstance(self.trenutnaFunkcija.desna, Konstanta):
                            self.bazaPR = int(self.trenutnaFunkcija.desna.num + 1)
                        elif isinstance(self.trenutnaFunkcija.lijeva, Zero):
                            self.bazaPR = 0
                        else:
                            raise(SemantičkaGreška("Primitivna degenerirana rekurzija za bazu mora imati konstantu"))
                    else:
                        self.bazaPR = self.trenutnaFunkcija
                elif self.PR and self.bazaPR: raise SemantičkaGreška("Primitivna rekurzija bez koraka rekurzije")
                else: 
                    if (self.imeF, self.mjesnost) in self.funkcije: raise SemantičkaGreška("Redeklaracija funkcije " + self.imeF + "^" + str(self.mjesnost))
                    self.funkcije[self.imeF, self.mjesnost] = self.trenutnaFunkcija
        if self.bazaPR: raise SemantičkaGreška("Zadnja funkcija je baza primitivne rekurzije koja nema korak")
        if not self.funkcije:
            raise SemantičkaGreška('Prazan program')
        return self.funkcije

    def argumentiIMjesnost(self):
        args, mjesnost = [],  0
        arg = None
        if self >= T.NULA:
            self.PR = True
            return [], 0
        elif arg := self >= T.IME:
            args.append(arg)
            mjesnost += 1
        while(self >= T.ZAREZ):
            arg = None
            if arg := self >= T.IME:
                args.append(arg)
                mjesnost += 1
            elif self >= T.NULA:
                self.PR = True
                if not self > T.ZATV:
                    raise SintaksnaGreška("Nakon 0 u primitivnoj rekurziji po zadnjem argumentu mora doći ')'")
            else: raise SintaksnaGreška("Zarez viška u deklaraciji argumenata funkcije " + self.imeF)
        if self >= T.PLUS:
            if not arg: raise SintaksnaGreška("+1 u koraku primitivne rekurzije mora doći nakon neke varijable") 
            self >> T.JEDAN
            if self > T.ZAREZ:
                raise SintaksnaGreška("Primitivna rekurzija je dopuštena samo po zadnjem argumentu")
            if not self.PR:
                raise SemantičkaGreška("Primitivna rekurzija bez baze rekurzije")
            mjesnost += 1
        if not mjesnost: raise SemantičkaGreška("Funkcija mora imati barem jedan argument")
        if len(args) != len(set(args)): raise SemantičkaGreška("U deklaraciji funkcije, svaki argument se može pojaviti najviše jednom")
        return args, mjesnost

    def izraz(self):
        if self >= T.SC:
            self >> T.OTV
            izraz = self.izraz()
            self >> T.ZATV
            return Kompozicija(Sljedbenik(), izraz)
        elif ime := self >= T.IME:
            if self > T.OTV:
                ime = ime.sadržaj
                
                self >> T.OTV
                izrazi = [self.izraz()]
                while self > T.ZAREZ:
                    self >> T.ZAREZ
                    izrazi.append(self.izraz())
                self >> T.ZATV
                if self.PR and ime == self.imeF and self.mjesnost == len(izrazi) + 1:
                    for i in range(len(izrazi)):
                        if izrazi[i] != Projekcija(i + 1, self.mjesnost):
                            raise SemantičkaGreška('Funkcija ' + ime + '^' + str(len(izrazi)) + ' nije definirana ili krivo korištenje zadnjeg argumenta u koraku primitivne rekurzije')
                    return Projekcija(self.mjesnost, self.mjesnost)
                elif (ime, len(izrazi)) not in self.funkcije:
                    raise SemantičkaGreška('Funkcija ' + ime + '^' + str(len(izrazi)) + ' nije definirana')
                if len(izrazi) == 1: izrazi = izrazi[0]
                return Kompozicija(ime, izrazi)
            else:
                if ime not in self.argumentiLista:
                    raise SemantičkaGreška("Varijabla nije argument funkcije")
                return Projekcija(self.argumentiLista.index(ime) + 1, self.mjesnost)
        num = self >> {T.NUM, T.NULA, T.JEDAN}
        return Kompozicija(Zero(), Projekcija(1, self.mjesnost)) if num ^ T.NULA else Kompozicija(Sljedbenik(), Konstanta(int(num.sadržaj) - 1, self.mjesnost))        

    start = program
    lexer = lexer

### AST
# Kompozicija: lijeva:poziv desna:[poziv]
# PRekurzija: baza:(poziv | broj:NUM) korak:poziv
# Projekcija: n:NUM k:NUM
#
# poziv: ime:IME | Sljedbenik | Zero | Kompozicija | Projekcija       
#
#
# Konstanta: num:NUM n:NUM
# Konstanta se ne prikazuje u zavrsnom stablu jer nije inicijalna funkcija, 
# ali se koristi kao pomocni AST


class Kompozicija(AST('lijeva desna')):
    def name(self):
        return 'o'


class PRekurzija(AST('baza korak')):
    def _asdict(self):
        return {'baza': self.baza, 'korak': self.korak}

    def name(self):
        return 'PR'


class Konstanta(AST('num n')):
    def _asdict(self):
        num = self.num if isinstance(self.num, int) else int(self.num.sadržaj)
        return {'': Kompozicija(Sljedbenik(), Konstanta(num-1, self.n)) if num > 0 else Kompozicija(Zero(), Projekcija(1, self.n))}

    def name(self):
        return ''


class Zero(AST('')):
    def name(self):
        return 'Z'


class Projekcija(AST('k n')):
    def name(self):
        n = self.n if self.n > 0 else 1
        return 'I^' + str(n) + '[' + str(self.k) + ']'

    def _asdict(self):
        return {}


class Sljedbenik(AST('')):
    def name(self):
        return 'Sc'

p0 = P('''
add(x, 0) = x
add(x, y + 1) = Sc(add(x, y))
mul(x, 0) = 0
mul(x, y + 1) = add(x, mul(x, y))
fact(0) = 1
fact(n + 1) = mul(fact(n), Sc(n))
f(x, y, 0) = add(x, y)
f(x, y, z+1) = mul(f(x, y, z), y)
f(x)=x
g(x) = f(x)
f(x,z)=mul(add(x,1), add(z,2))
z(z) = fact(z)
x(x) = z(x)
a(x, 0) = 0
a(x, y+1) = 0
''')


prikaz(p0)

import unittest


class TestCases(unittest.TestCase):
    def test1(self):
        with self.assertRaises(SemantičkaGreška):
             P("h(x, 0) = 1")
             
    def test2(self):
        with self.assertRaises(SemantičkaGreška):
            P("g(x + 1) = x")

    def test3(self):
        with self.assertRaises(SemantičkaGreška):
             P("g(y, y) = y")

    def test4(self):
        with self.assertRaises(SemantičkaGreška):
            P("g(x) = z")

    def test5(self):
        with self.assertRaises(SemantičkaGreška):
            P('''
                z(z, x) = z
                x(x) = z(x)
            ''')

    def test6(self):
        with self.assertRaises(SemantičkaGreška):
            P('''
                a(x, 0) = x
                a(x, n+1) = a(x, b)
            ''')

    def test7(self):
        with self.assertRaises(SemantičkaGreška):
            P('''
                a(x) = x
                a(y) = y
            ''')

    def test8(self):
        with self.assertRaises(SemantičkaGreška):
            P('''
                a(x, 0) = x
                a(x, n+1) = a(n, x)
            ''')

    def test9(self):
        with self.assertRaises(SemantičkaGreška):
            P('''
                a(x, 0) = x
                a(x, n+1) = a(x)
            ''')

    def test10(self):
        with self.assertRaises(SintaksnaGreška):
            P('''
                a(0+1) = 0
                a(n+1) = a(n)
            ''')

    def test11(self):
        with self.assertRaises(SintaksnaGreška):
            P('''
                a(0, n) = 0
                a(x+1, n) = a(n)
            ''')

    def test12(self):
        with self.assertRaises(SintaksnaGreška):
            P('''
                a(n, 0) = 0
                a(x+1, n) = a(n)
            ''')

    def test11(self):
        with self.assertRaises(SintaksnaGreška):
            P('''
                a(0, n) = 0
                a(x+1, n) = a(n)
            ''')


unittest.main()