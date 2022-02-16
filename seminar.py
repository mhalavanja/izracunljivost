from typing import Literal
from vepar import *


class T(TipoviTokena):
    PLUS, OTV, ZATV, EQ, ZAREZ = '+()=,'

    class Z(Token):
        literal = "Z" 
        def mjesnost(self, funkcije): return 1

    class SC(Token):
        literal = "Sc" 
        def mjesnost(self, funkcije): return 1

    class NUM(Token):
        pass
    
    class NULA(NUM):
        literal = '0'

    class JEDAN(NUM):
        literal = '1'

    class IME(Token):
        def mjesnost(self, funkcije): return funkcije[self][0]

        def _asdict(self):
            return {'': self.sadržaj}


def lexer(lex):
    for znak in lex:
        if znak.isspace():
            lex.zanemari()
        elif znak.isalpha():
            lex.zvijezda(str.isalnum)
            # if lex.sadržaj == 'Z':
            #     yield lex.literal(T.Z)
            # elif lex.sadržaj == 'I':
            #     yield lex.literal(T.I)
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

# BKG
# program -> program funkcija | funkcija
# funkcija -> primRek | IME OTV argumenti ZATV EQ izraz
# primRek -> baza korak
# baza  -> IME OTV argumenti ZAREZ NULA ZATV EQ izraz
# korak -> IME OTV argumenti PLUS JEDAN ZATV EQ izraz
# argumenti -> argumenti ZAREZ IME | IME
# izraz -> Z OTV izraz ZATV | SC OTV izraz ZATV | NUM | IME | IME OTV izraz (ZAREZ izraz)* ZATV | # OTV izraz ZATV 


class P(Parser):
    imeF = None
    argumentiLista = None

    def program(self):
        self.funkcije = Memorija()
        while not self > KRAJ:
            self.lastImeF = self.imeF if self.imeF else None
            self.lastArgumenti = self.argumentiLista if self.argumentiLista != None else None
            self.imeF = self >> T.IME
            self >> T.OTV
            self.argumentiLista = self.argumenti()
            self >> T.ZATV
            self >> T.EQ
            self.trenutnaFunkcija = self.izraz()
            if self.lastImeF == self.imeF and len(self.lastArgumenti) + 2 == len(self.argumentiLista):
                baza = self.funkcije[self.lastImeF, len(self.lastArgumenti)]
                korak = Funkcija(self.imeF, self.argumentiLista, self.trenutnaFunkcija)
                prekurzija = PRekurzija(baza, korak)
                del self.funkcije[self.lastImeF, len(self.lastArgumenti)]
                self.funkcije[self.imeF, len(self.argumentiLista) - 1] = prekurzija
            else:
                self.funkcije[self.imeF, len(self.argumentiLista)] = Funkcija(self.imeF, self.argumentiLista, self.trenutnaFunkcija)
        if not self.funkcije:
            raise SemantičkaGreška('Prazan program')
        return self.funkcije

    def argumenti(self):
        args = []
        if self >= T.NULA:
            self.PR = True
            return []
        elif arg := self >= T.IME:
            args.append(arg)
        while(self >= T.ZAREZ):
            if var := self >= T.IME:
                args.append(var)
            elif self >= T.NULA:
                self.PR = True
                if self >= T.ZAREZ:
                    raise SintaksnaGreška("Jedini brojevi koji se mogu pojaviti kao argumenti funkcije su 0 i 1 u primitivnoj rekurziji po zadnjem argumentu")
        if self >= T.PLUS:
            self >> T.JEDAN
            if self > T.ZAREZ:
                raise SintaksnaGreška("Primitivna rekurzija je dopuštena samo po zadnjem argumentu")
            # fxy = self.imeF.sadržaj + "("
            # for arg in args:
            #     fxy += arg.sadržaj + ","
            # fxy.removesuffix(',')
            # fxy += ")"
            # fxy = Token(T.IME, fxy)
            args.append("PR dummy argument")
        return args

    # def izraz(self):
    #     t = self.clan()
    #     while self >= T.PLUS:
    #         t = Add2(t, self.clan())
    #     return t

    def izraz(self):
        # if self >= T.OTV:   # Neka matematicka operacija unutar zagrada
        #     trenutni = self.izraz()
        #     self >> T.ZATV
        if self >= T.SC:
            self >> T.OTV
            izraz = self.izraz()
            self >> T.ZATV
            return Sljedbenik(izraz, len(self.argumentiLista))
        elif ime := self >= T.IME:
            if self > T.OTV:  # Poziv funkcije
                lijeva = ime
                
                self >> T.OTV
                izrazi = [self.izraz()]
                while self > T.ZAREZ:
                    self >> T.ZAREZ
                    izrazi.append(self.izraz())
                self >> T.ZATV
                if self.PR and ime == self.imeF and len(self.argumentiLista) == len(izrazi) + 1:
                    for i in range(len(izrazi) - 1):
                        if izrazi[i] != Projekcija(i + 1, len(self.argumentiLista)):
                            raise SemantičkaGreška('Funkcija ' + ime.sadržaj + '^' + str(len(izrazi)) + ' nije definirana ili krivo korištenje zadnjeg argumenta u koraku primitivne rekurzije')
                    # izrazi.append(Projekcija(len(self.argumentiLista), len(self.argumentiLista)))
                    # return ListaAST(izrazi) 
                    return Projekcija(len(self.argumentiLista), len(self.argumentiLista))
                elif (ime, len(izrazi)) not in self.funkcije:
                    raise SemantičkaGreška('Funkcija ' + ime.sadržaj + '^' + str(len(izrazi)) + ' nije definirana')
                desne =  izrazi
                return Kompozicija(lijeva, desne)
            else:
                if ime not in self.argumentiLista:
                    raise SemantičkaGreška("Varijabla nije argument funkcije")
                return Projekcija(self.argumentiLista.index(ime) + 1, len(self.argumentiLista))
        num = self >> {T.NUM, T.NULA, T.JEDAN}
        return Zero(len(self.argumentiLista)) if num ^ T.NULA  else Konstanta(num, len(self.argumentiLista))        

    start = program
    lexer = lexer

# AST
# funkcija: FIME|NULFUNKCIJA|Sc|KI:Token
#           Kompozicija: lijeva:funkcija desne:[funkcija]
#           PRekurzija: baza:funkcija korak:funkcija


kriva_mjesnost = SemantičkaGreška('Mjesnosti ne odgovaraju')


class Funkcija(AST('ime parametri izraz')):
    def name(self):
        # return str(self.ime.sadržaj) + '(' + self.strParametri() + ') ='
        return ''

    def strParametri(self):
        ret = ''
        for p in self.parametri:
            ret += str(p.sadržaj) + ', '
        return ret[:-2]

    def _asdict(self):
        return {'': self.izraz}


class Kompozicija(AST('lijeva desne')):
    def mjesnost(self, funkcije):
        l = self.lijeva.mjesnost(funkcije)
        if len(self.desne) != l:
            raise kriva_mjesnost
        G1, *ostale = self.desne
        k = G1.mjesnost(funkcije)
        if any(G.mjesnost(funkcije) != k for G in ostale):
            raise kriva_mjesnost
        return k

    def _asdict(self):
        lijeva = self.lijeva.sadržaj if isinstance(self.lijeva, Token) else self.lijeva
        if isinstance(self.desne, type(Nenavedeno())):
            return {'': lijeva}
        return {'lijevi': lijeva, 'desni': self.desne}
        # return {'': lijeva, '': self.desne}

    def name(self):
        return 'o'  # kompozicija


class PRekurzija(AST('baza korak')):
    def mjesnost(self, funkcije):
        k = self.baza.mjesnost(funkcije)
        if self.korak.mjesnost(funkcije) != k + 2:
            raise kriva_mjesnost
        return k + 1

    def _asdict(self):
        # return {'baza': self.baza.sadržaj + '^' + str(self.bazaArgs), 'korak': self.korak.sadržaj + '^' + str(self.korakArgs)}
        return {'baza': self.baza, 'korak': self.korak}

    def name(self):
        return 'PR'


class Konstanta(AST('num n')):
    def mjesnost(self, funkcije):
        return self.n

    def _asdict(self):
        num = self.numn if isinstance(self.num, int) else int(self.num.sadržaj)
        return {'': Sljedbenik(Token(T.NUM, num-1), self.n) if num > 1 else Sljedbenik(Zero(self.n), self.n)}

    def name(self):
        # n = self.n if isinstance(self.n, int) else int(self.n.sadržaj)
        # return 'C^1(' + str(n) + ')'
        return ''


class Zero(AST('n')):
    def mjesnost(self, funkcije):
        return 1

    def name(self):
        # return 'Z'
        return ''

    def _asdict(self):
        # return {'': Kompozicija(Projekcija(1, self.n), Nenavedeno())}
        return {'': Kompozicija("Z", Projekcija(1, self.n))}


class Projekcija(AST('k n')):
    def mjesnost(self, funkcije):
        return self.n

    def name(self):
        n = self.n if self.n > 0 else 1
        return 'I^' + str(n) + '[' + str(self.k) + ']'

    def _asdict(self):
        return {'': None}


class Sljedbenik(AST('izraz n')):
    def mjesnost(self, funkcije):
        return 1

    def _asdict(self):
        num = None
        if isinstance(self.izraz, int):
            num = self.izraz
        elif isinstance(self.izraz, AST0):
            # return {'': Kompozicija(self.izraz, Nenavedeno())}
            return {'': Kompozicija("Sc", self.izraz)}
        else:
            num = int(self.izraz.sadržaj)
        # return {'': Kompozicija(Zero(self.n) if num == 0 else Sljedbenik(num - 1, self.n), Nenavedeno())}
        return {'': Kompozicija("Sc", Zero(self.n) if num == 0 else Sljedbenik(num - 1, self.n))}

    def name(self):
        # return 'Sc'
        return ''


# p1 = P('''
# g(x)=x
# h(x,y,z)=
# f(x,0)=g(x)
# f(x,y+1)=
# ''')

p2 = P('''
f(x,z) = Sc(Sc(x))
add(x, 0) = x
add(x, y + 1) = Sc(add(x, y))
mul(x, 0) = 0
mul(x, y + 1) = add(x, mul(x, y))
fact(0) = 1
fact(n + 1) = mul(fact(n), Sc(n))
''')
prikaz(p2)