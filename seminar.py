from vepar import *


class T(TipoviTokena):
    PLUS, PUTA, OTV, ZATV, EQ, ZAREZ= '+*()=,'

    class I(Token):
        def mjesnost(self, funkcije): return 1

    class Z(Token):
        def mjesnost(self, funkcije): return 1

    class SC(Token):
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
            if lex.sadržaj == 'Z':
                yield lex.literal(T.Z)
            elif lex.sadržaj == 'I':
                yield lex.literal(T.I)
            elif lex.sadržaj == 'S' and lex.pogledaj() == 'c':
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
# program -> program | funkcija
# funkcija -> funkcija | IME OTV argumenti ZATV EQ izraz
# argumenti -> argumenti ZAREZ IME | IME (+JEDAN) | NULA | 
# izraz -> Z OTV izraz ZATV | I OTV izraz ZATV | SC OTV izraz ZATV | aritm | IME
# aritm -> aritm PLUS clan | aritm MINUS clan | clan
# clan -> clan ZVJEZDICA faktor | faktor
# faktor -> NUM | IME | OTV izraz ZATV | IME OTV izraz (ZAREZ izraz)* ZATV


class P(Parser):
    imeF = None
    argumentiLista = None

    def program(self):
        self.funkcije = Memorija()
        while not self > KRAJ:
            self.lastImeF = self.imeF if self.imeF else None
            self.lastArgumenti = self.argumentiLista if self.argumentiLista else None
            self.imeF = self >> T.IME
            self >> T.OTV
            self.argumentiLista = self.argumenti()
            self >> T.ZATV
            self >> T.EQ
            self.trenutnaFunkcija = self.izraz()
            if self.lastImeF == self.imeF and len(self.lastArgumenti) == len(self.argumentiLista):
                prekurzija = PRekurzija(*self.odrediBazuIKorak())
                del self.funkcije[self.lastImeF, len(self.lastArgumenti)]
                self.funkcije[self.imeF, len(self.argumentiLista)] = prekurzija, len(self.argumentiLista)
            else:
                self.funkcije[self.imeF, len(self.argumentiLista)] = Funkcija(self.imeF, self.argumentiLista, self.trenutnaFunkcija)
            self.lastFunkcija = self.funkcije[self.imeF, len(self.argumentiLista)]
        if not self.funkcije:
            raise SemantičkaGreška('Prazan program')
        return self.funkcije

    def odrediBazuIKorak(self):
        bazaPoziv = self.funkcije[self.lastImeF, len(self.lastArgumenti)]
        korakPoziv = self.funkcije[self.imeF, len(self.argumentiLista)]
        # return bazaPoziv, korakPoziv
        return bazaPoziv, Funkcija(self.imeF, self.argumentiLista, self.trenutnaFunkcija)

    def argumenti(self):
        args = [self >> T.IME]
        while(self >= T.ZAREZ):
            if var := self >= T.IME:
                args.append(var)
            elif self >= T.NULA:
                self.PR = True
                args.append(T.NULA)
                if self >= T.ZAREZ:
                    raise SintaksnaGreška("Jedini brojevi koji se mogu pojaviti kao argumenti funkcije su 0 i 1 u primitivnoj rekurziji po zadnjem argumentu")
        if self >= T.PLUS:
            self >> T.JEDAN
            if self > T.ZAREZ:
                raise SintaksnaGreška("Primitivna rekurzija je dopuštena samo po zadnjem argumentu")
        return args

    def izraz(self):
        t = self.clan()
        while self >= T.PLUS:
            t = Add2(t, self.clan())
        return t

    def clan(self):
        t = self.faktor()
        while op := self >= T.PUTA:
            t = Mul2(t, self.faktor())
        return t

    def faktor(self):
        if self >= T.OTV:   # Neka matematicka operacija unutar zagrada
            trenutni = self.izraz()
            self >> T.ZATV
        elif ime := self >= T.IME:
            if self > T.OTV:  # Poziv funkcije
                lijeva = ime
                desne = self.pozivFunkcije(ime)
                return Kompozicija(lijeva, desne)
            else:
                if ime not in self.argumentiLista:
                    raise SemantičkaGreška("Varijabla nije argument funkcije")
                return Projekcija(self.argumentiLista.index(ime) + 1, len(self.argumentiLista))
        num = self >> {T.NUM, T.NULA, T.JEDAN}
        return Zero(len(self.argumentiLista)) if num == 0 else Konstanta(num, len(self.argumentiLista))

    def pozivFunkcije(self, ime):
        self >> T.OTV
        izrazi = [self.izraz()]
        while self > T.ZAREZ:
            self >> T.ZAREZ
            izrazi.append(self.izraz())
        self >> T.ZATV
        if not (funkcija := self.funkcije[ime, len(izrazi)]):
            raise SemantičkaGreška(
                'Funkcija ' + ime + '^' + brArg + ' nije definirana')
        # return Poziv(ime, izrazi, funkcija)
        return izrazi

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


class Poziv(AST('ime argumenti funkcija')):
    def name(self):
        return str(self.ime)

    def _asdict(self):
        return {'izraz': self.ime}


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
        lijeva = self.lijeva.sadržaj if isinstance(
            self.lijeva, Token) else self.lijeva
        if isinstance(self.desne, type(Nenavedeno())):
            return {'': lijeva}
        return {'lijevi': lijeva, 'desni': self.desne}

    def name(self):
        return 'o'  # kompozicija


# class PRekurzija(AST('baza bazaArgs korak korakArgs')):
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


class Konstanta(AST('n k')):
    def mjesnost(self, funkcije):
        return self.k

    def _asdict(self):
        n = self.n if isinstance(self.n, int) else int(self.n.sadržaj)
        return {'': Sljedbenik(n-1, self.k)}

    def name(self):
        n = self.n if isinstance(self.n, int) else int(self.n.sadržaj)
        return 'C^1(' + str(n) + ')'


class Zero(AST('k')):
    def mjesnost(self, funkcije):
        return 1

    def name(self):
        return 'Z'

    def _asdict(self):
        return {'': Kompozicija(Projekcija(1, self.k), Nenavedeno())}


class Projekcija(AST('n k')):
    def mjesnost(self, funkcije):
        return n

    def name(self):
        return 'I^' + str(self.k) + '[' + str(self.n) + ']'

    def _asdict(self):
        return {'': None}


class Sljedbenik(AST('n k')):
    def mjesnost(self, funkcije):
        return 1

    def _asdict(self):
        n = self.n if isinstance(self.n, int) else int(self.n.sadržaj)
        return {'': Kompozicija(Sljedbenik(n-1, self.k), Nenavedeno()) if n > 0 else Kompozicija(Zero(self.k), Nenavedeno())}

    def name(self):
        return 'Sc'


class Add2(AST('lijevi desni')):
    def mjesnost(self, funkcije):
        return 2

    def _asdict(self):
        return {'lijevi': self.lijevi, 'desni': self.desni}

    def name(self):
        return 'add^2'


class Mul2(AST('lijevi desni')):
    def mjesnost(self, funkcije):
        return 2

    def name(self):
        return 'mul^2'

    def _asdict(self):
        lijevi = 'lijevi'
        desni = 'desni'
        return {lijevi: self.lijevi, desni: self.desni}


class Pow2(AST('baza na')):
    def mjesnost(self, funkcije):
        return 2

    def name(self):
        return 'pow'

    def _asdict(self):
        return {'baza': self.baza, 'na': self.na}


# p1 = P('''
# g(x)=x
# h(x,y,z)=
# f(x,0)=g(x)
# f(x,y+1)=
# ''')

p2 = P('''
g(x)=x
h(x,y,z) = z+1
f(x,0)=g(x)
f(x,y+1)=h(x, y, f(x,y))
''')
prikaz(p2)