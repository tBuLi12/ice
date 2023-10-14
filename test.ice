fun dupa() {
    Map(String, int){}
    Map(String, int)

    struct        : Name { name };
    tuple         : Name ( inner );
    arg_app       : Name [ inner ];
    variant       : Name . variant;
    variant value : Name . variant ( value );
    union         : default
}

type Tuple[T] (T, T)

type User[T] {
    name: String,
    age: int,
}

type Som (const T)

type Parser[const keywords: List[String]] {
    state: State,
}

fun getPlaceholderTypes(fmt: String): type (...: Show) {
    var types = type ();
    fmt.each.match {
        'i' => types = (...types, Int),
        's' => types = (...types, String),
        _   => {},
    }
    types
}

value <-> pattern
type  <-> type pattern

fun name[typeParams](args): returnType body

FUNCTIONS:
name       - ident
typeParams - list of type patterns
args       - list of pattern : type name
returnType - type name
body       - an expression

PATTERNS:
    match against a value of kind:
        tuple       :   (a, b, ...rest)
        struct      :   { a as a, b as b, ...rest }
        vairant     :   .variant(a)
        union       :   a : T
        named       :   Name(a)
        vector      :   [a, b, ...rest]
        reference   :   *a
        type        :   T is Trait
                    or  (A, B, ...Rest)  
                    or  { a as A, b as B, ...Rest }  
                    or  (A | B | ...Rest)  
                    or  { a as A | b as B | ...Rest } 
                    or  List[T]  
                    or  [T] 
                    or  ref T  
                    or  type   

(A | B | ...Rest) <- a union pattern

match against a non-union T: it's ambiguous
    A == B == T, Rest == (|)?
    A == B == T, Rest == (|T)?

match against a union T with not equal number of members: won't work
    missing members could be ! or elem of T


match against a union T with an equal number of members:
    find a permutation that matches, if none then error with ambiguity (eg?)
        fun eg[T1, T2](arg: T1 | T2) {}
            ^ - passing a 2 member union is ambiguous, specifying the parameters explicitly is not.


    impls must be proven inferrable (how?)
    ^ - actually anything that contains union patterns must be proven inferrable.
        so like: impls and match patterns? ye, basically

        define some f(type), the result of which can be intersected
        then:
            for tuple: map f over it and fold with &
            for struct: same
            for variant: same
            for named: map f over type arguments, fold with &
            for vector: f(item type)
            for reference: f(pointee)
            for primitives and types: f(.) = {} (some kind of zero value)

inference from unions is pretty limited...
    1. force typesets of all union members to be pairwise disjoint - stupid, but es
       ^ - sizes of unions cannot change during monomorphisation, must be equal to match


fun print[(...Ts: Show)](const fmt: String, ...rest: Ts): String 
          ^^^^^^^^^^^^^ - type pattern               ^^ - argument type: a type name or ? + type pattern
    where Ts = getPlaceholderTypes(fmt)
{
    var out = "";
    fmt.each c, i {
        match c {
            'i' => rest.(i)
            's' => rest.(i)
            _   => 
        }
    }
}

// works
fun parse[(...Ts)](getters: (...(String) -> Ts)): (...Ts) {
    let text = "dupa";

    getters.each(text)
}

// works
fun parse[(...Ts)](getters: (...(String) -> Ts?)): (|...Ts) {
    getters.each get {
        if (get() is Some(val)) {
            return val;
        }
    }
    panic()
} 

// works
fun fmt[(...Ts: Into[String])](const format: String, values: (...Ts)): String {
    const fragments = format.split("%v");
    if (value.len() != Ts.len()) {
        compileError("invalid number of placeholders");    
    }

    var output = "";

    fragments.interspearse(values).each.match {
        fragment: String => output += fragment,
        value            => output += value.toString(),
    }
    output
}


fun name [ GenericParameter ] ( parameter : ParameterType ) : ReturnType -> body
           ^^^^^^^^^^^^^^^^ -   typepat   - ^^^^^^^^^^^^^     ^^^^^^^^^^ - typename

[{Props}]
[{...Props,}]
[{Ts, Ts...}]

[(...(...Ts))]

Ts - a tuple of tuples
^^ - tf is that?
within expression:
    Kind::Type()


typeParms - concrete kinds

(Ts::each T => Set[T])
repr? it's a TS mapped type ig

so T must be some special kind here (iteration type variable?) (can't be same as in imperative code, due to inference)
T = TyKind::IterVar(&TyKind::TupleParameter) - guarantees uniqueness

and then what would Ts be
Ts = TyKind::Tuple, well, not rly. it is a tuple, but it's contents are unknown.
Ts = TyKind::TupleParameter - that's better

so (Ts::each T => Set[T]) = TyKind::Map(TyKind::TupleParameter, Type)




(() -> T for T in Ts)

def (...Ts: Default) as Default {
    
    static fun default(): (...Ts) {
                          ^^^^^^^ - this is Type::Tuple([TupleItem::Spread()])
        Ts::each T { T::default() }
                     ^^^^^^^^^^^^ this is T; yeah but wtf is T;
                                  well, T is a local constant of type Type::Type(TyKind::IterVar(&Ts));



    }

}

needed kinds:
TyKind::Type()
TyKind::TupleType(TyKind::Type)



TyKind & Value
generic syntax
iterator api
union matching
annotations
. vs ::, variant literals
lambdas
anonymous impls
pipeline
> control flow + patterns (ast? iiv?)

def Trait[params] {
    ... impls go here, regular functions + capture
}

let a = b ?? return 5;

fun stuff[T, ...|Ts, ...Ts, {...,Ts}, {...|Ts}](other: String, const param: type): List[T] {



    const V = List[T];          tc needs to undestand this
              Expr(Apply(Expr(Name("List")), [Expr(Name("T"))]))

    eval(Expr(Apply(Expr(Name("List")), [Expr(Name("T"))])))
        eval(Expr(Name("List"))) -> Value(Type(&List[_]))
        eval(Expr(Name("T")))    -> Value(Type(Parameter(1)))
            Value(Type(&List[Parameter(1)]))


    the constanst need to be stored somewhere both at check time and monomorph time
    same goes for normal variables, but only the type matters here, the value is ofc unknown

    so two tables

    vars[]
    consts[]


    const ss = s.slice(0, 3);   tc doesn't care about this
               ^^^^^^^^^^^^^ - comptime call

    let ss2 = s.slice(0, 3);
              ^^^^^^^^^^^^^ - runtime call    

    V::new()
    Expr(Call(Lookup(Expr(Name("V")), "new"), [])) -gettype-> 

    eval(Name("V")): look in scopes, found in topmost, a constant. it must have a value then, right? no.
    Value(Type(&List[Parameter(1)]))


    type  N = comecall(s); not ok
    const N = comecall(s); ok

    var a = N::new();
    a = N::new();

    
}



next up: what about reflection - we know, es.

your job:
    call gettype -> Type
    compare with return type

function repr:
    a name
    a bunch of const parameters
    a bunch of regular parameters
    some return type
    an expression

kind:
    struct
    tuple
    variant
    union
    ref
    vec
    named

within typenames: 
    Ts each T => List[T]

within expressions:
    collection.each item { stuff(item) }
    collection.each.stuff()

type Parser[const kws: List[String]] {
    state: State
}

fun zip[(*Ts)](lists: Ts each T => List[T, ]): Ts {
    s.split().each.name.len();
    s.split().each c { };
    
    embedding types in expressions:
    type()
    type {  }

    embedding expressions in types:
    # ?
    

    Ts.each T {
        just like that
    };
}

PIPELINE:
    tc known values
    mp known values
    rt known values
