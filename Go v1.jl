#Determina si el elemento e se ecuentra en lla lista lis, la lista debe ser dada entre []
#taaqui(lis,e)= lis==[] ? false : (e==pop!(lis) || taaqui(lis,e))
#=
using Gadfly
using PyPlot
using LinearAlgebra
=#

"""
    taaqui(lis,e)
Entrega el valor boleano sobre si "e" se encuentra en la lista "lis"
"""
function taaqui(lis,e)
    l2=copy(lis)
    l2==[] ? false : (e==pop!(l2) || taaqui(l2,e))
end # functiontaaqui(lis,e)


#Da el producto de todos lls elementos de una lista
producto(lis)= lis==[] ? 1 : pop!(lis)*producto(lis)


# Da todas las posiciones posibles en un tablero de nxn
function tablero(n::Int)
    lis=[]

    for i in 1:n, j in 1:n
        push!(lis, (i,j))
    end # for
    lis
end


function sinrep(lis)
    if length(lis)==0
        lis
    else
        a=pop!(lis);
        lis2=copy(lis)
        (taaqui(lis2,a) ? sinrep(lis) : push!(sinrep(lis),a))
    end # if
end # function

function vxy(n::Int,m::Int)
    lis=[]
    for i in (n-1):(n+1)
        push!(lis,(i,m))
    end
    for j in (m-1):(m+1)
        push!(lis,(n,j))
    end # for
    sinrep(lis)
end # function



vxy(pos::Tuple{Int,Int})=vxy(pos[1],pos[2])


govecindad(pos::Tuple{Int,Int},n::Int)=filter(x -> taaqui(tablero(n),x),vxy(pos))
govecindad(i::Int,j::Int,n::Int)=filter(x -> taaqui(tablero(n),x),vxy(i,j))

#tiene que tener una lista en la que todos sus elementos esten entre parentesis con dos elementos
function quiparentesis(lis)
    if lis==[]
        lis
    else
        a=pop!(lis)
        push!(push!(quiparentesis(lis),a[1]),a[2])
    end
end # function


function quita(lis1, lis2)
    if lis2==[]
        lis1
    else
        b=copy(lis2)
        a=pop!(b)
        quita(filter(x -> x!=a,lis1),b)
    end
end # function

libres1(pos,bla,neg,n)=quita(govecindad(pos,n),union(bla,neg))

#libres1((2,1),[(1,1),(6,5)],[(1,2),(7,5)],9)#->'((3 1) (2 2) (2 1))

function woba(pos,bla, neg)
    if taaqui(bla,pos)
        2
    elseif taaqui(neg,pos)
        3
    else
        1
    end
end # function

wob(pos,bla,neg)=woba(pos,bla,[])*woba(pos,[],neg)

#wob((6,5),[(1,1),(6,5)],[(6,5),(2,1),(7,5)])


function mimap(lis,f)
    if lis==[]
        lis
    else
        a=pop!(lis)
        push!(mimap(lis,f),f(a))
    end # if
end

#la lista debe tener una longitud par y la funcion f debe aceptar dos variables
function mapde2(lis,f)
    if lis==[]
        []
    else
        a=pop!(lis)
        b=pop!(lis)
        push!(mapde2(lis,f),f(a,b))
    end
end # function

#mapde2([1,2,3,4,5,6],(x,y)->(x+y,factorial(y)))

#mimap([1,2,3,4,5,6],x -> factorial(x))

function sigbla(pos,bla,vec)
    if intersect(bla,vec)==[]
        1
    else
        2
    end
end # function



function signeg(pos,neg,vec)
    if intersect(neg,vec)==[]
        1
    else
        3
    end
end



function wobsig(pos,bla,neg,n)
    vec=govecindad(pos,n)
    sigbla(pos,bla,vec)*signeg(pos,neg,vec)
end

#wobsig((8,5),[(1,1),(6,5)],[(2,1),(7,5)],9)

ismult3(k)=isinteger(k/3)

generabl(bla,neg,fbl,fne,n)=filter((p -> iseven(wobsig(p,union(bla,fbl),union(neg,fne),n))),quita(tablero(n),union(bla,neg)))

generane(bla,neg,fbl,fne,n)=filter((p -> ismult3(wobsig(p,union(bla,fbl),union(neg,fne),n))),quita(tablero(n),union(bla,neg)))

#generane([(1,2),(2,1)],[(3,1)],[],[],4)
#generane([(1,2),(2,1)],[(3,1)],[],[(3,2),(4,1),(2,2),(3,3),(4,2)],4)

liguales(lis1,lis2)= (quita(lis1,lis2)==[]) & (quita(lis2,lis1)==[])

#liguales([1,2,3],[1,1,1,1,2,2,2,3])


function evaluaauxbl(bla,neg,fbl,fne,n,m)
    sigfbl=generabl(bla,neg,fbl,fne,n)
    if sigfbl==fbl
        fbl
    else
        evaluaauxbl(bla,neg,sigfbl,fne,n,m-1)
    end
end

#evaluaauxne([(1,2),(2,1)],[(3,1)],[],[],4,16)

function evaluaauxne(bla,neg,fbl,fne,n,m)
    sigfne=generane(bla,neg,fbl,fne,n)
    if fne==sigfne
        fne
    else
        evaluaauxne(bla,neg,fbl,sigfne,n,m-1)
    end
end

function evalua(bla,neg,n)
    bl=evaluaauxbl(bla,neg,[],[],n,n*n)
    ne=evaluaauxne(bla,neg,[],[],n,n*n)
    union(bla,quita(bl,ne)),union(neg,quita(ne,bl))
end # function

#quita(evaluaauxbl([(1,2),(2,1)],[(3,1)],[],[],4,16),evaluaauxne([(1,2),(2,1)],[(3,1)],[],[],4,16))

#evaluador([(1,2),(2,1)],[(3,1)],4)

evaluador(bla,neg,n)=map(length,evalua(bla,neg,n))

#pruebabla=[(1,3),(1,4),(1,5),(2,1),(2,2),(2,4),(3,1),(3,3),(3,4),(3,6),(3,9),(4,1),(4,3),(4,4),(4,5),(4,6),(4,8),(4,9),(5,3),(5,8),(6,4),(6,5),(6,7),(6,8),(7,5),(7,6),(7,7),(8,8),(8,6),(9,7)]

#pruebaneg=[(1,6),(1,8),(2,9),(2,7),(2,6),(2,5),(3,2),(3,5),(3,7),(3,8),(4,2),(4,7),(5,1),(5,2),(5,4),(5,5),(5,6),(5,7),(6,1),(6,3),(6,6),(7,2),(7,3),(7,4),(8,4),(8,5),(9,3),(9,5),(9,6)]

function sinrep(lis)
    lisal=copy(lis)
    if length(lisal)==0
        lisal

    else
        a=pop!(lisal);
        lis2=copy(lisal)
        (taaqui(lis2,a) ? sinrep(lisal) : push!(sinrep(lisal),a))
    end # if
end # function

#para unir lista de listas
function miunion(lis)
    l2=copy(lis)
    if length(l2)==0
        []
    else
        union(pop!(l2),miunion(l2))
    end # if
end # function


function miappend(lis)
    l2=copy(lis)
    if length(l2)==0
        []
    else
        a=pop!(l2)
        l3=copy(l2)
        append!(a,miappend(l3))
    end # if
end # function

function cadena(con,cad,n)
    nuevos=filter((pos->taaqui(con,pos)),miunion(map(p->govecindad(p,n),cad)))
    res=quita(sinrep(nuevos),cad)
    sinrep((length(res)==0 ? cad : cadena(con,nuevos,n)))
end # function

#=cadena([(3,4),(4,4),(4,5),(5,5),(5,6),(1,9)],[(5,5)],9)

filter((pos->taaqui([(3,4),(4,3),(4,5),(5,5),(5,6),(1,9)],pos)),miunion(map(p->govecindad(p,9),[(5,5)])))

map(p->govecindad(p,9),[(5,5),(1,1)])

miunion([[(4,5),(5,4)],[(1,1)]])0=#

veccad(cad,n)=miunion(map(pos->govecindad(pos,n),cad))

#println(veccad([(1,1),(1,2),(2,2)],9))

libertad(ami,ene,cad,n)=quita(veccad(cad,n),union(ene,ami))
#libertad([(1,2),(2,1),(2,2)],[(1,3),(3,1)],[(1,2),(2,1),(2,2)],9)


function libertadp(bl,ne,pos,n)
    cade=(taaqui(bl,pos) ? cadena(bl,[pos],n) : (taaqui(ne,pos) ? cadena(ne,[pos],n) : []))
    libertad(bl,ne,cade,n)
end # function

#print(libertadp([(1,2),(2,1),(2,2)],[(1,3),(3,1)],(2,1),9))

function confcad(conf,n)
    if conf==[]
        []
    else
        caden=cadena(conf,[first(conf)],n);
        union([caden],confcad(quita(conf,caden),n))
    end
end # function

#print(confcad([(8,7),(9,7),(9,5),(9,3),(9,2),(5,1),(6,3),(8,3),(6,2),(7,2),(9,6),(1,2),(6,1),(7,9),(9,4),(7,7),(3,2),(8,8),(8,2),(5,2),(8,5)],9))


function ataque(enevc,pos,n,m)
    nvc=copy(enevc)
    if nvc==[]
        m
    else
        if taaqui(pop!(nvc),pos)
            ataque(nvc,pos,n,m+1)
        else
            ataque(nvc,pos,n,m)
        end
    end
end # function

#ataque([[(2,1)],[(5,4),(3,4),(4,5),(4,3)],[(3,4),(2,4)]],(3,4),9,0)



mata(ami,ene,pos,n)=sinrep(miunion(map((p->cadena(ene,[p],n)),filter((po->(taaqui(ene,po)  && (libertadp(append!([pos],copy(ami)),ene,po,n)==[]))),govecindad(pos,n)))))

#print(mata([(1,3),(2,2)],[(1,1),(1,2)],(2,1),9))

#=
function mataf(ami,ene,pos,n)
    A=copy(ami)
    map((po->(libertadp(append!(A,[pos]),ene,po,n))),intersect(govecindad(pos,n),ene))

end

libertadp([(1,3),(2,2),(2,1)],[(1,1),(1,2)],(1,1),9)==[]

govecindad((2,1),9)

append!([(2,1)],copy([(1,3),(2,2)]))

mataf([(1,3),(2,2)],[(1,1),(1,2)],(2,1),9)

taaqui([(1,1),(1,2)],(1,1))

true & true =#



juegacadena(ami,ene,n)=map((cad-> [libertad(ami,ene,cad,n),length(cad)]),confcad(ami,n))

#print(first(juegacadena([(1,1),(1,2),(4,4),(2,3)],[(2,2),(1,3),(5,1)],9)))


function jugcad(cadenn,pos,m)
    caden=copy(cadenn)
    if length(cadenn)==0
        m
    else
        A=pop!(caden)
        if taaqui(first(A),pos)
            jugcad(caden,pos,m+A[2])
        else
            jugcad(caden,pos,m)
        end
    end # if
end

suma(lis)= isempty(lis) ? 0 : sum(lis)

function jugadacad(ami,ene,pos,n,enevc,jcad)
    vpos=govecindad(pos,n)
    jjj=jugcad(jcad,pos,n)
    [jjj-1,( #= jjj==1 ? 1 : =# suma(map(x->x[2],juegacadena(union([pos],ami),ene,n)))-suma(map(x->x[2],jcad))),(suma(map(x->x[2],juegacadena(ene,union([pos],ami),n)))-suma(map(x->x[2],jcad))),length(mata(ami,ene,pos,n))]
end # function



#jugcad([[[(1,2),(2,2),(1,1)],3],[[(3,3)],1]],(2,3),1)

#=letra=[1,3,"w",4,5]

letra[3]=#


function juegaal(ami,ene,pos,n)
    if pos==(0,0)
        [ene,ami]
    else
        [quita(ene,mata(ami,ene,pos,n)),append!([pos],copy(ami))]
    end # if
end # function


#=function partida(tab,jug,n,m)
    if length(jug)=0
        iseven(m) ? tab : reverse(tab)
    else
        A=copy(jug)
        first
    end # if
end # function=#




function partida(tab,jug,n,m)
    if isempty(jug)
        iseven(m) ? tab : reverse(tab)
    else
        A=copy(jug)
        b=popfirst!(A)
        partida(juegaal(first(tab),tab[2],b,n),A,n,m+1)
    end # if
end # function

partida(jug,n)=partida([[],[]],jug,n,0)

#=
print(juegaal([(1,1),(1,4),(2,4),(3,3),(3,5)],[(3,4),(4,3),(4,5),(5,4)],(4,4),9))

print(partida([(1,1),(1,2),(0,0),(2,1),(0,0)],9))

print(juegaal([(1,2)],[(1,1)],(2,1),9))

print(partida([(4,4),(5,4),(6,4),(4,5),(5,5),(4,3),(1,1),(3,4)],9))
=#


function maxfun(lis,f,m)
    A=copy(lis)
    if length(lis)==0
        m
    else
        b=pop!(A)
        C=copy(m)
        d=pop!(C)
        fd=f(d)
        fb=f(b)
        if fd>fb
            maxfun(A,f,m)
        elseif fd==fb
            maxfun(A,f,append!([b],copy(m)))
        else
            maxfun(A,f,[b])
        end
    end # if
end # function


maxfun(lis,f)=isempty(lis) ? [] : sinrep(maxfun(lis,f,[first(lis)]))

maxfun([-5,-4,-3,-2,-1,-0-1,2,3,4,5],x->x*x)


function provec(v1,v2)
    b1=copy(v1)
    b2=copy(v2)
    if (isempty(v1) || isempty(v2))
        0
    else
        pop!(b1)*pop!(b2)+provec(b1,b2)
    end
end # function

#   provec([1,2,3,4,5],[1,2,3,4,5])


function mejorjcad2(ami,ene,vec,n,proh)
    jcad1=juegacadena(ami,ene,n)
    enevc=map((cad->sinrep(quita(veccad(cad,n),union(ami,ene)))),confcad(ene,n))
    maxfun(quita(tablero(n),union(ami,ene,proh)),pos-> provec(vec,jugadacad(ami,ene,pos,n,enevc,jcad1)))
end  #function


# println(mejorjcad2([(1,3),(1,5),(2,4),(3,3),(3,5)],[(2,3),(3,4),(4,3),(4,4),(4,5)],[1,2,7,-1],9,[]))

mejorjuegocad2(ami,ene,vec,n,proh)=rand(mejorjcad2(ami,ene,vec,n,proh))

#=
println(mejorjuegocad2([(1,3),(1,5),(2,4),(3,3),(3,5)],[(2,3),(3,4),(4,3),(4,4),(4,5)],[1,2,7,-1],9,[]))

println(mejorjuegocad2([(2,3),(3,4),(4,3),(4,4),(4,5)],[(1,3),(1,5),(2,4),(3,3),(3,5)],[1,2,7,6],9,[]))
=#

# jugadacad

#print(partida([(0,0),(4,4),(3,3),(2,2),(1,1),(1,2),(5,5),(2,1)],9))


mejornegraauxcad2(parti,vec,n,proh)=mejorjuegocad2(first(parti),parti[2],vec,n,proh)
jueganegracad2(jug,vec,n,proh)=mejornegraauxcad2(partida(jug,n),vec,n,proh)
mejorblancaauxcad2(parti,vec,n,proh)=mejorjuegocad2(parti[2],parti[1],vec,n,proh)
juegablancacad2(jug,vec,n,proh)=mejorblancaauxcad2(partida(jug,n),vec,n,proh)

#=
ejemplopartida=[(5,5),(6,5),(6,4),(5,6),(4,4),(7,4),(6,6),(3,4),(7,5),(8,5),(5,3),(5,2),(4,6),(3,6),(5,7),(5,8),(3,5),(2,5),(4,5),(8,2),(8,4),(2,7),(1,6),(8,8),(7,3),(3,2),(2,6),(7,7),(2,4),(2,3),(1,5),(3,8),(4,7),(4,3),(4,1),(9,4),(3,7),(7,9),(3,3),(1,8),(2,9),(6,1),(6,8),(1,2),(2,1),(9,6),(4,2),(4,9),(1,7),(7,2),(2,8),(6,2),(2,2),(8,3),(4,8),(6,3),(5,9),(5,1),(3,9),(9,2),(7,4),(9,3),(1,9),(9,5),(8,6),(9,7),(1,3),(8,7),(1,3)]

juegablancacad2(ejemplopartida,[3,4,4.5,7],9,[])
=#

suicidio(ami,ene,pos,n)=(isempty(libertadp(union([pos],ami),ene,pos,n)) && isempty(mata(ami,ene,pos,n)))

#suicidio([(3,1),(2,2)],[(1,2),(2,1)],(1,1),9)

function pasa(ami,ene,pos,vec,n)
    jcad1=juegacadena(ami,ene,n)
    (provec(vec,jugadacad(ami,ene,pos,n,map(first,jcad1),jcad1))<0 ? (0,0) : pos)
end # function

#pasa([(1,2),(8,9)],[(2,3),(6,8)],(9,9),[1,0.5,9,11],9)

function listasluegode(lis,e)
    k=findfirst(x->x==e,lis)
    map(m->lis[1:m],k:(length(lis)))
end # function


#print(listasluegode([1,2,4,5,6,4,3],2))

#revisar findmax

function ko(jug,pos,n)
    if !(taaqui(jug,pos))
        false
    else
        #B=copy(A)
        #li=pop!(B)
        #partida(li,n)==partida(union(jug,[pos]),n) ? true : ko(jug,pos,n,B)
        taaqui(map(lisj->partida(lisj,n),listasluegode(jug,pos)),partida(union(jug,[pos]),n))
    end # if
end # function

#ko(jug,pos,n)=ko(jug,pos,n,listasluegode(jug,pos))

#=
@time ko([(1,1),(4,1),(2,2),(3,2),(3,1),(2,1)],(3,1),9)

ejem=[1,2,3,4,5,6,7,8,9]
=#

function yajuegablanca(jug,vec,n,proh)
    posi=juegablancacad2(jug,vec,n,proh)
    tab=partida(jug,n)
    if pasa(tab[2],tab[1],posi,vec,n)==(0,0)
        (0,0)
    elseif suicidio(tab[2],tab[1],posi,n)
        yajuegablanca(jug,vec,n,union(proh,[posi]))
    elseif ko(jug,posi,n)
        yajuegablanca(jug,vec,n,union(proh,[posi]))
    else
        posi
    end # if
end # function

function yajueganegra(jug,vec,n,proh)
    posi=jueganegracad2(jug,vec,n,proh)
    tab=partida(jug,n)
    if pasa(tab[1],tab[2],posi,vec,n)==(0,0)
        (0,0)
    elseif suicidio(tab[1],tab[2],posi,n)
        yajueganegra(jug,vec,n,union(proh,[posi]))
    elseif ko(jug,posi,n)
        yajueganegra(jug,vec,n,union(proh,[posi]))
    else
        posi
    end # if
end # function

#=
yajuegablanca([(0,0),(1,2),(2,3),(8,9),(6,8)],[1,0.5,8,11],9,[])

yajueganegra([(1,1),(4,1),(2,2),(3,2),(3,1),(2,1)],[1,0.5,8,11],9,[])
=#

function versus(vecn,vecb,n,m,jug)
    if m==0
        jug
    elseif iseven(length(jug))
        versus(vecn,vecb,n,m,union(jug,[yajueganegra(jug,vecn,n,[])]))
    else
        versus(vecn,vecb,n,m-1,union(jug,[yajuegablanca(jug,vecb,n,[])]))
    end # if
end # function

@time versus(rand(4),rand(4),9,6,[])


#=
print(versus([8,9,1,5],[1,3,5,9],9,10,[]))

juegablancacad2([(1,2),(1,1),(2,1)],[1,0.5,9,11],9,[])


print(versus([1,0.5,9,11],[5,6,8,11],9,10,[(1,2),(2,3),(8,9),(6,8)]))

pareje9=[(5,5),(3,5),(7,5),(4,7),(3,4),(2,4),(3,3),(4,5),(5,4),(2,3),(5,7),(5,8),(6,7),(4,4),(4,3),(5,3)]

print(partida([(5,5),(3,5),(7,5),(4,7),(3,4),(2,4),(3,3),(4,5),(5,4),(2,3),(5,7),(5,8),(6,7),(4,4),(4,3),(5,3)],9))

print(yajueganegra(pareje9,[1,2,15,20],9,[]))

print(versus([1,0.5,9,11],[5,6,8,11],9,5,[(5, 5), (3, 5), (7, 5), (4, 7), (3, 4), (2, 4), (3, 3), (4, 5), (5, 4), (2, 3), (5, 7), (5, 8), (6, 7), (4, 4), (4, 3), (5, 3), (5, 6), (2, 5), (6, 5), (4, 6), (6, 4), (4, 8), (8, 5), (3, 7), (7, 6), (2, 6)]))

print(versus([1,0.5,9,11],[5,6,8,11],9,5,[(5, 5), (3, 5), (7, 5), (4, 7), (3, 4), (2, 4), (3, 3), (4, 5), (5, 4), (2, 3), (5, 7), (5, 8), (6, 7), (4, 4), (4, 3), (5, 3), (5, 6), (2, 5), (6, 5), (4, 6), (6, 4), (4, 8), (8, 5), (3, 7), (7, 6), (2, 6), (9, 5), (1, 4), (7, 4), (3, 6), (6, 6), (2, 2), (9, 6), (1, 2), (7, 3), (1, 1)]))

print(versus([1,0.5,9,11],[5,6,8,11],9,5,[(5, 5), (3, 5), (7, 5), (4, 7), (3, 4), (2, 4), (3, 3), (4, 5), (5, 4), (2, 3), (5, 7), (5, 8), (6, 7), (4, 4), (4, 3), (5, 3), (5, 6), (2, 5), (6, 5), (4, 6), (6, 4), (4, 8), (8, 5), (3, 7), (7, 6), (2, 6), (9, 5), (1, 4), (7, 4), (3, 6), (6, 6), (2, 2), (9, 6), (1, 2), (7, 3), (1, 1), (9, 4), (6, 8), (9, 3), (1, 3), (9, 2), (3, 8), (7, 2), (2, 7), (8, 2), (3, 9)]))

print(versus([1,0.5,9,11],[1,0.5,9,11],9,5,[(5, 5), (3, 5), (7, 5), (4, 7), (3, 4), (2, 4), (3, 3), (4, 5), (5, 4), (2, 3), (5, 7), (5, 8), (6, 7), (4, 4), (4, 3), (5, 3), (5, 6), (2, 5), (6, 5), (4, 6), (6, 4), (4, 8), (8, 5), (3, 7), (7, 6), (2, 6), (9, 5), (1, 4), (7, 4), (3, 6), (6, 6), (2, 2), (9, 6), (1, 2), (7, 3), (1, 1), (9, 4), (6, 8), (9, 3), (1, 3), (9, 2), (3, 8), (7, 2), (2, 7), (8, 2), (3, 9), (8, 4), (7, 8), (8, 1), (1, 7), (6, 3), (5, 9), (6, 2), (3, 2), (5, 2), (7, 9)]))
=#


function parejas(lis)
    A=copy(lis)
    length(lis)<2 ? [] : union([[popfirst!(A),popfirst!(A)]],parejas(A))
end # function

#print(parejas([1,2,3,4,5,6,7,8,9,10]))

function cruce(v1,v2,v3)
    x=(v3[2]*v2[3])/(v1[2]*v3[3])
    [x*v1[1],x*v1[2],v2[3],v2[4]]
end # function


#cruce([1,0.5,9,11],[2,3,4,5],[1,1,2,2])


function valorblanca(vecb,vecn,n,m,jug)
    tabfin=partida(versus(vecn,vecb,n,m,jug),n)
    tabini=partida(jug,n)
    punfin=evaluador(tabfin[2],tabfin[1],n)
    punini=evaluador(tabini[2],tabini[1],n)
    punfin[1]-punini[1]
end

function valornegra(vecb,vecn,n,m,jug)
    tabfin=partida(versus(vecn,vecb,n,m,jug),n)
    tabini=partida(jug,n)
    punfin=evaluador(tabfin[2],tabfin[1],n)
    punini=evaluador(tabini[2],tabini[1],n)
    punfin[2]-punini[2]
end

@time 1+1

valornegra(rand(4),rand(4),9,5,[])

pruebablanca(vecb,lisv,n,m,jug)=suma(map((vec->valorblanca(vecb,vec,n,m,jug)),lisv))
pruebanegra(vecn,lisv,n,m,jug)=suma(map((vec->valornegra(vecn,vec,n,m,jug)),lisv))

function torneoblanco1(lisb,lisv,n,m,jug)
    pru=map((vec->pruebablanca(vec,lisv,n,m,jug)),lisb)
    #sumas= (findmax([pru])[1])<1 ? findmax(pru)[1]+1 : 0
    function ff(k)
        fit=pru[k]
        fit<1 ? [] : map(x->lisb[k],1:fit)
    end # function
    miappend(map(ff,1:(length(lisb))))
end # function

function torneonegro1(lisn,lisv,n,m,jug)
    pru=map((vec->pruebanegra(vec,lisv,n,m,jug)),lisn)
    #sumas= (findmax([pru])[1])<1 ? findmax(pru)[1]+1 : 0
    function ff(k)
        fit=pru[k]
        fit<1 ? [] : map(x->lisn[k],1:fit)
    end # function
    miappend(map(ff,1:(length(lisn))))
end # function

#@time print(torneonegro1([[1,2,3,4],[2,3,4,5],[3,4,5,6],[6,5,4,3]],[[4,3,2,1],[5,4,3,2],[6,5,4,3],[3,4,5,6]],9,2,[]))

#=
(findmax([1,2,3,5])[1])

miunion([[1],[2,7],[1,2,3,4]])

append!([1,2,3],[3,4,5])
=#
#    a=1,b=2,c=3,d=4,e=5,f=6,g=7,h=8,i=9

ej1=[(5,6),(5,4),(5,2),(5,8),(6,8),(6,7),(5,7),(7,8),(4,8),(6,9),(3,7),(3,4),(7,3),(2,6),(2,7),(3,2),(6,6),(7,6),(6,5),(3,6),(7,5),(8,6),(8,5),(4,6),(4,7),(6,3),(6,2),(5,5),(4,3),(3,3),(4,4),(4,5),(4,2),(4,9),(3,9),(5,9),(1,6),(1,5),(1,7),(2,5),(3,1),(4,1),(5,1),(2,1),(4,1),(2,9),(3,8),(9,5),(2,2),(7,4),(2,3),(2,4),(6,4),(5,3),(8,2),(8,4),(8,3),(9,4),(1,3),(1,8),(1,1),(9,3),(1,4),(2,8)]

function geneauxblanco1(pob,n,m,g,jug)
    print(pob)
    if g==0
        pob
    else
        pun1=torneoblanco1(pob,map(x->rand(4),1:4),n,m,jug)
        #pun=miappend([pun1,pun1,pun1])
        A=map(x->cruce(rand(pun1),rand(pun1),rand(pun1)),1:(length(pob)))
        #print(A)  #Al Activar esto se imprime cda generacion de vectores
        geneauxblanco1(A,n,m,g-1,jug)
    end # if
end # function

geneblanco(n,m,g,jug,pob)=geneauxblanco1(map(x->rand(4),1:pob),n,m,g,jug)

function geneauxnegro1(pob,n,m,g,jug)
    if g==0
        pob
    else
        pun1=torneonegro1(pob,map(x->rand(4),1:4),n,m,jug)
        #pun=miappend([pun1,pun1,pun1])
        A=map(x->cruce(rand(pun1),rand(pun1),rand(pun1)),1:(length(pob)))
        #print(A)  #Al Activar esto se imprime cda generacion de vectores
        geneauxnegro1(A,n,m,g-1,jug)
    end # if
end # function

genenegro(n,m,g,jug,pob)=geneauxnegro1(map(x->rand(4),1:pob),n,m,g,jug)

#shuffle([1,2,3,4])

#=
@time print(geneauxblanco1([[1,2,3,4],[2,3,4,5],[3,4,5,6],[6,5,4,3]],9,2,3,[]))

print(rand([1,2,3,4],2))
=#


function tornvecbl(lisb,n,jug)
    lisn=map(x->rand(4),1:5)
    maxfun(lisb,vec->pruebablanca(vec,lisn,n,4,jug))
end # function

function tornvecne(lisb,n,jug)
    lisn=map(x->rand(4),1:5)
    maxfun(lisb,vec->pruebanegra(vec,lisn,n,4,jug))
end # function


function juegagenbl(n,m,g,jug,pob)
    vecs=tornvecbl((geneblanco(n,m,g,jug,pob)),9,jug)
    isempty(vecs) ? (0,0) : yajuegablanca(jug,rand(vecs),9,[])
end # function


borde(n)=union(map(x->(1,x),1:n),map(x->(n,x),1:n),map(x->(x,1),1:n),map(x->(x,n),1:n))

print(borde(20))

function juegagenne(n,m,g,jug,pob)
    vec=tornvecne(genenegro(n,m,g,jug,pob),9,jug)
    isempty(jug) ? yajueganegra(jug,rand(vec),n,borde(n)) : yajueganegra(jug,rand(vec),n,[])
end


#=
@time juegagenne(9,4,4,[],4)
@time juegagenne(9,4,4,[],3)
@time juegagenne(9,4,3,[],4)
@time juegagenne(9,3,4,[],4)

juegagenbl(9,4,4,[(6,6)],4)
=#
@time 1+1



function jueganegraprom(jug,n,m)
    if isempty(jug)
        prpr=borde(n)
    else
        prpr=[]
    end
    A=map(x->rand(4),1:m)
    B=[]
    for v in A
        push!(B,yajueganegra(jug,v,n,prpr))
    end # for
    #print(B)
    C=sinrep(B)
    rand(maxfun(C,x->count(y->y==x,B)))
end

function juegablancaprom(jug,n,m)
    A=map(x->rand(4),1:m)
    B=[]
    for v in A
        push!(B,yajuegablanca(jug,v,n,[]))
    end # for
    #print(B)
    C=sinrep(B)
    rand(maxfun(C,x->count(y->y==x,B)))
end


function torneoprom(jug,n,m,s)
    jugaux=copy(jug)
    if s==0
        jug
    else
        if iseven(length(jug))
            neg=jueganegraprom(jug,n,m)
            print(neg)
            torneoprom(push!(jugaux,neg),n,m,s-1)
        else
            bla=juegablancaprom(jug,n,m)
            print(bla)
            torneoprom(push!(jugaux,bla),n,m,s-1)
        end # if
    end # if
end # function

#El torneo fin, corre hasta que ambos pasen, es decir hasta el final de la partida
function torneopromfin(jug,n,m)
    jugaux=copy(jug)
    if (length(jug) < 2 ? false : ( last(jug)==(0,0) && jug[length(jug)-1]==(0,0) ) || length(sinrep(jug))>n*n-1 )
        jug
    else
        if iseven(length(jug))
            neg=jueganegraprom(jug,n,m)
            print(neg)
            torneopromfin(push!(jugaux,neg),n,m)
        else
            bla=juegablancaprom(jug,n,m)
            print(bla)
            torneopromfin(push!(jugaux,bla),n,m)
        end # if
    end
end # function

#torneopromfin([],4,4)


function jueganegraneg(jug,n,m)
    if isempty(jug)
        prpr=borde(n)
    else
        prpr=[]
    end
    A=map(x->map(x->x*1.25-0.25,rand(4))+[0,0.25,0,0],1:m)
    #print(A)
    B=[]
    for v in A
        push!(B,yajueganegra(jug,v,n,prpr))
    end # for
    #print(B)
    if count(y->y==(0,0),B)>(length(B)/2)
        (0,0)
    else
        C=sinrep(quita(B,[(0,0)]))
        rand(maxfun(C,x->count(y->y==x,B)))
    end
end

#jueganegraneg([],9,6)


function juegablancaneg(jug,n,m)
    A=map(x->map(x->x*1.2-0.2,rand(4))+[0,0.25,0,0],1:m)
    B=[]
    for v in A
        push!(B,yajuegablanca(jug,v,n,[]))
    end # for
    #print(B)
    if count(y->y==(0,0),B)>(length(B)/2)
        (0,0)
    else
        C=sinrep(quita(B,[(0,0)]))
        rand(maxfun(C,x->count(y->y==x,B)))
    end
end


function torneoneg(jug,n,m,s)
    jugaux=copy(jug)
    if s==0
        jug
    else
        if iseven(length(jug))
            neg=jueganegraneg(jug,n,m)
            print(neg) #Imprime jugada mientras la calcula
            torneoprom(push!(jugaux,neg),n,m,s-1)
        else
            bla=juegablancaneg(jug,n,m)
            print(bla) #Imprime jugada mientras la calcula
            torneoprom(push!(jugaux,bla),n,m,s-1)
        end # if
    end # if
end # function

function torneonegfin(jug,n,m)
    jugaux=copy(jug)
    if (length(jug) < 2 ? false : ( last(jug)==(0,0) && jug[length(jug)-1]==(0,0) ) || length(sinrep(jug))>n*n-2 )
        jug
    else
        if iseven(length(jug))
            neg=jueganegraneg(jug,n,m)
            print(neg)
            torneonegfin(push!(jugaux,neg),n,m)
        else
            bla=juegablancaneg(jug,n,m)
            print(bla)
            torneonegfin(push!(jugaux,bla),n,m)
        end # if
    end
end # function

#torneo con negras jugando como neg y blancas con prom
function tornnnegbprom(jug,n,m,s)
    jugaux=copy(jug)
    if s==0
        jug
    else
        if iseven(length(jug))
            neg=jueganegraneg(jug,n,m)
            print(neg) #Imprime jugada mientras la calcula
            tornnnegbprom(push!(jugaux,neg),n,m,s-1)
        else
            bla=juegablancaprom(jug,n,m)
            print(bla) #Imprime jugada mientras la calcula
            tornnnegbprom(push!(jugaux,bla),n,m,s-1)
        end # if
    end # if
end # function


#torneo con negras jugando como prom y blancas con neg
function tornnprombneg(jug,n,m,s)
    jugaux=copy(jug)
    if s==0
        jug
    else
        if iseven(length(jug))
            neg=jueganegraprom(jug,n,m)
            print(neg) #Imprime jugada mientras la calcula
            tornnprombneg(push!(jugaux,neg),n,m,s-1)
        else
            bla=juegablancaneg(jug,n,m)
            print(bla) #Imprime jugada mientras la calcula
            tornnprombneg(push!(jugaux,bla),n,m,s-1)
        end # if
    end # if
end # function


function tnnegbpromfin(jug,n,m)
    jugaux=copy(jug)
    if (length(jug) < 2 ? false : ( last(jug)==(0,0) && jug[length(jug)-1]==(0,0) ) || length(sinrep(jug))>n*n-2 )
        jug
    else
        if iseven(length(jug))
            neg=jueganegraneg(jug,n,m)
            print(neg)
            tnnegbpromfin(push!(jugaux,neg),n,m)
        else
            bla=juegablancaprom(jug,n,m)
            print(bla)
            tnnegbpromfin(push!(jugaux,bla),n,m)
        end # if
    end
end # function

function tnprombneg(jug,n,m)
    jugaux=copy(jug)
    if (length(jug) < 2 ? false : ( last(jug)==(0,0) && jug[length(jug)-1]==(0,0) ) || length(sinrep(jug))>n*n-2 )
        jug
    else
        if iseven(length(jug))
            neg=jueganegraprom(jug,n,m)
            print(neg)
            tnprombneg(push!(jugaux,neg),n,m)
        else
            bla=juegablancaneg(jug,n,m)
            print(bla)
            tnprombneg(push!(jugaux,bla),n,m)
        end # if
    end
end # function

#=

torneonegfin([],9,6)
partidagenerada1=[(7, 7),(3, 6),(6, 7),(3, 5),(6, 8),(2, 6),(5, 7),(1, 6),(5, 8),(3, 4),(5, 9),(0, 0),(4, 8),(3, 7),(6, 9),(4, 5),(4, 7),(5, 5),(5, 6),(2, 4),(3, 8),(4, 6),(7, 9),(1, 5),(7, 6),(2, 7),(8, 7),(5, 4),(7, 5),(3, 3),(7, 8),(2, 8),(0, 0),(4, 4),(7, 4),(1, 4),(3, 9),(1, 3),(9, 7),(2, 3),(8, 4),(0, 0),(9, 8),(3, 2),(8, 5),(1, 7),(8, 6),(0, 0),(0, 0)]


last([1,4,6,8,3,6,78,9,4])
ej1[length(ej1)]


# Primera entrada lista de jugadas de la partida, segunda: tamaño del tablero
# tercera: tamaño de la muestra, cuarta numero de jugadas
torneoprom([],9,6,8)


@time jueganegraprom([(5,2),(5,6),(6,2),(4,6)],9,10)
@time jueganegraneg([(5,2),(5,6),(6,2),(4,6)],9,10)
@time juegablancaprom([(5,2),(5,6),(6,2)],9,10)

@time jueganegraprom([(1,9),(6,9),(1,8),(6,8),(2,9),(3,9)],9,10)
@time juegablancaprom([(1,9),(6,9),(1,8)],9,10)
@time juegablancaneg([(1,9),(6,9),(1,8)],9,10)

@time jueganegraprom(push!(copy(ej1),(2,1),(6,4),(7,2),(2,7),(6,1),(3,9)),9,6)
@time juegablancaprom(push!(copy(ej1),(2,1),(6,4),(7,2),(2,7),(6,1),(3,9),(8,1)),9,6)

jueganegraneg([],4,6)

jueganegraneg(ej1,9,10)
jueganegraprom(ej1,9,6)

maxfun([1,2,3,4,5,6],x->x)

count(i-> i==2,[1,0,4,3,6,7,9,2,3,0,5,4,7,10,79])

map(x->x*1.1-0.1,rand(4)) =#

print(torneonegfin([],4,6))

print("terminado")
