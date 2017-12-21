--Práctica Programación declarativa
--Autor: Jaime Dan Porras Rhee
--Cómo se descifró el texto: mediante la función calculaFrecuencias se calcularon las frecuencias de aparición de cada letra en el texto codificado. Poniéndolas en orden de mayor a menor frecuencia mediante ordenaFrecuencias y uniéndolas a la lista ordenada de las frecuencias del español se consiguió una primera aproximación de cómo debían de sustituirse las letras. Este mapeo conseguido se aplica sobre el texto cifrado obteniendo un nuevo texto que debería ser por lo menos más legible (mediante transformaString). Como las frecuencias en español no siempre mantienen el orden en cualquier texto ( por ejemplo, en ocasiones la 'a' es la más repetida antes que la 'e', y otros casos) se decidió coger el quijote, tomar letras de 1000 en 1000, calcular las frecuencias de esos grupos de 1000 letras, compararlos con las frecuencias del texto codificado, y aplicar la transformación sobre el texto cifrado (prueba25). Esto dio como resultado muchos textos en el que alguno tenía más sentido que otro. En alguno de ellos solo era necesario sustituir unas pocas letras, y sustituyéndolas se consiguió la codificación final (codificacionMasAprox) con la que el texto cifrado ya cobraba sentido. Este método como posible automatización era muy lento y se abandonó. 
--Como intento para automatizar se hicieron algunas funciones, como wordToNum, encaja, encajesufijo,... utilizados en la prueba252, pero se abandonó por ser muchas las posibilidades que había que explorar  
--Para descifrar el texto con la ayuda del usuario, se crearon appPrincipal y comparacion, funciones desde las que podemos hacer una primera aproximacion mediante el analisis de frecuencias y posteriormente comparar los textos transformado y codificado y elegir qué otras posibles sustituciones de letras podemos aplicar, aplicarlas para ver el resultado, seguir comparando si aún no tenemos claro qué sustitución aplicar, etc. El menu de comparacion se repite hasta que indiquemos que queremos empezar de 0, o aceptar la codificacion que hemos usado, o salir del bucle (recursivo).
--(Deben de estar texto.cod.txt y Quijote.txt junto a este archivo)

import Data.Char
import Data.List.Split
import Data.List

-- ALgunas funciones útiles para la práctica

--mapea recibe la lista de caracteres del texto codificado con 
--sus frecuencias, la lista de caracteres del español (u otro texto  
-- del español sin codificar) con sus frecuencias
--ambas ordenadas por frecuencia de mayor a menor, y agrupa los caracteres de
--ambas listas que estén en la misma posición
--(Ataque a ciegas)
mapea:: [(Char,Int)] -> [(Char,Int)] ->[(Char,Char)]
mapea (c:cs) (n:ns) = (fst c, fst n):mapea cs ns
mapea [] _ = []
mapea _ [] = []
--revisar cómo poner eso último bien


--transformaChar recibe una lista [(Char,Char)] (codificacion)
--y sustituye el char
--Se supone que en lista está la codificación (izq codificado, der descod.)
--sustituye el char a través de una función auxiliar
--lo que distingue a esta funcion es que clasifica los char 
--segun sean mayusculas, minusculas u otros caracteres
--la auxiliar es la que hace la verdadera conversión
transformaChar:: [(Char,Char)] -> Char -> Char
transformaChar lista c 
    | isUpper c = toUpper (transformaCharDirecta lista (toLower c) )
    | isLower c = transformaCharDirecta lista c 
    | otherwise = c
--transformaCharDirecta recibe un char, la lista [(Char,Char)] (codificacion)
--y sustituye el char por snd (char,c), con (char,c) en la lista
--solo recibe char del alfabeto en minuscula
transformaCharDirecta::[(Char,Char)] -> Char -> Char
transformaCharDirecta (prim:resto) c 
    | c == fst prim = snd prim
    | otherwise = transformaCharDirecta resto c 
transformaCharDirecta [] c = c

transforma2 a b = transformaChar a b--he cambiado el otro asi que este ya no es 
--necesario

--prodMapas coge dos mapeos y los fusiona
--no hay conflictos si son disjuntos pero si en dos mapas distintos la misma letra
--  se transforma en una distinta en cada caso, o una misma letra es la transformacion
--  de dos distintas letras hay un conflicto
--en caso de conflicto haremos que el mapa utilizado como primer argumento es el 
--  que prevalezca sobre el otro, en los dos tipos de conflicto
prodMapas::[(Char,Char)] -> [(Char,Char)] -> [(Char,Char)]
prodMapas [] y = y
prodMapas y [] = y
prodMapas x y =
    let igsIz = [a | a<- y, b <- x, (fst a)==(fst b)]
        igsDer = [a | a <- y, b<- x, (snd a) == (snd b)]
        resto = [a | a<-y, not (elem a igsDer), not(elem a igsIz)]
    in x++resto


--addMapa hace lo mismo que la funcion anterior, con la salvedad de que el segundo
--  argumento es la transformacion de un solo caracter
addMapa::[(Char,Char)] -> (Char,Char) -> [(Char,Char)]
addMapa (x:xs) y
    | (fst x) == (fst y) || (snd x) == (snd y) = x:xs
    | otherwise = y:(x:xs)
addMapa [] y = y:[]

--solapase toma dos mapeos y mira si se solapan, es decir, si no son disjuntos
solapase::[(Char,Char)] -> [(Char,Char)] -> Bool
solapase x y = not (null a) || not (null b)
    where a = [u | u<-x, v <- y, (fst u) == (fst v)]
          b = [u | u<-x, v <- y, (snd u) == (snd v)] 

--mapFromString genera un mapeo de caracteres a partir de un string
--ejemplo: ['a','b','c','d'] -> [('a','b'),('c','d')]
--precond: que el string tenga longitud par
mapFromString::[Char]->[(Char,Char)]
mapFromString [] = []
mapFromString z = ((head z),(head (tail z))):(mapFromString (tail (tail z))) --(tail (tail z))--(x,y):(mapFromString ys)

--coge un string, un mapeo de caracteres y lo aplica sobre el string, devolviendo el resultado
transformaString::String -> [(Char,Char)] -> String
transformaString a b = map (transformaChar b) a

--Hay varias funciones para calcular frecuencias 

--calculaFrecuencias
--Recibe un texto y produce como salida una lista con los 
--caracteres y su frecuencia relativa de aparición en el texto.
--calculaFrecuencias::[]
calculaFrecuencias::String->[(Char,Int)]
calculaFrecuencias x = ordenaFrecuencias (calculaFrecAux x [] [])

--calculaFrecAux tiene un parámetro acumulador
calculaFrecAux::String->[(Char,Int)]->[(Char,Int)]->[(Char,Int)]
--calculaFrecAux (x:xs) [] [] es calculaFrecAux xs [] ((x,1):[])
calculaFrecAux (x:xs) [] [] 
    | not(isAlpha x) = calculaFrecAux xs [] []
    | otherwise = calculaFrecAux xs [] ((toLower x,1):[])
--calculaFrecAux (x:xs) z [] = calculaFrecAux xs [] ((x,1):z)
calculaFrecAux (x:xs) z [] 
    | not(isAlpha x) = calculaFrecAux xs z []
    | otherwise = calculaFrecAux xs [] ((x,1):z)
calculaFrecAux (x:xs) z (y:ys)
    | not(isAlpha x) = calculaFrecAux xs z (y:ys)
    | x == (fst y) = calculaFrecAux xs [] ((((toLower x),1+(snd y)):z)++ys) 
    | x /= (fst y) = calculaFrecAux ((toLower x):xs) (y:z) ys
calculaFrecAux [] z y = y

--calculaFrecs
calculaFrecs::String -> [(Char,Int)] ->[(Char,Int)]
calculaFrecs (x:xs) lisz 
    | not(isAlpha x) = calculaFrecs xs lisz
    | null b = calculaFrecs xs ((a,1):lisz)
    | otherwise = calculaFrecs xs ((a,(y+1)):v)
    where a = (toLower x)
          y = (sum (map snd [elementos | elementos <- lisz, (fst elementos) == a]))
          b = [elementos | elementos <- lisz, (fst elementos) == a]
          v = [u | u<-lisz, (fst u) /= a]
calculaFrecs [] z = ordenaFrecuencias z

--calculaFrecuencias2
calculaFrecuencias2::String->[(Char,Int)]
calculaFrecuencias2 z = ordenaFrecuencias
     (calculaFrecuencias2Aux (qsort (concat (separaPalabras z))) [])
--calculaFrecuencias2
calculaFrecuencias2Aux::String->[(Char,Int)]->[(Char,Int)]
calculaFrecuencias2Aux (x:xs) z = 
  calculaFrecuencias2Aux (drop y xs) ((x,1+y):z)
        where y = length(takeWhile (==x) xs)

calculaFrecuencias2Aux [] z = z



--frecuenciaTotal coge lista [(Char,Int)] y devuelve la suma de los int
frecuenciaTotal :: [(Char,Int)] -> Int
--frecuenciaTotal [] es 0
--frecuenciaTotal (x:xs) es(snd x) + frecuenciaTotal xs
frecuenciaTotal x = sum (map snd x)
--calculaFrecRel pasa de la lista [(Char,Int)] de frecuencias absolutas
-- a la de frecuencias relativas.
calculaFrecRel::[(Char,Int)]->[(Char,Float)]
calculaFrecRel lis = 
    map (\(x , y)->(x , (100*(fromIntegral y)/(fromIntegral z)) )) lis
        where z = frecuenciaTotal lis
    

--ordenaFrecuencias coge una lista [(Char,Int)]
--y la ordena de mayor a menor frecuencia
ordenaFrecuencias::[(Char,Int)]->[(Char,Int)]
ordenaFrecuencias [] = []
ordenaFrecuencias (x:xs) = ordenaFrecuencias [u | u <- xs, (snd u) >= (snd x)]
                           ++ [x] ++
                           ordenaFrecuencias [u | u <- xs, (snd u) < (snd x)]





--separaPalabras separa las palabras de un string, y las pasa a minúsculas
separaPalabras::String -> [String]
separaPalabras = (map (map toLower.(takeWhile isAlpha))).(splitWhen (not.isAlpha))

--separaPalabras2 separa las palabras de un string, y las pasa a minúsculas
--mejora separaPalabras ya que ya no es necesario hacer el takeWhile
--antes se hacía porque en vez de splitWhen se usaba words
separaPalabras2::String -> [String]
separaPalabras2 = 
    (filter (/= [])).(map (map toLower)).(splitWhen (not.isAlpha))


--Pone las palabras por longitud
--Recibe una lista de strings y elabora una lista con el formato
--[(i,j), tal que i es la longitud, y j el conjunto de palabras con longitud i,
--          e i va desde 1 hasta la maxima longitud que haya en z]
--lo mejor es darle como entrada separaPalabras2
palsPorLong::[String]->[(Int,[String])]
palsPorLong z = 
    [(i,(palsDeLong v i))| i <- [1..maximo] ]
    where maximo = maximum (map length z)
          v = strictQsort z

--coge una lista de strings y un entero, y coge solamente las palabras que tenga como longitud dicho entero
palsDeLong::[String]->Int->[String]
palsDeLong z n = 
    [pals | pals<- z , length(pals) == n]

--Calcula la frecuencia de aparicion de palabras desde una lista de Strings
frecPalabras::[String]->[(String,Int)]
frecPalabras (x:xs) =
    (x,length y):(frecPalabras v)
    where y = [pal | pal <- (x:xs), pal == x]
          v = [pal | pal <- xs, pal /= x]
frecPalabras [] = []



--letrasS coge una palabra y devuelve las letras que contiene esa palabra 
--  sin repetición, y en el orden en que aparecen
-- Hay que llamarla con [] en el segundo argumento, ya que ahí es donde
--  se van acumulando las letras de la palabra
letrasS::[Char] -> [Char] -> [Char]
letrasS (x:xs) z
    | not(elem x z) = letrasS xs (z++[x])
    | otherwise = letrasS xs z
letrasS [] z = z

--wordToNum pasa de una palabra a un numero de la siguiente manera:
--  a la primera letra x le asigna el 1, cualquier aparición de x se pasa a uno
--  la siguiente letra en aparecer distinta de x se sustituirá por 2, y así  etc.
wordToNum::[Char] -> [Int]
wordToNum (x:xs) =
    map cambiar (x:xs)
    where lets = letrasS (x:xs) []
          mapeo = zip lets [1..(length (x:xs))] 
          pareja z = [(w,u)|(w,u) <- mapeo, w == z]
          cambiar car = snd ((pareja car)!!0)

--encaje mira cuánto de una palabra encaja en la otra
--en principio para palabras de la misma longitud (aunque con el zipWith funciona de cualquier forma)
encaje::[Char]->[Char]->[Bool]
encaje = zipWith (==)

--encajeSufijo intenta encajar una palabra como sufijo de la otra
--el segundo argumento es la palabra y el primero el sufijo que se desea 
--      encajar en la palabra
encajeSufijo::[Char]->[Char]->[Bool]
encajeSufijo suf pal = zipWith (==) suf (drop ((length pal) - (length suf)) pal)



--esta función simplemente cuenta cuántas letras hay en total en una lista de
--  Strings.
cuentaLetras::[String]->Int
cuentaLetras z = sum.(map length) $ z

--qsort::[String] -> [String]
qsort [] = [] 
qsort (x:l) =
    qsort [y | y<-l, y<=x] ++ [x] ++
    qsort [y | y<-l, y>x]
--strict Qsort no coge elementos repetidos
strictQsort [] = []
strictQsort (x:l) =
    strictQsort [y | y<-l, y<x] ++ [x] ++
    strictQsort [y | y<-l, y>x]

--leeArchivo lee el archivo, calcula sus frecuencias, lo imprime y lo pone en un string
leeArchivo::FilePath -> IO String
leeArchivo fileName = 
    do 
        s <- readFile fileName
        --let v = filter (!= '\241') s
        let x = calculaFrecuencias s
        print x
        return s



 --   queremos conseguir [a,[(b,numero)]] donde a es el caracter cod y la lista de la derecha muestra las posibles conversiones para a y el número de ocurrencias de esta posibilidad
    --filtramos los que tengan fst a, y cogemos los snd, concat y calcula frecuencias jajaja
pruebanueva::[(Char,Char)]->[(Char,[(Char,Int)])]
pruebanueva x = [(a, (funcionNueva a x)) | a <- ['a'..'z']]
    --where f a = calculaFrecs (concat (map snd ([ (a,b) | (a,b)<-x]) )) []
funcionNueva::Char->[(Char,Char)]->[(Char,Int)]
funcionNueva a x = calculaFrecs  [ y | (a,y)<- x ] []
            

--getInt para conseguir leer un enterio de la entrada
getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int)

--Esta funcion es el menu para descidificar manualmente
-- se puede: elegir qué descodificación queremos aplicar (puede ser parcial)
--           comparar texto codificado con original para elegir qué letras creemos que están bien
--           aplicar la codificacion que llevamos
--           empezar desde el principio (solamente texto original y primera aproximcacion aplicando directamente la sustitucion por frecuencias)
--           darse por satisfecho, si creemos que el texto ya esta bien descifrado (se imprimirá la descodificación aplicada)
--           irse de la aplicacion
comparacion:: String -> [(Char,Char)] -> String -> IO ()
comparacion [] _ _= return ()
comparacion x y tr = 
    do
      --hay que pasar también el texto original, al seleccionar 2 aplicarle lo que llevamos y usar ese como el nuevo originial
        putStrLn "¿Que deseas hacer? \n 0.Continuar descodificando \n 1.Continuar leyendo \n 2. Aplicar descodificacion \n 3. Borron y cuenta nueva \n 4. Estoy satisfecho con la descodificacion \n 5. Quiero irme a casa (cerrar)"
        n <- getInt
        --introducir por teclado los nuevos caracteres que deben de codificarse
        -- leer los siguientes (sin aplicar la descodificacion) y repetir lo anterior
        if n==0 then do putStrLn "Introduce: 'caracter codificado'+ espacio + 'descodificacion del caracter' + espacio + repetir "
                        codi <- getLine 
                        let --inLinea
                           sinEspacios = filter (/= ' ') codi
                           newMap = mapFromString sinEspacios
                           fuseMap = prodMapas newMap y
                        comparacion x fuseMap tr
                     --obtener la nueva codificacion (mapeo) y unirla
            --leer los siguientes caracteres y repetir las acciones
            else if n==1 then 
                    do
                     let remain = drop 15 x
                         next = take 15 x
                         remain2 = drop 15 tr
                         next2 = take 15 tr
                     putStrLn next
                     putStrLn next2
                     comparacion remain y remain2--este está resultando un poco confuso ya que solo tendría sentido en la primera iteracion
                --a partir de aqui solo tendria sentido leer los siguientes chars y ver qué codificaciones hacer desde ahi
            --comparacion x y z x' y' z'
                --aplicar la descodificacion a todo el texto y mostrar las lineas desde 
                --  el principio
                else if n==2 then do putStrLn (transformaString x y)
                                     comparacion x y (transformaString x y)
                    --comenzar de 0 con el texto codificado
                    else if n==3 then appPrincipal--restart
                        else if n==4 then imprimeLista y  
                            else putStrLn "Ok, hasta luego"


        
--imprimeLista imprime cada elemento de una lista en una nueva linea
imprimeLista:: Show a => [a] -> IO ()
imprimeLista (x:xs) = 
    do
        print x
        >>
        imprimeLista xs
imprimeLista [] = 
    return ()

--Esta es la funcion que ha de iniciarse si queremos aplicar la descodificacion manual
--Se hará el análisis de frecuencias, y posteriormente se irá a la función comparación, desde donde se compararán textos original y transformado y se podrá ir eligiendo qué transformación queremos aplicar
appPrincipal = 
    do
        a <- leeArchivo "texto.cod.txt"
        let y = mapea (calculaFrecuencias a) frecSpanish
            tr = transformaString a y 
        putStrLn tr
        putStrLn "Codificacion utilizada: "
        imprimeLista y
        
        let v = take 15 a
            w = take 15 tr
        putStrLn v
        putStrLn w
        putStrLn "¿Que letras crees que estan bien descodificadas?"
        s <- getLine
        let j = [ elems | elems <- y, elem (fst elems) s]
        comparacion a j tr




--CodificacionMasAprox es la codificacion para descifrar texto.cod.txt
--se ha hallado utilizando funciones del programa
codificacionMasAprox = [('o','a'),('q','t'),
    ('v','o'),('l','d'),('k','e'),('y','l'),('x','m'),('p','u'),
    ('w','n'),('i','g'),('r','s'),('u','p'),('s','r'),('g','i'),('e','v'),('m','c'),
    ('f','j'),('b','y'),('t','q'),('n','b'),('c','x'),('j','f'),('a','z')]
        --hasta aqui supongo




---A partir de aquí solo hay funciones de prueba
hazIntentos a y = 
    do
        let trans = transformaString a y
        return trans



prueba25 = 
    do
        a <- leeArchivo "Quijote.txt"
        b <- leeArchivo "texto.cod.txt"
        let wor = separaPalabras2 b
            fuente = palsPorLong.separaPalabras2 $ (take 1000 a)
            --salida = [ (prueba251 (wordToNum x) x 
            --    (map wordToNum (snd ((palsPorLong.separaPalabras2 $ a)!!(length x-1 )))) )|
            --     x <- wor ]
        --print salida
        let solucion = [ [(x,y) | y<-(snd ( fuente !!(length x-1) ) ) , (wordToNum y) == (wordToNum x) ] | x <- wor ]
        print solucion


--h en las dos, w en la izq, z en las der, d en la der k en la izq
--a b c d e f g h i j k l m n o p q r s t u v w x y z
        




prueba1 = ordenaFrecuencias (calculaFrecuencias prueba0)

prueba00 = 
    "Yo, Juan Gallo de Andrada, escribano de Camara del Rey nuestro senor, de "++
    " los que residen en su Consejo, certifico y doy fe que, habiendo visto por"++
    " los senores del un libro intitulado El ingenioso hidalgo de la Mancha,"++
    " compuesto por Miguel de Cervantes Saavedra, tasaron cada pliego del dicho"++
    " libro a tres maravedis y medio; el cual tiene ochenta y tres pliegos, que"++
    " al dicho precio monta el dicho libro docientos y noventa maravedis y medio,"++
    " en que se ha de vender en papel; y dieron licencia para que a este precio"++
    " se pueda vender, y mandaron que esta tasa se ponga al principio del dicho"++
    " libro, y no se pueda vender sin ella. Y, para que dello conste, di la"++
    " presente en Valladolid, a veinte dias del mes de deciembre de mil y"++
    " seiscientos y cuatro anos."

prueba0 = "O qvlv ky xpwlv yk iprqo uskrkseos yo usgeomglol lk rpr xkwrofkr lk "++
            "mvsskv, wv eobo o rks tpk rkskr xoygmgvrvr rk kwqkskw lk yv tpk wv "++
            "rk qgkwkw tpk kwqksos b lkrmpnsow yv tpk rgkxusk lkngv tpklos "++
            "rkmskqv b o royev lk xgsolor jpsqgeor. Upkr xkwplo kr yo ikwqk lk "++
            "Xolsgl uoso krqv. Mvqgyyor, tpk rvgr qvlvr pwvr mvqgyyor. Tpk songo "++
            "xk lo. Xkwvr xoy tpk ouoskmkw kw wpkrqsv rvmvssv qkmwgmor xvlkswor "++
            "b xpb kjgmomkr lk kwmsguqomgvw lk qkcqvr. Yor ouygmor b ygrqv, wv "++
            "kr wgwipw ygv. Mgksqv kr tpk wv eoyk homksyv lk mpoytpgks xowkso wg "++
            "o mpoytpgks hvso lky lgo. Yor myoekr wkmkrosgor uoso lkrmgjsos ky "++
            "qkcqv how lk krqos lgruvwgnykr kw mpoytpgks xvxkwqv. Rg uoso "++
            "skmvslosyor eor o pros yo xkxvsgo wv yor kygfor mpowlv krqor "++
            "qksxgwowlv pw nvqkyyvw, uvs kfkxuyv. Rg yor krmsgnkr kw pwo wvqo, "++
            "skegro qp ipobonkso owqkr lk xkqksyo kw yo yoeolvso v kwegosyo oy "++
            "qgwqk. Mvror ukvskr rk how egrqv. Tpk yykeos pwo ipobonkso, xk "++
            "skjgksv. Uksv rg yv homkr mvw mpglolv, pwo eglo lk ivavro gwqgxglol "++
            "qk krukso: kw qpr krmsgqvr uvlsor los sgkwlo rpkyqo o qp uorgvw, "++
            "rvyqos yvr xor onbkmqvr kconspuqvr v xvjosqk lk qpr usvjkrvskr. Wv "++
            "qk uskvmpukr, tpk wolgk lkrmpnsgso qpr ukmolvr b rkmskqgyyvr. Tpk "++
            "roqgrjommgvw, kw jgw."
prueba2 = ['e','a','o','s','r','n','i','d','l','c','t','u','m','p','b',
            'g','v','y','q','h','f','z','j','x','w','k']
frecSpanish = zip prueba2 [25,24..1]
prueba3 = zip ['a','e','o','s','r','n','i','d','l','c','t','u','m','p',
            'b','g','v','y','q','h','f','z','j','x','w','k'] [25,24..1]
prueba4 = map (transformaChar (mapea prueba1 prueba3)) prueba0

prueba5 = map (map toLower) (separaPalabras prueba0)



prueba8 = qsort (concat (separaPalabras prueba0))
prueba88 z = ordenaFrecuencias
     (calculaFrecuencias2Aux (qsort (concat (separaPalabras z))) [])


prueba22 = 
    do
        a <- leeArchivo "Quijote.txt"
        let y = splitEvery 1000 (take 20000 a)
        let u = map calculaFrecuencias y
            u3 = map (map fst) u
            u2 = [(x,(map (elemIndex x) u3))|x<-prueba2]
        print u2
        --let v = map (mapea prueba1) u
        --let z = map (transformaString prueba0) v
        --print z
--prueba221 = 
--    do
--        a <- leeArchivo "Quijote.txt"
--        let y = splitEvery 1000 (take 20000 a)
 --       let u = map calculaFrecuencias y
 --       print u
--        let v = map (mapea prueba1) u
        --let z = map (transformaString prueba0) v
        --print z

prueba24 = 
    do
        a <- leeArchivo "Quijote.txt"
        let y = strictQsort a
            z = palsPorLong.separaPalabras2 $ a
        print y 
        print z

prueba251 x y z = ((filter (x==) z), y)

prueba252 = 
    do
        a <- leeArchivo "Quijote.txt"
        b <- leeArchivo "texto.cod.txt"
        let wor = palsPorLong.separaPalabras2 $ b
            fuente = palsPorLong.separaPalabras2 $ (take 10000 a)
            v = (length wor)-1
            longs = [v, (v-1)..1]
            posibs = [[(x,y)|x<-(snd (wor !! n)), y<-(snd (fuente !! n)),(wordToNum x) == (wordToNum y)]|n<-[0..(v-1)]]
            encajes = concat posibs
        print encajes
        print (frecPalabras (separaPalabras2 b))
        let mapas = concat (map (prueba2522) encajes)
            resultado = pruebanueva mapas
            nuevaFrec = zip ['a'..'z'] (take 26 (repeat []))
        print resultado
        --print encajes
prueba2522 (x,y) = zip x y



-- calculaFrecAux (x:xs) [] = calculaFrecAux xs ((x,1):[])
-- calculaFrecAux (x:xs) (y:ys)
--     | x == (fst y) = calculaFrecAux xs ((x,1+(snd y)):ys)
--     | x /= (fst y) = calculaFrecAux (x:xs) ys
-- calculaFrecAux [] y = y
hola2 (a:b) = 
    y
    where y=length(takeWhile (==a) b)


--sum(map snd prueba1)

testFrec1 = 
    [calculaFrecuencias (take i prueba0) | i<-[1..184]]












