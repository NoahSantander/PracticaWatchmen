module Library where
import PdePreludat

-- Defino mis alias
type Nombre = String
type Habilidad = String
type Habilidades = [Habilidad]
type Year = Number
type Vigilante = (Nombre, Habilidades, Year)
type Vigilantes = [Vigilante]
type Evento = Vigilantes -> Vigilantes
type Eventos = [Evento]
type Edad = Number

-- Defino mis tipos
algunosVigilantes = [ ("El Comediante", ["Fuerza"], 1942), ("Buho Nocturno", ["Lucha", "Ingenierismo"], 1963), ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964), ("Espectro de Seda", ["Lucha", "Sigilo", "Fuerza"], 1962), ("Ozimandias", ["Inteligencia", "Más Inteligencia Aún"], 1968), ("Buho Nocturno", ["Lucha", "Inteligencia", "Fuerza"], 1939), ("Espectro de Seda", ["Lucha", "Sigilo"], 1940)]
agentesDelGobierno = [("Jack Bauer","24"), ("El Comediante", "Watchmen"), ("Dr. Manhattan", "Watchmen"), ("Liam Neeson", "Taken")]

-- Defino funciones auxiliares
vigilanteNombre :: Vigilante -> Nombre
vigilanteNombre (nombre, _, _) = nombre

vigilanteHabilidades :: Vigilante -> Habilidades
vigilanteHabilidades (_, habilidadesVigilante, _) = habilidadesVigilante

vigilanteYear :: Vigilante -> Year
vigilanteYear (_, _, yearAparicion) = yearAparicion

retirar :: Nombre -> Vigilantes -> Vigilantes
retirar nombre vigilantes = filter (not.(==nombre).vigilanteNombre) vigilantes

agregarHabilidad :: Habilidad -> Vigilante -> Vigilante
agregarHabilidad habilidad (algo, habilidades, algo2) = (algo, habilidad:habilidades, algo2)

nombresAgentes :: [Nombre]
nombresAgentes = map (fst) agentesDelGobierno
nombresVigilantes :: [Nombre]
nombresVigilantes = map (vigilanteNombre) algunosVigilantes

esAgente :: Vigilante -> Bool
esAgente vigilante = elem (vigilanteNombre vigilante) nombresAgentes

sonAgentes :: Vigilantes -> Vigilantes
sonAgentes vigilantes = filter (esAgente) vigilantes 

tieneSucesor :: Vigilante -> Bool
tieneSucesor vigilante = ((>1).length) (filter (==(vigilanteNombre vigilante)) nombresVigilantes)

sonElMismo :: Vigilante -> Vigilante -> Bool
sonElMismo vigilante1 vigilante2 = vigilanteNombre vigilante1 == vigilanteNombre vigilante2 
esMasJoven :: Vigilante -> Vigilante -> Vigilante
esMasJoven vigilante1 vigilante2 
    | vigilanteYear vigilante1 > vigilanteYear vigilante2 = vigilante1
    | otherwise = vigilante2

eliminarMasViejo :: Vigilantes -> Vigilantes
eliminarMasViejo [] = [] 
eliminarMasViejo (vigilante:vigilantes) = foldl (esMasJoven) vigilante (filter (sonElMismo vigilante) vigilantes):(eliminarMasViejo (filter ((not.sonElMismo vigilante)) vigilantes))

-- Defino los eventos
destruccionNiuShork :: Evento
destruccionNiuShork = (retirar "Rorschach").(retirar "Dr. Manhattan") 

muerteVigilante :: Nombre -> Evento
muerteVigilante = retirar

guerraDeVietnam :: Evento
guerraDeVietnam vigilantes = (map (agregarHabilidad "Cinismo") (sonAgentes vigilantes))++(filter (not.esAgente) vigilantes)

accidenteDeLaboratorio :: Year -> Evento
accidenteDeLaboratorio year vigilantes = ("Dr. Manhattan", ["Manipulacion de la materia a nivel atomico"],year):vigilantes

actaDeKeene :: Evento
actaDeKeene vigilantes = eliminarMasViejo (filter (tieneSucesor) vigilantes) ++ (filter (not.tieneSucesor) vigilantes)

-- Defino desarrollo de una historia
historia :: Eventos -> Vigilantes -> Vigilantes
historia eventos vigilantes = foldr ($) vigilantes $ eventos

-- Defino la función
tieneMasHabilidades :: Vigilante -> Vigilante -> Vigilante
tieneMasHabilidades vigilante1 vigilante2 
    | (length.vigilanteHabilidades) vigilante1 > (length.vigilanteHabilidades) vigilante2 = vigilante1
    | otherwise = vigilante2

nombreDelSalvador :: Vigilantes -> Nombre
nombreDelSalvador vigilantes = vigilanteNombre (foldl1 (tieneMasHabilidades) (destruccionNiuShork vigilantes))

-- Defino la función 
tieneNombreMasLargo :: Vigilante -> Vigilante -> Vigilante
tieneNombreMasLargo vigilante1 vigilante2 
    | (length.vigilanteNombre) vigilante1 > (length.vigilanteNombre) vigilante2 = vigilante1
    | otherwise = vigilante2

elElegido :: Vigilantes -> Habilidad
elElegido vigilantes = (head.vigilanteHabilidades) (foldl1 (tieneNombreMasLargo) (guerraDeVietnam vigilantes))

-- Defino la función 
calcularEdad :: Vigilante -> Edad
calcularEdad vigilante = 2023 - vigilanteYear vigilante

masAntiguo :: Vigilante -> Vigilante -> Vigilante
masAntiguo vigilante1 vigilante2 
    | vigilanteYear vigilante1 < vigilanteYear vigilante2 = vigilante1
    | otherwise = vigilante2

patriarca :: Vigilantes -> Edad
patriarca vigilantes = calcularEdad (foldl1 (masAntiguo) (actaDeKeene vigilantes))

-- Usar funciones de orden superior me fue util para utilizar una funcion como argumento de otra y que esta la use, es decir, que le delega la responsabilidad, sin conocerla.