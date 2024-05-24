import Data.Time.Clock
import Data.List
import System.IO
import System.IO (IOMode(WriteMode), openFile, hPutStr, hClose)
import Data.Maybe (catMaybes)
import Control.DeepSeq (rnf)

-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el vehículo aún está en el parqueadero o ya salió
} deriving (Show, Read)  -- Agregamos Read aquí

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    Vehiculo placaVehiculo tiempo Nothing : parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> Either String [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    if any ((== placaVehiculo) . placa) parqueadero
    then Right (map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero)
    else Left ("Vehículo con placa " ++ placaVehiculo ++ " no encontrado en el parqueadero.")


-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    diffUTCTime tiempoActual (entrada vehiculo)

-- Función para guardar la información de los vehículos en un archivo de texto


guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    contenedor <- openFile "parqueadero.txt" WriteMode
    hPutStr contenedor (unlines (map mostrarVehiculo parqueadero))
    hClose contenedor
    putStrLn "Parqueadero guardado en el archivo parqueadero.txt."

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Maybe Vehiculo]
cargarParqueadero = do
    contenedor <- openFile "parqueadero.txt" ReadMode
    contenido <- hGetContents contenedor
    rnf contenido `seq` hClose contenedor
    let lineas = lines contenido
    return (map leerVehiculo lineas)
    where
        leerVehiculo :: String -> Maybe Vehiculo
        leerVehiculo linea = case reads linea of
            [(v, "")] -> Just v
            _         -> Nothing


-- Función para mostrar la información de un vehículo como cadena de texto
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo vehiculo =
    "Vehiculo {placa = \"" ++ placa vehiculo ++ "\", entrada = " ++ show (entrada vehiculo) ++ ", salida = " ++ case salida vehiculo of
        Just tiempo -> show tiempo
        Nothing -> "Nothing"
    ++ "}"

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el parqueadero desde el archivo de texto
    maybeParqueadero <- cargarParqueadero
    let parqueadero = catMaybes maybeParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"
    -- Ciclo principal del programa
    cicloPrincipal parqueadero


-- Función para el ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Listar vehiculos"
    putStrLn "5. Salir"


    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            case registrarSalida placaVehiculo tiempoActual parqueadero of
                Right parqueaderoActualizado -> do
                    putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " salido del parqueadero."
                    guardarParqueadero parqueaderoActualizado
                    cicloPrincipal parqueaderoActualizado
                Left mensajeError -> do
                    putStrLn mensajeError
                    cicloPrincipal parqueadero

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnParqueadero vehiculo tiempoActual
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero

        "4" -> do
            putStrLn "Lista de vehiculos"
            contenido <- readFile "parqueadero.txt"
            putStrLn contenido
            cicloPrincipal parqueadero


        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero