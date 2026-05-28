open System
open System.Threading

// ======================================================
// TIPOS DE DATOS
// ======================================================

type EstadoPrograma =
    | Menu
    | Ejecutando
    | Terminado

type Estado = {

    EstadoPrograma : EstadoPrograma

    RedibujarPantalla : bool

    Tick : int

    Reloj : int

    //  jugador
    MonstruoX : int
    MonstruoY : int

    IndiceMenu : int

    //  misiles jugador
    Misiles : (int * int) list

    //  enemigo
    EnemigoX : int
    EnemigoY : int

    //  misiles enemigo
    MisilesEnemigo : (int * int) list

    //  vidas
    Vidas : int

    //  niveles
    Nivel : int
}

// ======================================================
// ESTADO INICIAL
// ======================================================

let estadoInicial = {

    EstadoPrograma = Menu

    RedibujarPantalla = true

    Tick = 0

    Reloj = 0

    MonstruoX = 5

    MonstruoY = Console.BufferHeight / 2

    IndiceMenu = 0

    Misiles = []

    EnemigoX = Console.BufferWidth - 10

    EnemigoY = Console.BufferHeight / 2

    MisilesEnemigo = []

    Vidas = 5

    Nivel = 1
}

// ======================================================
// MENU
// ======================================================

let opcionesMenu = [|

    "Nuevo Juego"
    "Cargar Juego"
    "Salir"

|]

// ======================================================
// MOSTRAR MENSAJES
// ======================================================

let mostrarMensaje x y color (mensaje:string) =

    if x >= 0 &&
       x < Console.BufferWidth &&
       y >= 0 &&
       y < Console.BufferHeight then

        Console.SetCursorPosition(x,y)

        Console.ForegroundColor <- color

        Console.Write(mensaje)

// ======================================================
// ACTUALIZACIONES
// ======================================================

let actualizarTick estado =

    { estado with Tick = estado.Tick + 1 }

let actualizarReloj estado =

    if estado.Tick % 40 = 0 then

        { estado with Reloj = estado.Reloj + 1 }

    else

        estado

// ======================================================
// MISILES JUGADOR
// ======================================================

let actualizarMisiles estado =

    let velocidad = 2 + estado.Nivel

    let nuevos =

        estado.Misiles

        |> List.map (fun (x,y) -> (x + velocidad, y))

        |> List.filter (fun (x,_) -> x < Console.BufferWidth - 4)

    { estado with Misiles = nuevos }

// ======================================================
// MISILES ENEMIGO
// ======================================================

let actualizarMisilesEnemigo estado =

    let velocidad = 1 + estado.Nivel

    let nuevos =

        estado.MisilesEnemigo

        |> List.map (fun (x,y) -> (x - velocidad, y))

        |> List.filter (fun (x,_) -> x > 0)

    { estado with MisilesEnemigo = nuevos }

// ======================================================
// ENEMIGO EN ONDA SENO
// ======================================================

let actualizarEnemigo estado =

    let amplitud = 5.0 + float estado.Nivel

    let nuevaY =

        int (
            float(Console.BufferHeight / 2)
            +
            Math.Sin(float estado.Tick * 0.1) * amplitud
        )

    { estado with EnemigoY = nuevaY }

// ======================================================
// DISPARO ENEMIGO
// ======================================================

let disparoEnemigo estado =

    let velocidadDisparo = max 8 (18 - estado.Nivel)

    if estado.Tick % velocidadDisparo = 0 then

        let nuevoMisil =

            (estado.EnemigoX - 2,
             estado.EnemigoY)

        { estado with

            MisilesEnemigo =
                nuevoMisil :: estado.MisilesEnemigo
        }

    else

        estado

// ======================================================
// IMPACTO EN JUGADOR
// ======================================================

let detectarImpactosJugador estado =

    let impacto =

        estado.MisilesEnemigo

        |> List.exists (fun (x,y) ->

            x >= estado.MonstruoX &&
            x <= estado.MonstruoX + 2 &&

            y = estado.MonstruoY
        )

    if impacto then

        Console.Clear()

        mostrarMensaje 25 10 ConsoleColor.Red
            "¡PERDISTE UNA VIDA!"

        Thread.Sleep(2000)

        let misilesRestantes =

            estado.MisilesEnemigo

            |> List.filter (fun (x,y) ->

                not (
                    x >= estado.MonstruoX &&
                    x <= estado.MonstruoX + 2 &&
                    y = estado.MonstruoY
                ))

        { estado with

            Vidas = estado.Vidas - 1

            MisilesEnemigo = misilesRestantes
        }

    else

        estado

// ======================================================
// IMPACTO EN ENEMIGO
// ======================================================

let detectarImpactosEnemigo estado =

    // AUMENTAMOS EL RANGO DE IMPACTO
    let enemigoGolpeado =

        estado.Misiles

        |> List.exists (fun (x,y) ->

            x >= estado.EnemigoX - 3 &&
            x <= estado.EnemigoX + 3 &&

            y >= estado.EnemigoY - 1 &&
            y <= estado.EnemigoY + 1
        )

    if enemigoGolpeado then

        // NIVEL FINAL
        if estado.Nivel = 5 then

            Console.Clear()

            mostrarMensaje 30 10 ConsoleColor.Green
                "¡¡WIN!!"

            Thread.Sleep(3000)

            { estado with
                EstadoPrograma = Terminado }

        else

            Console.Clear()

            mostrarMensaje 20 10 ConsoleColor.Green
                ("¡SUBISTE AL NIVEL "
                 + string (estado.Nivel + 1)
                 + "!")

            Thread.Sleep(2000)

            { estado with

                Nivel = estado.Nivel + 1

                Misiles = []

                MisilesEnemigo = []

                EnemigoY = Console.BufferHeight / 2
            }

    else

        estado

// ======================================================
// DIBUJAR MENU
// ======================================================

let dibujarMenu estado =

    Console.Clear()

    // Centrar título
    let centroX = Console.BufferWidth / 2 - 20

    // 🎮 TITULO GRANDE estilo pixel (como tu imagen)
    mostrarMensaje centroX 3 ConsoleColor.Green  "███╗   ███╗███████╗███╗   ██╗"
    mostrarMensaje centroX 4 ConsoleColor.Green  "████╗ ████║██╔════╝████╗  ██║"
    mostrarMensaje centroX 5 ConsoleColor.Green  "██╔████╔██║█████╗  ██╔██╗ ██║"
    mostrarMensaje centroX 6 ConsoleColor.Green  "██║╚██╔╝██║██╔══╝  ██║╚██╗██║"
    mostrarMensaje centroX 7 ConsoleColor.Green  "██║ ╚═╝ ██║███████╗██║ ╚████║"
    mostrarMensaje centroX 8 ConsoleColor.Green  "╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝"

    mostrarMensaje centroX 10 ConsoleColor.Red  " █████╗ ████████╗████████╗ █████╗  ██████╗██╗  ██╗"
    mostrarMensaje centroX 11 ConsoleColor.Red  "██╔══██╗╚══██╔══╝╚══██╔══╝██╔══██╗██╔════╝██║ ██╔╝"
    mostrarMensaje centroX 12 ConsoleColor.Red  "███████║   ██║      ██║   ███████║██║     █████╔╝ "
    mostrarMensaje centroX 13 ConsoleColor.Red  "██╔══██║   ██║      ██║   ██╔══██║██║     ██╔═██╗ "
    mostrarMensaje centroX 14 ConsoleColor.Red  "██║  ██║   ██║      ██║   ██║  ██║╚██████╗██║  ██╗"
    mostrarMensaje centroX 15 ConsoleColor.Red  "╚═╝  ╚═╝   ╚═╝      ╚═╝   ╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝"

    //  efecto parpadeo
    let colorTitulo =
        if estado.Tick % 20 < 10 then ConsoleColor.Yellow
        else ConsoleColor.Cyan

    mostrarMensaje (centroX + 10) 17 colorTitulo "=== BIENVENIDO ==="

    //  OPCIONES (centradas)
    for i = 0 to opcionesMenu.Length - 1 do

        let color =
            if i = estado.IndiceMenu then ConsoleColor.Yellow
            else ConsoleColor.Gray

        let prefijo =
            if i = estado.IndiceMenu then "► "
            else "  "

        let texto = prefijo + opcionesMenu.[i]

        let posX = Console.BufferWidth / 2 - texto.Length / 2

        mostrarMensaje posX (20 + i) color texto

    //  instrucciones
    mostrarMensaje (Console.BufferWidth / 2 - 12) 25 ConsoleColor.DarkGray
        "↑ ↓ ENTER"

// ======================================================
// DIBUJAR JUEGO
// ======================================================

let dibujarJuego estado =

    Console.Clear()

    // reloj
    mostrarMensaje 2 0 ConsoleColor.Green
        ("Reloj: " + string estado.Reloj)

    // vidas
    mostrarMensaje 2 1 ConsoleColor.Red
        ("Vidas: " + string estado.Vidas)

    // nivel
    mostrarMensaje 2 2 ConsoleColor.Cyan
        ("Nivel: " + string estado.Nivel)

    // jugador
    mostrarMensaje
        estado.MonstruoX
        estado.MonstruoY
        ConsoleColor.Red
        "👽"

    // misiles jugador
    for (x,y) in estado.Misiles do

        mostrarMensaje x y
            ConsoleColor.Yellow
            "==>"

    // enemigo
    mostrarMensaje
        estado.EnemigoX
        estado.EnemigoY
        ConsoleColor.Magenta
        "🤖"

    // misiles enemigo
    for (x,y) in estado.MisilesEnemigo do

        mostrarMensaje x y
            ConsoleColor.Cyan
            "<=="

    // controles
    mostrarMensaje 2 4 ConsoleColor.Gray
        "ESPACIO = Disparar"

    mostrarMensaje 2 5 ConsoleColor.Gray
        "ESC = Menu"

// ======================================================
// REDIBUJAR
// ======================================================

let redibujarPantalla estado =

    match estado.EstadoPrograma with

    | Ejecutando ->

        dibujarJuego estado

        estado

    | Menu ->

        if estado.RedibujarPantalla then

            Console.Clear()

            dibujarMenu estado

            { estado with
                RedibujarPantalla = false }

        else

            estado

    | Terminado ->

        estado

// ======================================================
// TECLADO MENU
// ======================================================

let actualizarTecladoMenu tecla estado =

    match tecla with

    | ConsoleKey.UpArrow ->

        { estado with

            IndiceMenu =
                max 0 (estado.IndiceMenu - 1)

            RedibujarPantalla = true }

    | ConsoleKey.DownArrow ->

        { estado with

            IndiceMenu =
                min (opcionesMenu.Length - 1)
                    (estado.IndiceMenu + 1)

            RedibujarPantalla = true }

    | ConsoleKey.Enter ->

        match estado.IndiceMenu with

        | 0 ->

            { estado with
                EstadoPrograma = Ejecutando }

        | 1 ->

            Console.Clear()

            mostrarMensaje 20 10
                ConsoleColor.Magenta
                "Cargando..."

            Thread.Sleep(1500)

            { estado with
                RedibujarPantalla = true }

        | 2 ->

            { estado with
                EstadoPrograma = Terminado }

        | _ -> estado

    | _ -> estado

// ======================================================
// TECLADO JUEGO
// ======================================================

let actualizarTecladoJuego tecla estado =

    match tecla with

    | ConsoleKey.LeftArrow ->

        { estado with

            MonstruoX =
                max 0 (estado.MonstruoX - 1) }

    | ConsoleKey.RightArrow ->

        { estado with

            MonstruoX =
                min (Console.BufferWidth - 5)
                    (estado.MonstruoX + 1) }

    | ConsoleKey.UpArrow ->

        { estado with

            MonstruoY =
                max 1 (estado.MonstruoY - 1) }

    | ConsoleKey.DownArrow ->

        { estado with

            MonstruoY =
                min (Console.BufferHeight - 2)
                    (estado.MonstruoY + 1) }

    | ConsoleKey.Spacebar ->

        let nuevoMisil =

            (estado.MonstruoX + 3,
             estado.MonstruoY)

        { estado with

            Misiles =
                nuevoMisil :: estado.Misiles }

    | ConsoleKey.Escape ->

        { estado with

            EstadoPrograma = Menu

            RedibujarPantalla = true }

    | _ -> estado

// ======================================================
// PROCESAR TECLADO
// ======================================================

let procesarTeclado estado =

    if Console.KeyAvailable then

        let tecla = Console.ReadKey(true)

        match estado.EstadoPrograma with

        | Menu ->
            actualizarTecladoMenu tecla.Key estado

        | Ejecutando ->
            actualizarTecladoJuego tecla.Key estado

        | Terminado ->
            estado

    else

        estado

// ======================================================
// LOOP PRINCIPAL
// ======================================================

let rec cicloPrincipal estado =

    let nuevoEstado =

        estado
        |> actualizarTick
        |> actualizarReloj
        |> procesarTeclado
        |> actualizarMisiles
        |> actualizarMisilesEnemigo
        |> actualizarEnemigo
        |> disparoEnemigo
        |> detectarImpactosJugador
        |> detectarImpactosEnemigo
        |> redibujarPantalla

    // GAME OVER
    if nuevoEstado.Vidas <= 0 then

        Console.Clear()

        mostrarMensaje 30 10
            ConsoleColor.Red
            "GAME OVER"

        Thread.Sleep(3000)

    elif nuevoEstado.EstadoPrograma <> Terminado then

        Thread.Sleep(25)

        cicloPrincipal nuevoEstado

// ======================================================
// INICIO
// ======================================================

Console.Clear()

Console.CursorVisible <- false

let colorAnterior =
    Console.ForegroundColor

cicloPrincipal estadoInicial

Console.ForegroundColor <- colorAnterior

Console.CursorVisible <- true

Console.Clear()

mostrarMensaje 25 10
    ConsoleColor.White
    "Programa Finalizado"

Console.ReadKey() |> ignore    