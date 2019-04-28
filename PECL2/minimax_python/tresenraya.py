import pygame, time

# CONSTANTES
# colores
COLOR_1 = (255, 255, 255)
COLOR_2 = (204, 204, 204)
# ventana
T_CASILLA = 200
T_LETRA = 128
FPS = 30
IMAGES = {"X": pygame.transform.smoothscale(pygame.image.load("images/x.png"), (T_LETRA, T_LETRA)),
          "O": pygame.transform.smoothscale(pygame.image.load("images/o.png"), (T_LETRA, T_LETRA))}
# juego
VACIO = " "
JUGADOR = "X"
MAQUINA = "O"
DIFICULTAD = 5


# FUNCIONES
def render_tablero(screen, tablero):
    """Renderiza el tablero y pinta el fondo con los colores del tema
    ARGUMENTOS:
        -screen. Superficie principal de pygame sobre la que se pinta todo (display).
        -tablero. String de longitud 9 que contiene los valores del tablero."""
    # pintar fondo
    b = False
    for i in range(0, T_CASILLA * 3, T_CASILLA):
        for j in range(0, T_CASILLA * 3, T_CASILLA):
            b = b == False
            if (b):
                pygame.draw.rect(screen, COLOR_1, ((i, j), (T_CASILLA, T_CASILLA)))
            else:
                pygame.draw.rect(screen, COLOR_2, ((i, j), (T_CASILLA, T_CASILLA)))
    # poner Os y Xs
    for a in range(9):
        if (tablero[a] == " "):
            continue
        screen.blit(IMAGES[tablero[a]], (a % 3 * T_CASILLA + (T_CASILLA - T_LETRA) / 2, a // 3 * T_CASILLA + (T_CASILLA - T_LETRA) / 2))


def minimax(tablero, turno_player, profundidad):
    """Implementacion del algoritmo minimax a nuestro tres en raya.
    ARGUMENTOS:
        -tablero. String de longitud 9 que contiene los valores del tablero.
        -turno_player. Booleano que indica el turno, si es positivo significa que le toca al jugador humano.
        -profundidad. Valor numerico que limita el numero de veces que la funcion se llama a si misma (dificultad) y que incita a la maquina
                      a realizar los movimientos que impliquen alargar la partida lo maximo posible (intentando ganar siempre)."""
    if (ganador(tablero) == MAQUINA):
        return (+10 - profundidad, None)  # gana pc
    elif (ganador(tablero) == JUGADOR):
        return (-10 - profundidad, None)  # pierde pc
    elif ((VACIO not in tablero) or (profundidad < 1)):
        return (0, None)  # empatan
    elif (turno_player):  # turno de jugador
        best = (+11, None)
        for a in range(9):
            if (tablero[a] == " "):
                valor = minimax(tablero[:a] + JUGADOR + tablero[a + 1:], not turno_player, profundidad - 1)[0]
                if (valor < best[0]):
                    best = (valor, a)  # jugador intenta causar el MENOR beneficio a pc
        return best
    else:  # turno de pc
        best = (-11, None)
        for a in range(9):
            if (tablero[a] == " "):
                valor = minimax(tablero[:a] + MAQUINA + tablero[a + 1:], not turno_player, profundidad - 1)[0]
                if (valor > best[0]):
                    best = (valor, a)  # pc intenta causar el MAYOR beneficio a si mismo
        return best


def ganador(tablero):
    """Indica si alguien ha ganado la partida y en caso verdadero devuelve la letra del ganador.
    Como argumento toma unicamente el tablero como string de longitud 9."""
    filas_ganadoras = ((0, 1, 2), (3, 4, 5), (6, 7, 8),
                       (0, 3, 6), (1, 4, 7), (2, 5, 8),
                       (0, 4, 8), (2, 4, 6))
    for fila in filas_ganadoras:
        if ((tablero[fila[0]] == VACIO) or (tablero[fila[1]] == VACIO) or (tablero[fila[2]] == VACIO)):
            continue
        if (tablero[fila[0]] == tablero[fila[1]] == tablero[fila[2]]):
            return tablero[fila[0]]
    return VACIO


def movimiento_pc(tablero):
    """Realiza el movimiento del pc en el tablero"""
    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
    t0 = time.time()  # inicio cronometro
    if tablero[4] == VACIO:
        a = 4  # cuando el centro esta vacio siempre trata de ocuparlo
    else:
        a = minimax(tablero, False, DIFICULTAD)[1]  # algoritmo minimax limitado segun la dificultad

    if (a is not None):
        tablero = tablero[:a] + MAQUINA + tablero[a+1:] # sustituye la posicion a en el tablero por su letra

    print("La maquina ha tardado {:.5f} ms".format((time.time() - t0) * 1000))  # tiempo desde inicio del cronometro
    pygame.mouse.set_cursor(*pygame.cursors.arrow)
    return tablero


# PROGRAMA PRINCIPAL
resolucion = (T_CASILLA * 3, T_CASILLA * 3)
pygame.init()
clock = pygame.time.Clock()
screen = pygame.display.set_mode(resolucion)
pygame.display.set_caption("3 en raya")
contador = ""
while True:
    # variables que se han de resetear al iniciar la partida
    turno_player = True
    tablero = VACIO * 9
    salir = False
    while not salir:
        # limitar los FPS para no consumir recursos innecesarios
        clock.tick(FPS)
        if ((not turno_player) and (ganador(tablero) == VACIO) and (VACIO in tablero)):
            tablero = movimiento_pc(tablero)
            turno_player = True
        # eventos
        for event in pygame.event.get():
            if (event.type == pygame.QUIT):  # click en la cruz roja
                raise SystemExit
            elif (event.type == pygame.KEYDOWN):
                if (event.key == pygame.K_ESCAPE):
                    raise SystemExit
            elif (event.type == pygame.MOUSEBUTTONDOWN):
                if (event.button == 1):
                    # Cuando la partida termina, se espera a clickar para reiniciarla
                    if ((ganador(tablero) != VACIO) or (not VACIO in tablero)):
                        salir = True
                    x = event.pos[0] // T_CASILLA + 3 * (event.pos[1] // T_CASILLA)
                    if (tablero[x] == " "):
                        tablero = tablero[:x] + JUGADOR + tablero[x + 1:]
                        turno_player = False
        # dibuja tablero
        render_tablero(screen, tablero)
        pygame.display.flip()
    if (ganador(tablero) != VACIO):
        contador = contador + ganador(tablero)
        print(ganador(tablero), "ha ganado! TOTAL: ", "X", str(contador.count("X"))+"-"+str(contador.count("O")), "O")
        print()
        salir = True
    elif VACIO not in tablero:
        print("EMPATE!! TOTAL: ", "X", str(contador.count("X"))+"-"+str(contador.count("O")), "O")
        print()
        salir = True
