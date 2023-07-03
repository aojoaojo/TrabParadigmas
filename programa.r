# Definindo as variáveis para os registradores
ACC <- 0  # Registrador acumulador
BNK <- 0  # Registrador de backup
IPT <- 1  # Registrador ponteiro de instrução
NIL <- 0  # Registrador nulo

# Função para executar a instrução NOP
NOP <- function() {
  # Não faz nada
  return(NULL)
}

# Função para executar a instrução MOV
MOV <- function(reg, imediato) {
  # Verificar o registrador
  if (reg == "ACC") {
    ACC <<- imediato
  } else if (reg == "NIL") {
    NIL <<- imediato
  } else {
    print("Registrador inválido!")
  }
}


# Função para executar a instrução SAV
SAV <- function() {
  BNK <<- ACC
}

# Função para executar a instrução SWP
SWP <- function() {
  temp <- ACC
  ACC <<- BNK
  BNK <<- temp
}

# Função para executar a instrução NEG
NEG <- function() {
  ACC <<- -ACC
}

# Função para executar a instrução ADD
ADD <- function(imediato) {
  ACC <<- ACC + imediato
}

# Função para executar a instrução SUB
SUB <- function(imediato) {
  ACC <<- ACC - imediato
}

# Função para executar a instrução PNT
PNT <- function() {
  print(ACC)
}

# Função para executar a instrução JMP
JMP <- function(rotulo) {
  # Buscar a posição da instrução correspondente ao rótulo
  posicao <- obterPosicaoRotulo(rotulo)
  
  # Verificar se o rótulo é válido
  if (posicao != -1) {
    IPT <<- posicao
  } else {
    print("Rótulo inválido!")
  }
}

# Função auxiliar para obter a posição de um rótulo no programa
obterPosicaoRotulo <- function(rotulo) {
  posicao <- -1
  
  # Verificar cada instrução no programa em busca do rótulo
  for (i in 1:length(programa)) {
    instrucao <- programa[i]
    
    # Verificar se a instrução é um rótulo
    if (endsWith(instrucao, ":")) {
      # Remover o ":" para comparar com o rótulo fornecido
      r <- substr(instrucao, 1, nchar(instrucao) - 1)
      
      # Se o rótulo for igual ao rótulo fornecido, obter a posição
      if (r == rotulo) {
        posicao <- i
        break
      }
    }
  }
  
  return(posicao)
}

# Função para executar a instrução JEQ
JEQ <- function(rotulo) {
  if (ACC == 0) {
    JMP(rotulo)
  }
}

# Função para executar a instrução JNZ
JNZ <- function(rotulo) {
  if (ACC != 0) {
    JMP(rotulo)
  }
}

# Função para executar a instrução JGZ
JGZ <- function(rotulo) {
  if (ACC > 0) {
    JMP(rotulo)
  }
}

# Função para executar a instrução JLZ
JLZ <- function(rotulo) {
  if (ACC < 0) {
    JMP(rotulo)
  }
}

# Dicionário de rótulos
rotulos <- list()

# Função para fazer o parse do arquivo de programa
parseProgram <- function(arquivo) {
  instrucoes <- readLines(arquivo)
  
  # Verificar o número máximo de instruções
  if (length(instrucoes) > 32) {
    print("Número máximo de instruções excedido!")
    return(FALSE)
  }
  
  # Variável para controlar o número de instruções
  numInstrucoes <- 0
  
  # Percorrer as instruções
  for (instrucao in instrucoes) {
    partes <- strsplit(instrucao, " ")[[1]]
    
    if (length(partes) > 0) {
      # Verificar se é um rótulo
      if (endsWith(partes[1], ":")) {
        rotulo <- substr(partes[1], 1, nchar(partes[1]) - 1)
        if (rotulo %in% names(rotulos)) {
          print("Rótulo duplicado!")
          return(FALSE)
        } else {
          rotulos[[rotulo]] <- numInstrucoes + 1
        }
        
        # Remover o rótulo da instrução
        partes <- partes[-1]
      }
      
      # Verificar o código da instrução
      if (length(partes) > 0) {
        instrucao <- partes[1]
        
        if (instrucao == "NOP") {
          numInstrucoes <- numInstrucoes + 1
        } else if (instrucao == "MOV") {
          if (length(partes) == 3) {
            reg <- partes[2]
            imediato <- as.integer(partes[3])
            if (is.na(imediato)) {
              print("Imediato inválido!")
              return(FALSE)
            }
            numInstrucoes <- numInstrucoes + 1
          } else {
            print("Instrução MOV inválida!")
            return(FALSE)
          }
        } else if (instrucao == "SAV") {
          numInstrucoes <- numInstrucoes + 1
        } else if (instrucao == "SWP") {
          numInstrucoes <- numInstrucoes + 1
        } else if (instrucao == "NEG") {
          numInstrucoes <- numInstrucoes + 1
        } else if (instrucao == "ADD") {
          if (length(partes) == 2) {
            imediato <- as.integer(partes[2])
            if (is.na(imediato)) {
              print("Imediato inválido!")
              return(FALSE)
            }
            numInstrucoes <- numInstrucoes + 1
          } else {
            print("Instrução ADD inválida!")
            return(FALSE)
          }
        } else if (instrucao == "SUB") {
          if (length(partes) == 2) {
            imediato <- as.integer(partes[2])
            if (is.na(imediato)) {
              print("Imediato inválido!")
              return(FALSE)
            }
            numInstrucoes <- numInstrucoes + 1
          } else {
            print("Instrução SUB inválida!")
            return(FALSE)
          }
        } else if (instrucao == "PNT") {
          numInstrucoes <- numInstrucoes + 1
        } else if (instrucao == "JMP") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            numInstrucoes <- numInstrucoes + 1
          } else {
            print("Instrução JMP inválida!")
            return(FALSE)
          }
        } else if (instrucao == "JEQ") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            numInstrucoes <- numInstrucoes + 1
          } else {
            print("Instrução JEQ inválida!")
            return(FALSE)
          }
        } else if (instrucao == "JNZ") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            numInstrucoes <- numInstrucoes + 1
          } else {
            print("Instrução JNZ inválida!")
            return(FALSE)
          }
        } else if (instrucao == "JGZ") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            numInstrucoes <- numInstrucoes + 1
          } else {
            print("Instrução JGZ inválida!")
            return(FALSE)
          }
        } else if (instrucao == "JLZ") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            numInstrucoes <- numInstrucoes + 1
          } else {
            print("Instrução JLZ inválida!")
            return(FALSE)
          }
        } else {
          print("Instrução inválida!")
          return(FALSE)
        }
      }
    }
  }
  
  # Verificar se o número de instruções é igual ao número de rótulos
  if (numInstrucoes != length(rotulos)) {
    print("Programa inválido!")
    return(FALSE)
  }
  
  return(TRUE)
}

# Função para executar o programa
executarPrograma <- function(arquivo) {
  # Fazer o parse do arquivo de programa
  if (!parseProgram(arquivo)) {
    return
  }
  
  # Definir o número máximo de instruções
  maxInstrucoes <- length(rotulos)
  
  # Inicializar os registradores
  ACC <- 0
  BNK <- 0
  IPT <- 1
  NIL <- 0
  
  # Loop principal de execução
  while (IPT <= maxInstrucoes) {
    # Obter a próxima instrução
    instrucao <- instrucoes[IPT]
    
    # Dividir a instrução em partes
    partes <- strsplit(instrucao, " ")[[1]]
    
    if (length(partes) > 0) {
      # Verificar se é um rótulo
      if (endsWith(partes[1], ":")) {
        # Remover o rótulo da instrução
        partes <- partes[-1]
      }
      
      # Verificar o código da instrução
      if (length(partes) > 0) {
        instrucao <- partes[1]
        
        if (instrucao == "NOP") {
          NOP()
        } else if (instrucao == "MOV") {
          if (length(partes) == 3) {
            reg <- partes[2]
            imediato <- as.integer(partes[3])
            MOV(reg, imediato)
          }
        } else if (instrucao == "SAV") {
          SAV()
        } else if (instrucao == "SWP") {
          SWP()
        } else if (instrucao == "NEG") {
          NEG()
        } else if (instrucao == "ADD") {
          if (length(partes) == 2) {
            imediato <- as.integer(partes[2])
            ADD(imediato)
          }
        } else if (instrucao == "SUB") {
          if (length(partes) == 2) {
            imediato <- as.integer(partes[2])
            SUB(imediato)
          }
        } else if (instrucao == "PNT") {
          PNT()
        } else if (instrucao == "JMP") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            JMP(rotulo)
          }
        } else if (instrucao == "JEQ") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            JEQ(rotulo)
          }
        } else if (instrucao == "JNZ") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            JNZ(rotulo)
          }
        } else if (instrucao == "JGZ") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            JGZ(rotulo)
          }
        } else if (instrucao == "JLZ") {
          if (length(partes) == 2) {
            rotulo <- partes[2]
            JLZ(rotulo)
          }
        }
      }
    }
    
    # Incrementar o ponteiro de instrução
    IPT <- IPT + 1
  }
}

# Executar o programa
executarPrograma("programa.txt")
