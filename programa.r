# Definindo as variáveis para os registradores
ACC <- 0  # Registrador acumulador
BNK <- 0  # Registrador de backup
IPT <- 1  # Registrador ponteiro de instrução
NIL <- 0  # Registrador nulo

NOP <- function() {
  # Não faz nada
  return(NULL)
}

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

SAV <- function() {
  BNK <<- ACC
}

SWP <- function() {
  temp <- ACC
  ACC <<- BNK
  BNK <<- temp
}

NEG <- function() {
  ACC <<- -ACC
}

ADD <- function(imediato) {
  ACC <<- ACC + imediato
}

SUB <- function(imediato) {
  ACC <<- ACC - imediato
}

PNT <- function() {
  print(ACC)
}

JMP <- function(rotulo) {
  #Verificar se o rótulo é válido
  if (verificaRotulo(rotulo) != -1) {
    obterPosicaoRotulo(rotulo)
  } else {
    print("Rótulo inválido!")
  }
}

JEQ <- function(rotulo) {
  if (ACC == 0) {
    JMP(rotulo)
  }
}

JNZ <- function(rotulo) {
  if (ACC != 0) {
    JMP(rotulo)
  }
}

JGZ <- function(rotulo) {
  if (ACC > 0) {
    JMP(rotulo)
  }
}

JLZ <- function(rotulo) {
  if (ACC < 0) {
    JMP(rotulo)
  }
}

JZ <- function(rotulo) {
  if (ACC == 0) {
    JMP(rotulo)
  }
}

JS <- function(rotulo) {
  if (ACC < 0) {
    JMP(rotulo)
  }
}


# Função para obter a posição de um rótulo no programa
obterPosicaoRotulo <- function(rotulo) {
  posicao <- 0
  
  # Verificar cada instrução no programa em busca do rótulo
  for (i in 1:length(programa)) {
    instrucao <- programa[i]
    novo <- paste(rotulo, ":" , sep = "")
    if(instrucao == novo) {
      IPT <<- i + 1
    }
  }

}
verificaRotulo <- function(rotulo) {
  posicao <- 0
  flag <- 0
  # Verificar cada instrução no programa em busca do rótulo
  for (i in 1:length(programa)) {
    instrucao <- programa[i]
    novo <- paste(rotulo, ":" , sep = "")
    if(instrucao == novo) {
      flag <- 1
    }
  }
  if(flag == 0)
  {
    return(-1)
  }
  return (1)
  
}
# Função para executar o programa
program <- function(instrucoes) {
  programa <<- instrucoes

  if(length(programa) == 0)
  {
    print("Programa vazio!")
    return(NULL)
  }
  # Executar as instruções do programa
  while (IPT <= length(programa)) {
    instrucao <- programa[IPT]

    # Separar a instrução e seus argumentos
    partes <- strsplit(instrucao, " ")[[1]]
    comando <- partes[1]
    argumento <- partes[2]

    IPT <<- IPT + 1
    
    # Executar a instrução
    if (comando == "NOP") {
      NOP()
    } else if (comando == "MOV") {
      reg <- argumento
      imediato <- as.numeric(partes[3])
      MOV(reg, imediato)
    } else if (comando == "SAV") {
      SAV()
    } else if (comando == "SWP") {
      SWP()
    } else if (comando == "NEG") {
      NEG()
    } else if (comando == "ADD") {
      imediato <- as.numeric(partes[2])
      ADD(imediato)
    } else if (comando == "SUB") {
      imediato <- as.numeric(partes[2])
      SUB(imediato)
    } else if (comando == "PNT") {
      PNT()
    } else if (comando == "JMP") {
      rotulo <- argumento
      JMP(rotulo)
    } else if (comando == "JEQ") {
      rotulo <- argumento
      JEQ(rotulo)
    } else if (comando == "JNZ") {
      rotulo <- argumento
      JNZ(rotulo)
    } else if (comando == "JGZ") {
      rotulo <- argumento
      JGZ(rotulo)
    } else if (comando == "JZ") {
      rotulo <- argumento
      JZ(rotulo)
    } else if (comando == "JLZ") {
      rotulo <- argumento
      JLZ(rotulo)
    } else if (comando == "JS") {
      rotulo <- argumento
      JS(rotulo)
    }
    else if (endsWith(comando, ":")) {
      # Se for rotulo, não aponta erro
    }
    else if(substr(comando, 1, 1) == '#')
    {
      # Se for comentário, não aponta erro
    }
    else {
      print("Instrução inválida!")
    }
  }
}

instrucoes <- readLines("path/do/seu/arquivo/aqui")

# Executar o programa
program(instrucoes)
