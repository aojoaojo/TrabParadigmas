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
  # Verificar se o rótulo é válido
  # if (obterPosicaoRotulo(rotulo) != -1) {
  #   obterPosicaoRotulo(rotulo)
  # } else {
  #   print("Rótulo inválido!")
  # }
  obterPosicaoRotulo(rotulo)
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

# Função para executar a instrução JZ
JZ <- function(rotulo) {
  if (ACC == 0) {
    JMP(rotulo)
  }
}

# Função para executar a instrução JS
JS <- function(rotulo) {
  if (ACC < 0) {
    JMP(rotulo)
  }
}

# debug(obterPosicaoRotulo)

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
  
  # return(posicao)
}

# Função para executar o programa
program <- function(instrucoes) {
  programa <<- instrucoes
  # Executar as instruções do programa
  while (IPT <= length(programa)) {
    instrucao <- programa[IPT]
    # Separar a instrução e seus argumentos
    partes <- strsplit(instrucao, " ")[[1]]
    comando <- partes[1]
    argumento <- partes[2]
    
    # Executar a instrução correspondente
    # print(IPT)
    IPT <<- IPT + 1
    

    if (comando == "NOP") {
      NOP()
      # print("NOP ok")
    } else if (comando == "MOV") {
      reg <- argumento
      imediato <- as.numeric(partes[3])
      MOV(reg, imediato)
    #   print("MOV ok")
    } else if (comando == "SAV") {
      SAV()
    #   print("SAV ok")
    } else if (comando == "SWP") {
      SWP()
    #   print("SWP ok")
    } else if (comando == "NEG") {
      NEG()
    #   print("NEG ok")
    } else if (comando == "ADD") {
      imediato <- as.numeric(partes[2])
      ADD(imediato)
    #   print("ADD ok")
    } else if (comando == "SUB") {
      imediato <- as.numeric(partes[2])
      SUB(imediato)
    #   print("SUB ok")
    } else if (comando == "PNT") {
      PNT()
    #   print("PNT ok")
    } else if (comando == "JMP") {
      rotulo <- argumento
      JMP(rotulo)
    #   print("JMP ok")
    } else if (comando == "JEQ") {
      rotulo <- argumento
      JEQ(rotulo)
    #   print("JEQ ok")
    } else if (comando == "JNZ") {
      rotulo <- argumento
      JNZ(rotulo)
    #   print("JNZ ok")
    } else if (comando == "JGZ") {
      rotulo <- argumento
      JGZ(rotulo)
    #   print("JGZ ok")
    } else if (comando == "JZ") {
      rotulo <- argumento
      JZ(rotulo)
    #   print("JZ ok")
    } else if (comando == "JLZ") {
      rotulo <- argumento
      JLZ(rotulo)
    #   print("JLZ ok")
    } else if (comando == "JS") {
      rotulo <- argumento
      JS(rotulo)
    #   print("JS ok")
    }  #Verificar se a instrução é um rótulo
    else if (endsWith(comando, ":")) {
      # Remover o ":" para comparar com o rótulo fornecido
      r <- substr(instrucao, 1, nchar(instrucao) - 1)
      
      # Se o rótulo for igual ao rótulo fornecido, obter a posição
      if (r == comando) {
        posicao <<- i
        break
      }
    }
    else {
      print("Instrução inválida!")
    }
  }
}

# Exemplo de programa
instrucoes <- readLines("/mnt/c/Users/rafae/Desktop/trab_jeremias/TrabParadigmas/prog-errado-05.idp")
#/home/rafanog/Desktop/TrabParadigmas/prog-correto-05.idp
# debug(SUB)

# Executar o programa
program(instrucoes)