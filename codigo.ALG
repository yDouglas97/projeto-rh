Algoritmo "SetorRH"

tipo
   cad_funcionarios = registro
      nome_funcio:caractere
      cpf:real
      rg:real
      end_funcio:caractere
      telefone:caractere
      qnt_filhos:inteiro
      cargo:inteiro
      salario:real
      mes:inteiro
      salario_anual:real
   fimregistro
Var
   //Variaveis SETOR RH
   A:vetor[1..99] de cad_funcionarios // o vetor pode ou não ser totalmente preenchido
   cpf,irpf:real
   resp,opcao:caractere
   nm_funcionario:caractere
   i,c,j,k,m:inteiro
   por,irpfpor:caractere
   situacao,situacao2,situacao3,situacao4:logico
   salario_liquido,desconto,horas_de_trabalho:real
   troca:caractere
   continuar:caractere
   salario_liquido_anual:real
   //fim
   //Procedimento simples para o MENU do setor de RH
procedimento menu ()
inicio
   limpatela
   escreval("===========================")
   escreval("           MENU            ")
   escreval("===========================")
   escreval("1 - Cadastrar funcionários.")
   escreval("2 - Relatório do funcionários.")
   escreval("3 - Calcular salário do funcionário.")
   escreval("4 - Exibir contra-cheque do funcionário.")
   escreval("5 - Voltar ao MENU PRINCIPAL")
   escreval("6 - Calculando")
   escreval("7 - Sair.")
   escreva("Escolha uma opção: ")
   leia(opcao)
fimprocedimento
//Essa função, altera o cargo de inteiro para caractere
funcao trocando_cargos (v1:inteiro):caractere
var
   cargo:caractere
inicio
   escolha (v1)
   caso 1
      cargo <- "Operário"
   caso 2
      cargo <- "Supervisor"
   caso 3
      cargo <- "Gerente"
   caso 4
      cargo <- "Diretor"
   fimescolha
   retorne cargo
fimfuncao
//Função para calcular o salário dos funcionários
funcao calculando_salario (v1:real):real
var    //Varíaveis locais
   hs_extra,sal_temp,sal_extra,salario:real
   kms:inteiro
inicio
   escolha (A[j].cargo)
      //A carga horaria mensal é definida em 192hs, qualquer hora a mais passa a ser extra
   caso 1
      se (horas_de_trabalho > 192) entao
         hs_extra <- horas_de_trabalho - 192
         horas_de_trabalho <- 192
         sal_temp <- horas_de_trabalho * 15
         sal_extra <- hs_extra * 30
         A[j].salario <- sal_extra + sal_temp
      senao
         A[j].salario <- horas_de_trabalho * 15
      fimse

   caso 2
      se (horas_de_trabalho > 192) entao
         hs_extra <- horas_de_trabalho - 192
         horas_de_trabalho <- 192
         sal_temp <- horas_de_trabalho * 40
         sal_extra <- hs_extra * 80
         A[j].salario <- sal_extra + sal_temp
      senao
         A[j].salario <- horas_de_trabalho * 40
      fimse
      //Gerente e diretor, não tem direito a horas extras.
   caso 3
      A[j].salario <- horas_de_trabalho * 60

   caso 4
      A[j].salario <- horas_de_trabalho * 80
   fimescolha

   retorne A[j].salario
fimfuncao
//Cadastro de funcionários
procedimento cadastro_funcionarios ()
inicio
   repita //Preferencia ao repita, pois não vamos sabemos quantos funcionários serão cadastrados.
      limpatela
      i <- i + 1 // Toda vez que um funcionário for cadastrado a variavel i sempre recebendo 1
      escreval("================================")
      escreval("Cadastrando o",i,"° funcionário.")
      escreval("================================")
      escreval("")
      escreva("Insira o NOME do FUNCIONÁRIO: ")
      leia(A[i].nome_funcio)

      repita //Loop caso o CPF já tenha sido registrado.
         escreva("Insira o CPF do funcionário: ")
         leia(cpf)
         //Verificando se já existe algum cpf igual no registro
         para c de 1 ate i faca
            se (cpf = A[c].cpf) entao
               k <- c
            fimse
         fimpara
         //Caso o K seja igual a 0 daria erro, atribuindo um valor a ele maior que o vetor.
         se (k = 0) entao
            k <- i + 1
         fimse
         //Se o cpf já tiver cadastrado o usuário deverá digitar um diferente
         se (cpf = A[k].cpf) entao
            escreval("CPF JÁ CADASTRADO")
            resp <- "N"
         senao
            resp <- "S"
         fimse
      ate (resp = "S")
      A[i].cpf <- cpf //Atribuindo o CPF ao registro CPF
      //Fim CPF.

      escreva("Insira o RG do funcionário: ")
      leia(A[i].rg)

      escreva("Insira o ENDEREÇO do funcionário: ")
      leia(A[i].end_funcio)

      escreva("Insira o TELEFONE do funcionário: ")
      leia(A[i].telefone)

      escreva("Insira a QUANTIDADE de FILHOS do funcionário: ")
      leia(A[i].qnt_filhos)

      repita //Loop caso o usuário forneça um cargo inexistente
         escreval("Escolha o cargo do funcionário de 1 a 4:")
         escreval("1 - Operário.")
         escreval("2 - Supervisor.")
         escreval("3 - Gerente.")
         escreval("4 - Diretor.")
         leia(A[i].cargo)
         se (A[i].cargo >= 1) e (A[i].cargo <= 4) entao //Controle de dados
            escreval("Funcionário cadastrado.")
            resp <- "s"
         senao
            escreval("Cargo Inexistente.")
            resp <- "n"
         fimse
      ate (resp = "s")
      //Fim cargo.

      situacao <- verdadeiro //Situação automaticamente passa a ser verdadeira, quando um funcionário é cadastrado
      escreval("")
      escreva("Continuar cadastrando? [S/N]")
      leia(resp)
      ordenamento //Sempre que um funcionário é cadastrado, é realizado um ordenamento por ordem alfabética.
      escreval("")
   ate (resp = "n")
fimprocedimento

procedimento ordenamento () //Procedimento para ordenação
var  //Variaveis locais
   k,aux_cargo,aux_qnt_filhos,mes:inteiro
   aux,aux_r,aux_end,aux_telefone:caractere
   aux_cpf,aux_rg,aux_salario,aux_salario_anual:real
inicio
   //Ordenando os nomes em ordem alfabetica
   para k de 1 ate i-1 faca
      para j de k+1 ate i faca
         se (A[k].nome_funcio > A[j].nome_funcio) entao
            aux <- A[k].nome_funcio
            A[k].nome_funcio <- A[j].nome_funcio
            A[j].nome_funcio  <- aux

            //Ordenando todas as outras informações
            aux_cpf <- A[k].cpf
            A[k].cpf <- A[j].cpf
            A[j].cpf <- aux_cpf

            aux_rg <- A[k].rg
            A[k].rg <- A[j].rg
            A[j].rg <- aux_rg

            aux_cargo <- A[k].cargo
            A[k].cargo <- A[j].cargo
            A[j].cargo <- aux_cargo

            aux_end <- A[k].end_funcio
            A[k].end_funcio <- A[j].end_funcio
            A[j].end_funcio <- aux_end

            aux_telefone <- A[k].telefone
            A[k].telefone <- A[j].telefone
            A[j].telefone <- aux_telefone

            aux_qnt_filhos <- A[k].qnt_filhos
            A[k].qnt_filhos <- A[j].qnt_filhos
            A[j].qnt_filhos <- aux_qnt_filhos

            aux_salario <- A[k].salario
            A[k].salario <- A[j].salario
            A[j].salario <- aux_salario
            
            aux_salario_anual <- A[k].salario_anual
            A[k].salario_anual <- A[j].salario
            A[j].salario_anual <- aux_salario_anual
            
            aux_mes <- A[k].mes
            A[k].mes <- A[j].salario
            A[j].mes <- aux_mes
         fimse
      fimpara
   fimpara
fimprocedimento

procedimento pergunta () //Como existem vários limpatela, procedimento para exibir as informações na tela do usuário até ele apertar qualquer tecla.
inicio
   escreval("=====================")
   escreval("Aperte qualquer tecla")
   escreval("=====================")
   leia(continuar)
fimprocedimento

procedimento contracheque ()
inicio
   //Desconto de 8% em salarios 1,000 a 2,500
   se (A[j].salario >= 1000) e (A[j].salario <= 2500) entao
      desconto <- A[j].salario * 0.08
      por <- "8%"

      //Desconto de 10% em salarios 2.500 a 4000
   senao
      se (A[j].salario > 2500) e (A[j].salario <= 4000) entao
         desconto <- A[j].salario * 0.10
         por <- "10%"

         //Desconto de 12% em salarios 4.000 a 5.550
      senao
         se (A[j].salario > 4000) e (A[j].salario <= 5500) entao
            desconto <- A[j].salario * 0.12
            por <- "12%"

            //Desconto de 15% em salarios acima de 5.500
         senao
            se (A[j].salario > 5500) entao
               desconto <- A[j].salario * 0.15
               por <- "15%"
            fimse
         fimse
      fimse
   fimse

   //================== IRPF ===================
   //IRPF Desconto de 7,5% me salários de 1.903,98 a 2.826,65
   se (A[j].salario > 1903.98) e (A[j].salario < 2826.65) entao
      irpf <- A[j].salario * 0.075
      irpfpor <- "7,5%"
      //IRPF Desconto de 15% me salários de 2.826,66 a 3.751,05
   senao
      se (A[j].salario > 2826.66) e (A[j].salario < 3751.05) entao
         irpf <- A[j].salario * 0.15
         irpfpor <- "15%"
         //IRPF Desconto de 22% me salários de 3.751,06
      senao
         se (A[j].salario > 3751.06) entao
            irpf <- A[j].salario * 0.22
            irpfpor <- "22%"
         fimse
      fimse
   fimse

   //Calculando salário liquido
   salario_liquido <- A[j].salario - desconto - irpf

fimprocedimento
//Procedimento para verificar se o nome digitado pelo usuário existe, ou caso exista nomes no registro iguais
procedimento verificador_funcionarios ()
inicio
   m <- 0
   situacao3 <- falso  //Essas duas situações servem de controle no caso 3
   situacao4 <- falso

   para c de 1 ate i faca   //O para é de 1 ate i, o i representa a quantidade de funcionários cadastrados
      se (nm_funcionario = A[c].nome_funcio) entao //O loop verifica em todo o vetor, se existe algum registro com o nome igual ao digitado pelo usuário.
         j <- c  // J recebe a posição de c, assim sabemos em qual posição ta cadastrado o funcionário
         m <- m + 1 // M recebe 1 toda vez que essa condição for atendida, dessa forma sabemos se existe mais de um funcionário com o mesmo nome
      fimse
   fimpara

   se (j = 0) entao //Essa condição é iniciada caso o usuário tenha digitado um nome que não existe no registro, entao o j passa a receber um valor maior que o vetor, pois ocorreria erro caso fosse 0
      j <- i + 1
   fimse

   se (nm_funcionario = A[j].nome_funcio) entao //Essa condição foi feita para deixar claro se existe ou não um funcionário com o nome digitado pelo usuário.
      situacao3 <- verdadeiro  //Se existir, a situacao3 passa a ser verdadeira, ela serve de controle no caso 3
   senao
      escreval("Funcionário não localizado.") //Caso não exista, é exibido esta mensagem
      pergunta
   fimse

   se (m = 1) entao //No caso 3, é necessário que a situacao3 e 4 sejam verdadeiras, para entrar
      situacao4 <- verdadeiro
   fimse

   se (m > 1) entao //Aqui, caso o nome digitado pelo usuário aparece mais de uma vez no registro, entramos nessa condição
      escreval("Foram encontrados mais de um funcionário com esse nome.")
      escreva("Digite o CPF: ")
      leia(cpf) //A forma de validar os funcionários passa a ser pelo CPF

      para c de 1 ate i faca //O loop aqui faz a mesma coisa, percorre todo o registro e verificar se tem cpf no registro igual ao digitado pelo usuário.
         se (cpf = A[c].cpf) entao
            j <- c //Se isso acontecer o j recebe a posição
         fimse
      fimpara

      se (j = 0) entao
         j <- i + 1
      fimse

      se (cpf = A[j].cpf) entao //Essa condição é feita para verificar se o CPF digitado realmente existe
         situacao4 <- verdadeiro //Existindo a variavel situacao4 passa a ser verdadeira, ela serve de controle no caso 3
      senao
         escreval("CPF não coincide com o nome do funcionário.")
         pergunta
      fimse
   fimse
fimprocedimento

Inicio
   //Começo do código do RH
   repita
      menu
      escolha(opcao)
      caso "1"
         cadastro_funcionarios

      caso "2"
         se (situacao = falso) entao  // Se não existir nenhum funcionário cadastrado, não é possível usar o caso 2
            escreval("Necessário cadastrar pelo menos um funcionário")
            pergunta
         senao
            ordenamento
            limpatela
            escreval("========================")
            escreval("Listagem de funcionários")
            escreval("========================")
            para c de 1 ate i faca   // O i é a quantidade de funcionário cadastrados.
               escreval(c,"° FUNCIONÁRIO.")
               escreval("NOME: ",A[c].nome_funcio)
               escreval("CPF:",A[c].cpf)
               escreval("RG:",A[c].rg)
               escreval("Endereço: ",A[c].end_funcio)
               escreval("Telefone: ",A[c].telefone)
               escreval("Quantidade de filhos:",A[c].qnt_filhos)
               troca <- trocando_cargos (A[c].cargo) //Função de inteiro para caractere
               escreval("Cargo: ",troca)
               se (A[c].salario <> 0) entao     //Salario só será exibido, caso tenha
                  escreval("Salario:",A[c].salario)
               fimse
               se (A[c].salario <= 1903.98) entao
                  escreval("Não paga IMPOSTO DE RENDA")
               senao
                  escreval("Paga IMPOSTO DE RENDA")
               fimse
               escreval("")
            fimpara
            pergunta
         fimse

      caso "3"
         se (situacao = falso) entao // Se não existir nenhum funcionário cadastrado, não é possível usar o caso 3
            escreval("Necessário cadastrar pelo menos um funcionário")
            pergunta
         senao
            repita
               limpatela
               escreval("========================")
               escreval("   Calculando salário   ")
               escreval("========================")
               escreva("Informe o nome do funcionário: ")
               leia(nm_funcionario)
               verificador_funcionarios  //Puxa o procedimento para fazer as verificações.
               se (situacao3 = verdadeiro) e (situacao4 = verdadeiro) entao  //A situacao 3 e 4, servem de controle, c
                  se (nm_funcionario = A[j].nome_funcio) ou (cpf = A[j].cpf) entao //Se o nome digitado tiver sido registrado
                     troca <- trocando_cargos (A[j].cargo) //Exibindo o cargo do funcionário, usando a Função que troca cargo
                     escreval("Cargo: ",troca)
                     escreva("Quantas horas foram trabalhadas? ")  //Calculando salário
                     leia(horas_de_trabalho)
                     escreval("")
                     A[j].salario <- calculando_salario (horas_de_trabalho) //Puxamos a função para calcular o salário, horas extras e etc...
                     escreval("O funcionário ",A[j].nome_funcio," trabalhou",horas_de_trabalho," horas, portanto seu salário é de R$",A[j].salario)
                     //Esse calculo só funcionará caso seja calculado 12x o salário
                     A[j].mes <- A[j].mes + 1 // Toda vez que o salário do funcionário for calculado, esse registro recebe 1
                     A[j].salario_anual <- A[j].salario_anual + A[j].salario //
                     contracheque
                     //A intenção dele é saber quanto o funcionário pagará de IRPF, anualmente.
                     situacao2 <- verdadeiro //Situação 2 passa a ser verdadeiro.
                     escreva("Continuar calculando salários? [S/N]")
                     leia(resp)
                  fimse
               fimse
            ate (resp = "N")

         fimse

      caso "4"
         se (situacao = falso) ou (situacao2 = falso) entao //Além de ter cadastrado um funcionário, é necessário ter calculado o salário, para poder usar esse caso.
            escreval("")
            escreval("Necessário cadastrar pelo menos um funcionário")
            escreval("E ter calculado o salário.")
            pergunta
         senao
            escreval("=====================")
            escreval("Salário LIQUIDO")
            escreval("=====================")
            escreval("")
            escreva("Informe o nome do funcionário: ")
            leia(nm_funcionario)

            verificador_funcionarios

            se (situacao3 = verdadeiro) e (situacao4 = verdadeiro) entao
               se (nm_funcionario = A[j].nome_funcio) ou (cpf = A[j].cpf) entao
                  contracheque
                  escreval("Salário BRUTO R$",A[j].salario)
                  escreval("Desconto de ",por," INSS R$",desconto)
                  escreval("Desconto de ",irpfpor," IRPF R$",irpf)
                  escreval("Salário LÍQUIDO R$",salario_liquido)
               fimse
               pergunta
            fimse
         fimse

      caso "6"
         se (situacao = falso) ou (situacao2 = falso) entao //Além de ter cadastrado um funcionário, é necessário ter calculado o salário, para poder usar esse caso.
            escreval("")
            escreval("Necessário cadastrar pelo menos um funcionário")
            escreval("E ter calculado o salário.")
            pergunta
         senao
            limpatela
            escreval("===================")
            escreval("Salários calculados")
            escreval("===================")
            escreva("Informe o nome do funcionário: ")
            leia(nm_funcionario)
            verificador_funcionarios
            escreval("Já foram calculados ",A[j].mes," SALÁRIOS")
            escreval("Totalizando o salário BRUTO de R$",A[j].salario_anual)
            pergunta
         fimse

      caso "7"
         escreval("Saindo do programa...")
         opcao <- "l"
      outrocaso
         limpatela
         escreval("Opção invalida.")
         pergunta
      fimescolha
   ate (opcao = "l")
Fimalgoritmo
