Algoritmo "semnome"
//Registro RH
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
   fimregistro
   //Fim registro RH
Var
   resposta,voltar:caractere

   //Variaveis SETOR RH
   A:vetor[1..3] de cad_funcionarios
   i,c,j:inteiro
   resp,opcao:caractere
   nm_funcionario:caractere
   horas_de_trabalho:real
   salario_liquido,desconto:real
   troca_cargos:caractere
   continuar:caractere
   situacao,situacao2:logico
   //Fim VAR RH

procedimento menu_principal ()
inicio
   limpatela
   escreval("================================")
   escreval("         MENU PRINCIPAL         ")
   escreval("================================")
   escreval("1 - SETOR OPERACIONAL.")
   escreval("2 - SETOR FINANCEIRO.")
   escreval("3 - SETOR DE RH.")
   escreval("4 - Encerrar PROGRAMA.")
   leia(resposta)
fimprocedimento


//Código do RH
//Procedimento simples para o MENU do setor de RH
procedimento menu ()
inicio

   limpatela
   escreval("============================")
   escreval("          MENU RH           ")
   escreval("============================")
   escreval("1 - Cadastrar funcionários.")
   escreval("2 - Listar funcionários.")
   escreval("3 - Calcular salário de funcionário.")
   escreval("4 - Relatório de funcionários.")
   escreval("5 - Calcular salário liquido do funcionário.")
   escreval("7 - Sair.")
   escreval("8 - Retornar ao MENU PRINCIPAL.")
   escreva("Escolha uma opção: ")
   leia(opcao)

fimprocedimento

//Transformando a varivavel cargo de inteira em caractere
funcao trocando_cargos (v1:inteiro):caractere
var
   carggoos:caractere
inicio
   se (v1 = 1) entao
      carggoos <- "Operário"
   senao
      se (v1 = 2) entao
         carggoos <- "Supervisor"
      senao
         se (v1 = 3) entao
            carggoos <- "Gerente"

         senao
            se (v1 = 4) entao
               carggoos <- "Diretor"
            fimse
         fimse
      fimse
   fimse
   retorne carggoos
fimfuncao
//Função para calcular o salário do funcionário
funcao calculando_salario (v1:real):real
var
   hs_extra,sal_temp,sal_extra,salario:real
inicio

   se (A[j].cargo = 1) entao
      se (horas_de_trabalho > 160) entao
         hs_extra <- horas_de_trabalho - 160
         horas_de_trabalho <- 160
         sal_temp <- horas_de_trabalho * 15
         sal_extra <- hs_extra * 30
         A[j].salario <- sal_extra + sal_temp
      senao
         A[j].salario <- horas_de_trabalho * 15
      fimse
   senao
      se (A[j].cargo = 2) entao
         se (horas_de_trabalho > 160) entao
            hs_extra <- horas_de_trabalho - 160
            horas_de_trabalho <- 160
            sal_temp <- horas_de_trabalho * 40
            sal_extra <- hs_extra * 80
            A[j].salario <- sal_extra + sal_temp
         senao
            A[j].salario <- horas_de_trabalho * 40
         fimse
      senao
         se (A[j].cargo = 3) entao
            se (horas_de_trabalho > 160) entao
               escreval("Trabalhu ploque quiz")
               A[j].salario <- horas_de_trabalho * 60
            senao
               A[j].salario <- horas_de_trabalho * 60
            fimse
         senao
            se (A[j].cargo = 4) entao
               escreval("Tralhu ploque quiz")
               A[j].salario <- horas_de_trabalho * 80
            senao
               A[j].salario <- horas_de_trabalho * 80
            fimse
         fimse
      fimse
   fimse
   retorne A[j].salario
fimfuncao


//Cadastro de funcionários
procedimento cadastro_funcionarios ()
inicio
   //Preferencia do repita ao PARA, pois não sabemos quantos
   //Funcionários serão cadastrados.
   repita
      limpatela
      i <- i + 1   //Variavel i sempre recebendo 1 a cada funcionário cadastrado

      escreval("================================")
      escreval("Cadastrando o",i,"° funcionário.")
      escreval("================================")
      escreval("")

      situacao <- verdadeiro

      escreva("Insira o NOME do FUNCIONÁRIO: ")
      leia(A[i].nome_funcio)

      escreva("Insira o CPF do funcionário: ")
      leia(A[i].cpf)

      escreva("Insira o RG do funcionário: ")
      leia(A[i].rg)

      escreva("Insira o ENDEREÇO do funcionário: ")
      leia(A[i].end_funcio)

      escreva("Insira o TELEFONE do funcionário: ")
      leia(A[i].telefone)

      escreva("Insira a QUANTIDADE de FILHOS do funcionário: ")
      leia(A[i].qnt_filhos)

      repita
         escreval("Escolha o cargo do funcionário de 1 a 4:")
         escreval("1 - Operário.")
         escreval("2 - Supervisor.")
         escreval("3 - Gerente.")
         escreval("4 - Diretor.")
         leia(A[i].cargo)

         //Controle de dados, caso o usuário digite um
         //Cargo menor que 1 ou maior que 4, o loop é iniciado
         //Até ser preenchido o cargo correto
         se (A[i].cargo >= 1) e (A[i].cargo <= 4) entao
            escreval("Funcionário cadastrado.")
            resp <- "s"
         senao
            escreval("Cargo Inexistente.")
            resp <- "n"
         fimse
      ate (resp = "s")

      escreval("")
      escreva("Continuar cadastrando? [S/N]")
      leia(continuar)
      ordenamento
      escreval("")

   ate (continuar = "N")

fimprocedimento

procedimento ordenamento ()
var
   k,aux_cargo,aux_qnt_filhos:inteiro
   aux,aux_r,aux_end,aux_telefone:caractere
   aux_cpf,aux_rg:real
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

         fimse
      fimpara
   fimpara

fimprocedimento
//FimRH
Inicio
   repita
      menu_principal ()
      escolha (resposta) //onde começa o escolha da  menu principal
      caso "1"
         escreval("SETOR OPERACIONAL.")
         escreval("INSIRA SEU CÓDIGO AQUI")
         escreval("=====================")
         escreval("Aperte qualquer tecla")
         escreval("=====================")
         leia(resp)

      caso "2"
         escreval("SETOR FINANCEIRO")
         escreval("INSIRA SEU CÓDIGO AQUI")
         escreval("=====================")
         escreval("Aperte qualquer tecla")
         escreval("=====================")
         leia(resp)

      caso "3"
         repita
            menu
            escolha(opcao)

            caso "1"
               cadastro_funcionarios

            caso "2"
               se (situacao = falso) entao
                  escreval("Necessário cadastrar pelo menos um funcionário")
                  escreval("=====================")
                  escreval("Aperte qualquer tecla")
                  escreval("=====================")
                  leia(resp)
               senao
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
                     troca_cargos <- trocando_cargos (A[c].cargo) //Função de inteiro para caractere
                     escreval("Cargo:",troca_cargos)
                     se (A[c].salario <> 0) entao     //Salario só será exibido, caso tenha sido realizado algum calculo
                        escreval("Salario:",A[c].salario)
                     fimse
                     escreval("")
                  fimpara

                  escreval("=====================")
                  escreval("Aperte qualquer tecla")
                  escreval("=====================")
                  leia(resp)
               fimse

            caso "3"
               se (situacao = falso) entao
                  escreval("Necessário cadastrar pelo menos um funcionário")
                  escreval("=====================")
                  escreval("Aperte qualquer tecla")
                  escreval("=====================")
                  leia(resp)
               senao
                  repita
                     escreval("Digite o nome do funcionário: ")
                     leia(nm_funcionario)

                     //Loop para validar se o funcionário é cadastrado
                     //Caso seja, a posição dele é apontada na variavel J
                     para c de 1 ate i faca
                        se (nm_funcionario = A[c].nome_funcio) entao
                           j <- c
                        fimse
                     fimpara

                     //Caso não encontre o funcionário, essa condicional
                     //É iniciada, e o J não fica vázio, recebendo um valor
                     //Maior que o número de funcionários registrados
                     se (j = 0) entao
                        j <- i + 1
                     fimse

                     se (A[j].nome_funcio = nm_funcionario) entao
                        //Função de inteiro para caractere
                        troca_cargos <- trocando_cargos (A[j].cargo)
                        escreval("Cargo: ",troca_cargos)
                        //Calculando salário
                        escreval("Quantas horas foram trabalhadas? ")
                        leia(horas_de_trabalho)
                        escreval("")
                        A[j].salario <- calculando_salario (horas_de_trabalho)
                        situacao2 <- verdadeiro
                        escreval("O funcionário ",A[j].nome_funcio," trabalhou",horas_de_trabalho," horas, portanto seu salário é de R$:",A[j].salario)
                        escreval("=====================")
                        escreval("Aperte qualquer tecla")
                        escreval("=====================")
                        leia(resp)
                        continuar <- "S"
                     senao
                        limpatela
                        escreval("Funcionário não localizado!")
                     fimse
                     escreval("")

                  ate (continuar = "S")
               fimse

            caso "4"
               se (situacao = falso) ou (situacao2 = falso) entao
                  escreval("")
                  escreval("Necessário cadastrar pelo menos um funcionário")
                  escreval("E ter calculado o salário.")
                  escreval("=====================")
                  escreval("Aperte qualquer tecla")
                  escreval("=====================")
                  leia(resp)
               senao
                  para c de 1 ate i faca
                     escreval(A[c].nome_funcio)
                     escreval(A[c].salario)
                     se (A[c].salario <= 1903.98) entao
                        escreval("Não paga IMPOSTO DE RENDA")
                     senao
                        escreval("Paga IMPOSTO DE RENDA")
                     fimse
                  fimpara

                  escreval("")
                  escreval("=====================")
                  escreval("Aperte qualquer tecla")
                  escreval("=====================")
                  leia(resp)
               fimse

            caso "5"
               se (situacao = falso) ou (situacao2 = falso) entao
                  escreval("")
                  escreval("Necessário cadastrar pelo menos um funcionário")
                  escreval("E ter calculado o salário.")
                  escreval("=====================")
                  escreval("Aperte qualquer tecla")
                  escreval("=====================")
                  leia(resp)
               senao
                  escreval("=====================")
                  escreval("Salário LIQUIDO")
                  escreval("=====================")
                  escreval("")
                  escreval("Informe o nome do funcionário:")
                  leia(nm_funcionario)

                  para c de 1 ate i faca
                     se (nm_funcionario = A[c].nome_funcio) entao
                        j <- c
                     fimse
                  fimpara

                  //Desconto de 8% em salarios 1,000 a 2,500
                  se (A[j].salario >= 1000) e (A[j].salario <= 2500) entao
                     escreval("Salário do funcionário tem DESCONTO de 8%")
                     desconto <- A[j].salario * 0.08
                     salario_liquido <- A[j].salario - desconto
                     escreval("Salário BRUTO R$:",A[j].salario)
                     escreval("Salário LÍQUIDO R$: ",salario_liquido)
                     escreval("Desconto de 8% R$:",desconto)
                  fimse

                  //Desconto de 10% em salarios 2.500 a 4000
                  se (A[j].salario > 2500) e (A[j].salario <= 4000) entao
                     escreval("Salário do funcionário tem DESCONTO de 10%")
                     desconto <- A[j].salario * 0.10
                     salario_liquido <- A[j].salario - desconto
                     escreval("Salário BRUTO R$:",A[j].salario)
                     escreval("Salário LÍQUIDO R$: ",salario_liquido)
                     escreval("Desconto de 10% R$:",desconto)
                  fimse

                  //Desconto de 12% em salarios 4.000 a 5.550
                  se (A[j].salario > 4000) e (A[j].salario <= 5500) entao
                     escreval("Salário do funcionário tem DESCONTO de 12%")
                     desconto <- A[j].salario * 0.12
                     salario_liquido <- A[j].salario - desconto
                     escreval("Salário BRUTO R$:",A[j].salario)
                     escreval("Salário LÍQUIDO R$: ",salario_liquido)
                     escreval("Desconto de 12% R$:",desconto)
                  fimse

                  //Desconto de 15% em salarios 2.500 a 4000
                  se (A[j].salario > 5000) entao
                     escreval("Salário do funcionário tem DESCONTO de 15%")
                     desconto <- A[j].salario * 0.15
                     salario_liquido <- A[j].salario - desconto
                     escreval("Salário BRUTO R$:",A[j].salario)
                     escreval("Salário LÍQUIDO R$: ",salario_liquido)
                     escreval("Desconto de 15% R$:",desconto)
                  fimse

                  escreval("=====================")
                  escreval("Aperte qualquer tecla")
                  escreval("=====================")
                  leia(resp)
               fimse

            caso "7"
               escreval("Saindo do programa...")
               interrompa
            caso "8"
               opcao <- "l"
            outrocaso
               limpatela
               escreval("Opção invalida.")
               escreval("=====================")
               escreval("Aperte qualquer tecla")
               escreval("=====================")
               leia(resp)

            fimescolha
         ate (opcao = "l")

      caso "4"
         escreval("Saindo do programa.")
         voltar <- "K"
      outrocaso
         escreval("Opção inválida.")
         escreval("=====================")
         escreval("Aperte qualquer tecla")
         escreval("=====================")
         leia(resp)
      fimescolha  //Término do escolha do MENU PRINCIPAL
   ate (voltar = "K")

Fimalgoritmo