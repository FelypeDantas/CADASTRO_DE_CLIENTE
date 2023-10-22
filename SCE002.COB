       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE002.
      *>AUTHOR. FELYPE DANTAS DOS SANTOS.
      *>*****************************************
      *> MANUTENÇÃO DO CADASTRO DE CLIENTES *
      *>*****************************************
      *>------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQCLI ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CPF-CNPJ
           FILE STATUS IS ST-ERRO
           ALTERNATE RECORD KEY IS APELIDO WITH DUPLICATES.
      *>---------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQCLI
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "ARQCLI.DAT".
       01 REGCLI.
           03 CPF-CNPJ       PIC 9(11).
           03 RAZAO-SOCIAL   PIC X(60).
           03 APELIDO        PIC X(30).
           03 CEP            PIC 9(08).
           03 LOGRADOURO     PIC X(30).
           03 NUMERO         PIC 9(03).
           03 BAIRRO         PIC X(30).
           03 CIDADE         PIC X(30).
           03 UF             PIC X(02).
           03 TELEFONE       PIC 9(11).
           03 EMAIL          PIC X(30).
           03 CONTATO        PIC X(15).
      *>------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL          PIC 9(01) VALUE ZEROS.
       77 W-CONT         PIC 9(06) VALUE ZEROS.
       77 W-OPCAO        PIC X(01) VALUE SPACES.
       77 ST-ERRO        PIC X(02) VALUE "00".
       77 W-ACT          PIC 9(02) VALUE ZEROS.
       77 MENS           PIC X(50) VALUE SPACES.
       77 LIMPA          PIC X(50) VALUE SPACES.

       01 TXUF.
            03 UF-SIGLA     PIC X(02) VALUE SPACES.
            03 UF-TEXTO     PIC X(20) VALUE SPACES.

       01 TBUF.
             03 FILLER     PIC X(22) VALUE "ACACRE".
             03 FILLER     PIC X(22) VALUE "ALALAGOAS".
             03 FILLER     PIC X(22) VALUE "AMAMAZONAS".
             03 FILLER     PIC X(22) VALUE "APAMAPA".
             03 FILLER     PIC X(22) VALUE "BABAHIA".
             03 FILLER     PIC X(22) VALUE "CECEARA".
             03 FILLER     PIC X(22) VALUE "DFDISTRITO FEDERAL".
             03 FILLER     PIC X(22) VALUE "ESESPIRITO SANTO".
             03 FILLER     PIC X(22) VALUE "GOGOIAS".
             03 FILLER     PIC X(22) VALUE "MAMARANHÃO".
             03 FILLER     PIC X(22) VALUE "MTMATO GROSSO".
             03 FILLER     PIC X(22) VALUE "MSMATO GROSSO DO SUL".
             03 FILLER     PIC X(22) VALUE "MGMINAS GERAIS".
             03 FILLER     PIC X(22) VALUE "PAPARA".
             03 FILLER     PIC X(22) VALUE "PBPARAIBA".
             03 FILLER     PIC X(22) VALUE "PRPARANÁ".
             03 FILLER     PIC X(22) VALUE "PEPERNAMBUCO".
             03 FILLER     PIC X(22) VALUE "PIPIAUI".
             03 FILLER     PIC X(22) VALUE "RJRIO DE JANEIRO".
             03 FILLER     PIC X(22) VALUE "RNR. GRANDE DO NORTE".
             03 FILLER     PIC X(22) VALUE "RSRIO GRANDE DO SUL".
             03 FILLER     PIC X(22) VALUE "RORONDONIA".
             03 FILLER     PIC X(22) VALUE "RRRORAIMA".
             03 FILLER     PIC X(22) VALUE "SCSANTA CATARINA".
             03 FILLER     PIC X(22) VALUE "SPSAO PAULO".
             03 FILLER     PIC X(22) VALUE "SESERGIPE".
             03 FILLER     PIC X(22) VALUE "TOTOCANTINS".

       01 TBUFR REDEFINES TBUF.
             03 TABUF    PIC X(22) OCCURS 27 TIMES.
       01 IND            PIC 9(02) VALUE ZEROS.
      *>-------------------------------------------------------------------
       SCREEN SECTION.
       01  SMT006.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01
               VALUE  "                               CADASTRO".
           05  LINE 03  COLUMN 41
               VALUE  "DE CLIENTES".
           05  LINE 08  COLUMN 01
               VALUE "CPF-CNPJ    : ".
           05  LINE 10  COLUMN 01
               VALUE "RAZAO SOCIAL: ".
           05  LINE 12  COLUMN 01
               VALUE "APELIDO     : ".
           05  LINE 14  COLUMN 01
               VALUE "CEP         : ".
           05  LINE 14  COLUMN 40
               VALUE "LOGRADOURO  : ".
           05  LINE 16  COLUMN 01
               VALUE "NUMERO      : ".
           05  LINE 16  COLUMN 40
               VALUE "BAIRRO      : ".
           05  LINE 18  COLUMN 01
               VALUE "CIDADE      : ".
           05  LINE 18  COLUMN 40
               VALUE "UF          : ".
           05  LINE 20  COLUMN 01
               VALUE "TELEFONE    : ".
           05  LINE 20  COLUMN 40
               VALUE "CONTATO     : ".
           05  LINE 22  COLUMN 01
               VALUE "EMAIL       : ".
           05  LINE 24  COLUMN 01
               VALUE "MENSAGEM    : ".
           05  TCPF-CNPJ
               LINE 08  COLUMN 15 PIC 999.999.999.99
               USING CPF-CNPJ.
           05  TRAZAO-SOCIAL
               LINE 10  COLUMN 15 PIC X(60)
               USING RAZAO-SOCIAL.
           05  TAPELIDO
               LINE 12  COLUMN 15 PIC X(30)
               USING APELIDO.
           05  TCEP
               LINE 14  COLUMN 15 PIC 99999.999
               USING CEP.
           05  TNUMERO
               LINE 16  COLUMN 15 PIC 9(03)
               USING NUMERO.
           05  TUF
               LINE 18  COLUMN 55 PIC X(02)
               USING UF.
           05  TTELEFONE
               LINE 20  COLUMN 15 PIC 9(11)
               USING TELEFONE.
           05  TCONTATO
               LINE 20  COLUMN 55 PIC X(15)
               USING CONTATO.
           05  TEMAIL
               LINE 22  COLUMN 15 PIC X(30)
               USING EMAIL.
      *>-------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       INC-OP0.
           OPEN I-O ARQCLI
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ARQCLI
                      CLOSE ARQCLI
                      MOVE "* ARQUIVO CLIENTE SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                   IF ST-ERRO = "30"
                       OPEN OUTPUT ARQCLI
                       MOVE "ISAM NAO CARREGADO " TO MENS
                       PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CLIENTE" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       INC-001.


                MOVE SPACES TO
                RAZAO-SOCIAL APELIDO LOGRADOURO BAIRRO CIDADE UF
                EMAIL CONTATO UF-SIGLA.

                MOVE ZEROS  TO CPF-CNPJ CEP TELEFONE NUMERO .
                DISPLAY SMT006.

       INC-002.
                ACCEPT TCPF-CNPJ
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE ARQCLI
                   GO TO ROT-FIM.
                IF CPF-CNPJ = SPACES
                   MOVE "*** CLIENTE INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CEP01.
                MOVE 0 TO W-SEL
                READ ARQCLI
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-007A THRU INC-007B
                      DISPLAY SMT006
                      MOVE "*** CLIENTE JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. CLIENTE"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TRAZAO-SOCIAL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF RAZAO-SOCIAL = SPACES
                   MOVE "RAZAO. DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.

       INC-004.
                ACCEPT TAPELIDO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF APELIDO = SPACES
                   MOVE "BAIRRO DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
       INC-005.
                ACCEPT TCEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF CEP = SPACES
                   MOVE "CEP DEVE SER DIFERENTE DE BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.

       INC-006.
               ACCEPT TNUMERO
               ACCEPT W-ACT FROM ESCAPE KEY
               IF W-ACT = 02 GO TO INC-005.
               IF NUMERO = SPACES
                   MOVE "NUMERO DEVE SER DIFERENTE DE BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-006.

       INC-007.
                ACCEPT TUF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-007A.
               MOVE 1 TO IND.
       INC-007B.
               MOVE TABUF (IND) TO TXUF
               IF UF NOT = UF-SIGLA
                      ADD 1 TO IND
                      IF IND > 27
                         MOVE "UF NAO EXISTE" TO MENS
                         PERFORM ROT-MENS THRU ROT-MENS-FIM
                         GO TO INC-007
                      ELSE
                         GO TO INC-007B
               ELSE
                     DISPLAY UF-TEXTO.

       INC-008.
               ACCEPT TTELEFONE
               ACCEPT W-ACT FROM ESCAPE KEY
               IF W-ACT = 02 GO TO INC-005.
               IF TELEFONE = SPACES
                   MOVE "TELEFONE DEVE SER DIFERENTE DE BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-007.

       INC-009.
               ACCEPT TCONTATO
               ACCEPT W-ACT FROM ESCAPE KEY
               IF W-ACT = 02 GO TO INC-005.
               IF CONTATO = SPACES
                   MOVE "CONTATO DEVE SER DIFERENTE DE BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-008.

       INC-010.
               ACCEPT TEMAIL
               ACCEPT W-ACT FROM ESCAPE KEY
               IF W-ACT = 02 GO TO INC-005.
               IF EMAIL = SPACES
                   MOVE "CONTATO DEVE SER DIFERENTE DE BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-009.

                   IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (24, 40) "DADOS OK (S/N) : ".
                ACCEPT (24, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** NOTA JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                   MOVE "ERRO NA GRAVACAO DO ARQUIVO DE NOTAS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.

      *>****************************************
      *> ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *>****************************************

       ACE-001.
                DISPLAY (24, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (24, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (24, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.

       EXC-OPC.
                DISPLAY (24, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (24, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQCLI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ALT-OPC.
                DISPLAY (24, 40) "ALTERAR  (S/N) : ".
                ACCEPT (24, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA ALTERACAO DO REGISTRO NOTAS"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *>*********************
      *> ROTINA DE FIM      *
      *>*********************
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.

      *>*********************
      *> ROTINA DE MENSAGEM *
      *>*********************

       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (24, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (24, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

           STOP RUN.
