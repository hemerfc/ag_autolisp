
;*******************************************************************************
; AG:PSRM
; Cria um bloco para a pista
(defun ag:Psrm (n passo comp_ant comp_port comp_post vel cor point ang / larg rect blockname)

  (setq blockname (CreateBlkName
                    (list "PSRM"
                          (itoa n)
                          (itoa passo)
                          (itoa comp_ant)
                          (itoa comp_port)
                          (itoa comp_post)
                          vel 
                          cor
                    )
                  )
  )

  (if (eq "L" cor) (setq cor 21) (setq cor 101))
  (setq larg (+ n 97))  ; largura = n + 60

  ;; se nao exite um esteira com este nome
  ;; cria um bloco para ela
  (if (null (tblsearch "BLOCK" blockname))
    (progn
      (entmake (list '(0 . "BLOCK")         ; required
                     '(100 . "AcDbEntity")  ; recommended
                     '(100 . "AcDbBlockBegin")  ; recommended
                     (cons 2 blockname)         ; required
                     '(8 . "0")         ; recommended
                     '(70 . 0)          ; required [NOTE 0 if no attributes]
                     '(280 . 0)         ; disable exploding block
                     '(10 0.0 0.0 0.0)  ; required
               )
      )

      ;; RECT ANT ************************************************************
      (setq rect (list (list 0 0)
                       (list 0 larg)
                       (list comp_ant larg)
                       (list comp_ant 0)
                 )
      )
      (MakeHatch rect cor)
      (DrawPoly rect 7)

      ;; RECT PORT ************************************************************
      (setq rect (list (list comp_ant 0)
                       (list comp_ant larg)
                       (list (+ comp_ant comp_port) larg)
                       (list (+ comp_ant comp_port) 0)
                 )
      )
      ; inverte a cor      
      (if (eq 101 cor) (setq cor 21) (setq cor 101))
      (MakeHatch rect cor)
      (DrawPoly rect 7)

      ;; RECT POST ************************************************************
      (setq rect (list (list (+ comp_ant comp_port) 0)
                       (list (+ comp_ant comp_port) larg)
                       (list (+ comp_ant comp_port comp_post) larg)
                       (list (+ comp_ant comp_port comp_post) 0)
                 )
      )
      ; inverte a cor
      (if (eq 101 cor) (setq cor 21) (setq cor 101))
      (MakeHatch rect cor)
      (DrawPoly rect 7)

      ;; PROTECAO LATERAL ************************************************************      
      ;; cria o retangulo da lateral da esteira
      (setq rect (list (list 0 (+ n 30))
                        (list 0 (+ n 67))
                        (list (+ comp_ant comp_port comp_post) (+ n 67))
                        (list (+ comp_ant comp_port comp_post) (+ n 30))
                  )
      )
      (MakeHatch rect 7)
      ;; TRIANGULO ************************************************************
      (setq rect (list (list comp_ant 0)
                       (list comp_ant larg)
                       (list (+ comp_ant comp_port) (/ larg 2))
                 )
      )
      (DrawPoly rect 7)


      (entmake (list '(0 . "ENDBLK")    ; required
                     '(100 . "AcDbBlockEnd")  ; recommended
                     '(8 . "0")  ; recommended
               )
      )
    )
  )

  ;; insere o bloco no desenho
  (InsertBlk blockname point ang)

  ;; atualiza o ponto de liga��o para a proxima esteira 
  (setq global:ag_join_point (polar point ang (+ comp_ant comp_port comp_post)))
)

;*******************************************************************************
;C:AG_PSRM
; commando para criar pistas psrm
(defun c:ag_psrm (/ n passo comp_ant comp_port comp_post cor vel point point ang)
  (terpri)
  (print "Entre com os dados para PSRM")

  (setq passo (ag:getPasso))
  (setq n (ag:getN))
  (setq comp_ant (ag:getComp "CompAnt"))
  (setq comp_port (ag:getComp "CompPort"))
  (setq comp_post (ag:getComp "CompPost"))
  (setq vel (ag:getVel))
  (setq cor (ag:getCor))
  (setq point (ag:getInsertPoint))
  (setq ang (ag:getInsertAngle point))

  ; cria o bloco
  (ag:psrm n passo comp_ant comp_port comp_post vel cor point ang)
  
  (princ)
)
;defun c:ag_psrm

(princ)
