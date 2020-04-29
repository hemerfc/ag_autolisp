
;*******************************************************************************
; CREATEPSLA
; Cria um bloco para a pista livre com as pediddas informadas
(defun ag:Psla (larg passo comp_ant comp_port comp_post point ang / rect blockname)

  (setq blockname (CreateBlkName
                    (list "PSLA"
                          (itoa larg)
                          (itoa passo)
                          (itoa comp_ant)
                          (itoa comp_port)
                          (itoa comp_post)
                    )
                  )
  )

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
      (MakeHatch rect 50)
      (DrawPoly rect 7)

      ;; RECT PORT ************************************************************
      (setq rect (list (list comp_ant 0)
                       (list comp_ant larg)
                       (list (+ comp_ant comp_port) larg)
                       (list (+ comp_ant comp_port) 0)
                 )
      )
      (MakeHatch rect 50)
      (DrawPoly rect 7)

      ;; RECT POST ************************************************************
      (setq rect (list (list (+ comp_ant comp_port) 0)
                       (list (+ comp_ant comp_port) larg)
                       (list (+ comp_ant comp_port comp_post) larg)
                       (list (+ comp_ant comp_port comp_post) 0)
                 )
      )
      (MakeHatch rect 50)
      (DrawPoly rect 7)

      ;; CENTRO ************************************************************
      (setq rect (list (list 0 0)
                       (list 0 (/ larg 2))
                       (list (+ comp_ant comp_port comp_post) (/ larg 2))
                       (list (+ comp_ant comp_port comp_post) 0)
                 )
      )
      (DrawPoly rect 7)
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

    ;; atualiza o ponto de ligação para a proxima esteira 
  (setq global:ag_join_point (polar point ang (+ comp_ant comp_port comp_post)))
)
;defun ag:Psla

;*******************************************************************************
;C:AG_PSLA
; commando para criar pistas plc
(defun c:ag_psla (/ n passo comp_ant comp_port comp_post point ang msg)
  (terpri)
  (print "Entre com os dados para PSLA")

  (setq passo (ag:getPasso))
  (setq n (ag:getN))
  (setq comp_ant (ag:getComp "CompAnt"))
  (setq comp_port (ag:getComp "CompPort"))
  (setq comp_post (ag:getComp "CompPost"))
  (setq point (ag:getInsertPoint))
  (setq ang (ag:getInsertAngle point))

    ; cria o bloco da PLA
  (ag:Psla n passo comp_ant comp_port comp_post point ang)

  (princ)
)
;defun c:ag_psla

(princ)
