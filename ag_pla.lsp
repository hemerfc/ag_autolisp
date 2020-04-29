
;*******************************************************************************
; CREATEPLA
; Cria um bloco para a pista livre com as pediddas informadas
(defun ag:Pla (n passo comp point ang / larg rect1 rect2 blockname)

  (setq blockname (CreateBlkName
                    (list "PLA"
                          (itoa n)
                          (itoa passo)
                          (itoa comp)
                    )
                  )
  )

  (setq larg (+ n 60))  ; largura = n + 60

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

      ;; cria o retangulo da esteira
      (setq rect1 (list (list 0 0)
                        (list 0 larg)
                        (list comp larg)
                        (list comp 0)
                  )
      )
      ;; cria o retangulo da centro da esteira
      (setq rect2 (list (list 0 0)
                        (list 0 (/ larg 2))
                        (list comp (/ larg 2))
                        (list comp 0)
                  )
      )
      (MakeHatch rect1 50)
      (DrawPoly rect1 7)
      (DrawPoly rect2 7)

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
  (setq global:ag_join_point (polar point ang comp))
)

;*******************************************************************************
;C:AG_PLA
; commando para criar pistas plc
(defun c:ag_pla (/ n passo comp point ang)
  (terpri)
  (print "Entre com os dados para PLA")

  (setq passo (ag:getPasso))
  (setq n (ag:getN))
  (setq comp (ag:getComp "Comp"))
  (setq point (ag:getInsertPoint))
  (setq ang (ag:getInsertAngle point))

  ; cria o bloco da PLA
  (ag:Pla n passo comp point ang)

  (princ)
)
;defun c:ag_pla

(princ)
