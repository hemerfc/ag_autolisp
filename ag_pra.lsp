
;*******************************************************************************
; CREATEPRA
; Cria um bloco para a pista livre com as pediddas informadas
(defun ag:pra (n passo comp vel cor point ang / rect1 rect2 blockname)

  (setq blockname (CreateBlkName
                    (list "PRA"
                          (itoa n)
                          (itoa passo)
                          (itoa comp)
                          vel
                          cor
                    )
                  )
  )

  (if (eq "L" cor) (setq cor 21) (setq cor 101))

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
                        (list 0 (+ n 97))
                        (list comp (+ n 97))
                        (list comp 0)
                  )
      )
      ;; cria o retangulo da lateral da esteira
      (setq rect2 (list (list 0 (+ n 30))
                        (list 0 (+ n 67))
                        (list comp (+ n 67))
                        (list comp (+ n 30))
                  )
      )
      (MakeHatch rect1 cor)
      (MakeHatch rect2 7)
      (DrawPoly rect1 7)

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
(defun c:ag_pra (/ tmp n passo comp vel cor point ang msg)
  (terpri)
  (print "Entre com os dados para PRA")

  (setq passo (ag:getPasso))
  (setq n (ag:getN))
  (setq comp (ag:getComp "Comp"))
  (setq vel (ag:getVel))
  (setq cor (ag:getCor))
  (setq point (ag:getInsertPoint))
  (setq ang (ag:getInsertAngle point))
  
  ; cria o bloco 
  (ag:pra n passo comp vel cor point ang)


  (princ)
)
;defun c:ag_pra

(princ)

;velocidades   35, 45, 60, 75
;passo         60, 90, 120
;comprimento deve ser divisivel pelo passo
;cor           L ou V
