;*******************************************************************************
; DTR
;degrees to radians function
(defun DTR (a)
  (* PI (/ a 180.0))
)
;defun DTR

;*******************************************************************************
; RTD
;radians to degrees function
(defun RTD (a)
  (if (not (eq a 0))
    (* (/ PI a) 180)
    0
  )
)
;defun RTD

;*******************************************************************************
; DRAWPOLY
; funcao para desenhar pologonos fechados, aguarda uma lista de vertices
; retorna o e nome da entidade criada
(defun DrawPoly (lst color)
  (entmakex (append (list (cons 0 "LWPOLYLINE")
                          (cons 100 "AcDbEntity")
                          (cons 100 "AcDbPolyline")
                          (cons 90 (length lst))
                          (cons 70 1)
                          (cons 62 color)
                    )
                    (mapcar (function (lambda (p) (cons 10 p))) lst)
            )
  )
)

;*******************************************************************************
;; List to String  -  Lee Mac
;; Concatenates each string in a supplied list, separated by a given delimiter
;; lst - [lst] List of strings to concatenate
;; del - [str] Delimiter string to separate each item

(defun LM:lst->str (lst del / resp itm)
  (setq resp (car lst))
  (foreach itm (cdr lst) (setq resp (strcat resp del itm)))
  resp
)

;*******************************************************************************
; CREATEBLKNAME
; Draw a rect with width and height parameters
(defun CreateBlkName (lista / bn)
  ;;"999_*"
  ;;(Setq bn (strcat "999_" (substr (rtos (getvar "cdate") 2 8) 10)))
  (setq bn (LM:lst->str lista "_"))
)

;*******************************************************************************
; CREATEBLKNAME
; Draw a rect with width and height parameters
(defun InsertBlk (blockname InsPoint InsAngle)
;; Insert block framing marker
  (entmake
    (list
      (cons 0 "INSERT")            ;; Entity type
      (cons 2 blockname)
      (cons 8 "0")
      (cons 10 InsPoint)           ;; insert point
      (cons 50 InsAngle)           ;; rotation
      (cons 70 0)
      ;(cons 66 1)         ;; Attribute follow
    )
  )
;  (entmake
;    (list
;      (cons 0 "ATTRIB")   ;; Entity type
;      (cons 1 Value)      ;; Attrib Value
;      (cons 2 "RTAG1")    ;; Attrib Tag
;      (cons 7 "1T14")     ;; Textstyle
;      (cons 8 "11T")      ;; Layer
;      (cons 10 InsPoint)  ;; First alignment point
;      (cons 11 InsPoint)  ;; Second alignment point
;      (cons 40 4.0)       ;; Text height
;      (cons 70 0)         ;; Attribute flags: 8
;      (cons 71 0)         ;; Text generation flags
;      (cons 72 1)         ;; Horizontal text justification type
;      (cons 74 2)         ;; Vertical text justification type
;    )
;  )
  (entmake
    (list
      (cons 0 "SEQEND")            ;; Entity type
      (cons 8 "0")                 ;; Layer
    )
  )
)

;*******************************************************************************
; MAKEHATCH
(defun MakeHatch (lst color)
 ;; By ElpanovEvgeniy
 ;; L - list point

 ;; returne - hatch ename
  (entmakex
    (apply
      'append
      (list (list '(0 . "HATCH")
                  '(100 . "AcDbEntity")
                  '(100 . "AcDbHatch")
                  '(10 0.0 0.0 0.0)
                  '(2 . "SOLID")
                  (cons 62 color)  ; cor
                  '(91 . 1)
                  '(92 . 7)
                  '(72 . 0)
                  '(73 . 1)
                  (cons 93 (length lst))
            )
            (mapcar (function (lambda (p) (cons 10 p))) lst)
            (list '(97 . 0))
      ) ;_  list
    ) ;_  apply
  ) ;_  entmakex
) ;_  defun


;*******************************************************************************
; AG:GETPASSO
; solicita o passo da esteira
(defun ag:getPasso (/ tmp passo)
  (if (null global:ag_passo) (setq global:ag_passo "90"))
  (initget "60 90 120")
  (if (setq tmp (getkword (strcat "\Passo [60/90/120] <" global:ag_passo ">: ")))
    ; if input not is null
    (setq global:ag_passo tmp
          passo (atoi tmp)
    )
    ; if input is null
    (setq passo (atoi global:ag_passo))
  )
  passo
)

;*******************************************************************************
; AG:GETN
; solicita o n da esteira
(defun ag:getN (/ n)
  (if (null global:ag_n) (setq global:ag_n 400))
  (setq msg (strcat "\nN <" (itoa global:ag_n) ">: "))
  (initget 4)
  (if (setq n (getint msg))
    (setq global:ag_n n)
    (setq n global:ag_n)
  )
  n
)

;*******************************************************************************
; AG:GETCOMP
; solicita o comprimemto da esteira
(defun ag:getComp (name / var_name comp msg)
  (if (null name)
    (setq var_name "global:ag_comp")
    (setq var_name (strcat "global:ag_" name))
  )

  ;; (eval (read var_name)) read from var
  ;; (set (read var_name) x) write x on var 

  (if (null (eval (read var_name))) (set (read var_name) 720))
  (setq msg (strcat "\n" name " <" (itoa (eval (read var_name))) ">: "))
  (initget 4)
  (if (setq comp (getint msg))
    (set (read var_name) comp)
    (setq comp (eval (read var_name)))
  )
  comp
)

;*******************************************************************************
; AG:GETINSERTPOINT
  ; solicita o ponto de insercao da pista
  ; se nao for informado usa o join_point da ultima pista desenhada
  ; se nao tem join point fica preso no while ate escolher um ponto
(defun ag:getInsertPoint (/ point msg)
  (setq msg (strcat "\nPonto de inserção "
              (if (not (null global:ag_join_point))
                " <Conectar>:"
                ":"
              )
            )
  )
  ; fica preso até entrar com um ponto
  (while (null point)
    ; solicita o ponto
    (setq point (getpoint msg))
    ; se não entrar com o ponto tenta usar o anterio
    (if (null point) (setq point global:ag_join_point))
  )
  point
)

;*******************************************************************************
; AG:GETINSERTANGLE
; solicita o angulo de insercao da esteira
(defun ag:getInsertAngle (point / ang msg)
  (if (null global:ag_ang) (setq global:ag_ang 0))
  (setq msg (strcat "\nAngulo <" (rtos global:ag_ang) ">: "))

  ; fica preso até entrar com um ponto
  (while (null ang)
    (if (setq ang (getangle point msg))
      (setq global:ag_ang ang)
      (setq ang global:ag_ang)
    )
  )
  ang
)

;*******************************************************************************
; AG:GETVEL
; solicita a velocidade da esteira
(defun ag:getVel ( / vel msg)
  (if (null global:ag_vel) (setq global:ag_vel "35"))
  (initget "35 45 60 75")
  (if (setq vel (getkword (strcat "\nVelocidade [35/45/60/75] <" global:ag_vel ">: ")))
    (setq global:ag_vel vel)
    (setq vel global:ag_vel)
  )
  vel
)
 ;*******************************************************************************
; AG:GETCOR
; solicita a cor da esteira
(defun ag:getCor ( / vel msg)
  (if (null global:ag_cor) (setq global:ag_cor "L"))
  (initget "L V")
  (if (setq cor (getkword (strcat "\nCor [L/V] <" global:ag_cor ">: ")))
    (setq global:ag_cor cor)
    (setq cor global:ag_cor)
  )

  (if (eq "L" global:ag_cor) (setq global:ag_cor "V") (setq global:ag_cor "L"))
  cor
)


  
(princ)
