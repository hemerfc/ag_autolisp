; Dump/Entget a graphical object (entsel behaviour), with options
; author: Grrr (thanks to Lee Mac & Tharwat that I've reached such level of coding)
(defun C:ag_dump ( / *error* SysVarLst R o m )
(setvar 'errno 0)

(defun *error* ( msg )
	(mapcar '(lambda ( n v / ) (setvar n v)) (mapcar 'car SysVarLst) (mapcar 'cadr SysVarLst))
	(if (not (member msg '("Function cancelled" "quit / exit abort")))
		(princ (strcat "\nError: " msg))
	)
	(princ)
)

(setq SysVarLst
	(mapcar '(lambda ( a b / ) (list a (getvar a) b))
		(list "CLIPROMPTLINES" "CMDECHO" "PDMODE") ; PDMODE, because sometimes is hard to (entsel) a point with PDMODE of 0 value
		(list 1 0 3) ; the new intended values
	)
)

(while (/= 52 (getvar 'errno))
	(mapcar '(lambda ( n v / ) (setvar n v)) (mapcar 'car SysVarLst) (mapcar 'caddr SysVarLst))
	(initget "Entget Dump eXit")
	(if (not m) (setq m "Entget"))
	(setq R (entsel (strcat "\nSelect entity or [Entget/Dump/eXit] <\"" m "\">: ")))
	(cond
		((member (getvar 'errno) '( 7 ))
			(princ "\nMissed, try again! ") ; non-nil return, stay in loop
			(setvar 'errno 0)
		)
		((and (not (listp R))(member (strcase R) '("E" "ENTGET")))
			(if (not (= m "Entget")) (setq m "Entget"))
		)
		((and (not (listp R))(member (strcase R) '("D" "DUMP")))
			(if (not (= m "Dump")) (setq m "Dump"))
		)
		((and (not (listp R))(member (strcase R) '("X" "EXIT")))
			(princ "\nI hope this routine helped! ")
			(mapcar '(lambda ( n v / ) (setvar n v)) (mapcar 'car SysVarLst) (mapcar 'cadr SysVarLst))
			(setvar 'errno 52)
			(vl-cmdf "_.textscr")
		)
		(R
			(cond
				((= (strcase m) "ENTGET")
					(if (= (type (car R)) 'ENAME)
						(progn
							(princ "\n******************** ENTGET RESULTS: ********************")
							(foreach x (entget (car R)) (princ "\n")(print x))
							(princ "\n******************** END OF RESULTS ********************")
						)
					)
				)
				((= (strcase m) "DUMP")
					(if (= (type (setq o (vlax-ename->vla-object (car R)))) 'VLA-OBJECT)
						(progn
							(princ "\n******************** DUMP RESULTS: ********************")
							(vlax-dump-object o T)
							(princ "\n******************** END OF RESULTS ********************")
						)
					)
				)
			); cond 
		)
		(T ; if user pressed enter
			(princ "\nBye, user! ")
			(setvar 'errno 52)
			(mapcar '(lambda ( n v / ) (setvar n v)) (mapcar 'car SysVarLst) (mapcar 'cadr SysVarLst))
			(vl-catch-all-apply (quote textscr) (list))
		)
	); cond
); while go

(princ)
);| defun |; (vl-load-com) (princ)