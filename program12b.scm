;author: nate alberti
;date: november 2021
; purpose: plays connect four with two players



(define (NRAgetColumnsHelper List)
    (if (null? List)
        0
        (+ 1 (NRAgetColumns (cdr List)))
     )
)

(define (NRAgetColumns Matrix)
    (if (null? Matrix)
        0
        (NRAgetColumnsHelper (car Matrix))
     )
)

(define (NRAgetRow List Column)
    (if (null? List)
        ()
        (if (= Column 1)
            (car List)
            (NRAgetRow (cdr List) (- Column 1))
         )
     )
)

(define (NRAgetCell Matrix Row Column)
    (if (null? Matrix)
        ()
        (if (= Row 1)
            (NRAgetRow (car Matrix) Column)
            (NRAgetCell (cdr Matrix) (- Row 1) Column)
         )
     )
)

(define (NRAreplaceList List Column Item)
    (if (null? List)
        ()
        (if (= Column 1)
            (cons Item (cdr List))
            (cons (car List) (NRAreplaceList (cdr List) (- Column 1) Item))
         )
        )   
)

(define (NRAsetCell Matrix Row Column Item)
    (if (null? Matrix)
        ()
        (if (= Row 1)
            (cons (NRAreplaceList (car Matrix) Column Item) (cdr Matrix))
            (cons (car Matrix) (NRAsetCell (cdr Matrix) (- Row 1) Column Item))
         )
     )
)

(define NRAGame 0)

(define (NRAStartGame)
  (begin
     (set! NRAGame '(1 (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (1 2 1 2 1 2) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)) )
     (display " AAAAAAAAAAAAAAAAA \n")
      #t ; to return nothing useful to your opponent
  )
)

(define (NRAFindFirstEmpty Row StartCol)
	(if (= 0 (NRAgetCell (cdr NRAGame) Row StartCol))
		StartCol
		(NRAFindFirstEmpty Row (- 1 StartCol))
	)
)

(define (NRAFirstEmptyCol Row StartCol)
   (if (= 0 (list-ref Row (- StartCol 1)))
      StartCol
      (NRAFirstEmptyCol Row (- StartCol 1))
   )
 )

(define (NRAMarkMove Row)
   (begin
      (set! NRAGame
            (if (= 1 (car NRAGame))
            	(cons 2 (NRAsetCell (cdr NRAGame) Row (NRAFirstEmptyCol (NRAgetRow (cdr NRAGame) Row) 6) 1))
            	(cons 1 (NRAsetCell (cdr NRAGame) Row (NRAFirstEmptyCol (NRAgetRow (cdr NRAGame) Row) 6) 2))
            )
      )
      Row
   )
)

(define (NRAChooseMoveHelper Row)
    (if (= 0 (NRAgetCell NRAGame Row 1))
        Row
       (NRAChooseMoveHelper (+ Row 1))
     )
   )

(define (NRAChooseMove)
  (NRAMarkMove (NRAChooseMoveHelper 1))
 )

(define (NRAChooseMoveHelper Row)
  (if (= Row 4)
      (NRAChooseMoveHelper 5)
      (if (= 0 (NRAgetCell (cdr NRAGame) Row 1))
          Row
          (NRAChooseMoveHelper (+ Row 1))
       )
   )
 )

(define (NRAWinP)
  (if (NRAWinPRow 1 1 1 0)
      ((display "player 1 wins"))
      (if (NRAWinPRow 1 2 1 0)
          ((display "player 2 wins"))
          ((display "nobody won yet"))
       )
   )
 )

; only recognizing the win when on first row
(define (NRAWinPRow Row playerChip Column inARow)
  (if (= 4 inARow)
      #t
      (if (= Row 8)
          #f
          (if (= 7 Column)
              (NRAWinPRow (+ Row 1) playerChip 1 0)
              (if (= playerChip (NRAgetCell (cdr NRAGame) Row Column) )
                  (NRAWinPRow Row playerChip (+ 1 Column) (+ 1 inARow))
                  (NRAWinPRow Row playerChip (+ 1 Column) 0)
               )
           )
       )
  )
 )

; replace these calls with simler getRow?
(define (NRAShowGame)
   (begin
     (display (NRAgetRow (cdr NRAGame) 1)) (newline)
     (display (NRAgetRow (cdr NRAGame) 2)) (newline)
     (display (NRAgetRow (cdr NRAGame) 3)) (newline)
     (display (NRAgetRow (cdr NRAGame) 4)) (newline)
     (display (NRAgetRow (cdr NRAGame) 5)) (newline)
     (display (NRAgetRow (cdr NRAGame) 6)) (newline)
     (display (NRAgetRow (cdr NRAGame) 7)) (newline)
      #t
   )
)
