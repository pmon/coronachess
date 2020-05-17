(declare
	(standard-bindings)
	(extended-bindings)
	(not safe)
	(block))

; squares
; 56 57 58 59 60 61 62 63
; 48 49 50 51 52 53 54 55
; 40 41 42 43 44 45 46 47
; 32 33 34 35 36 37 38 39
; 24 25 26 27 28 29 30 31
; 16 17 18 19 20 21 22 23
;  8  9 10 11 12 13 14 15
;  0  1  2  3  4  5  6  7

(define white +1)
(define black -1)

(define pawn 1)
(define knight 2)
(define bishop 3)
(define rook 4)
(define queen 5)
(define king 6)

(define (square-file square) (fx+ 1 (fxremainder square 8)))
(define (square-rank square) (fx+ 1 (fxquotient square 8)))

(define u64-and bitwise-and)
(define u64-ior bitwise-ior)
(define u64-xor bitwise-xor)
(define u64-not bitwise-not)
(define (u64-shift n i)	(u64-and #xFFFFFFFFFFFFFFFF (arithmetic-shift n i)))
(define (u64-mult a b) (u64-and #xFFFFFFFFFFFFFFFF (* a b)))
(define bitscan-fwd first-bit-set)
(define (bitscan-rev n) (- (integer-length n) 1))
(define (reset-ls1b bits) (u64-and bits (- bits 1)))

(define (square->bit square)
	(u64-shift 1 square))

(define (bits->squares bits)
	(if (zero? bits) '() (cons (bitscan-fwd bits) (bits->squares (reset-ls1b bits)))))

; return the front fill for the given bits of side
(define (front-fill side bits)
	(fold	(lambda (shift b) (u64-ior b (u64-shift b (fx* side shift))))
		bits '(8 16 32)))

(define (file-fill bits) (u64-ior (front-fill white bits) (front-fill black bits)))

; (define-macro (u64-ref0 v)
;  `(u64vector-ref ,v 0))

;; ---- static vector construction ----

; (define w-castle-short (u64vector (apply u64-ior (map square->bit (list 4 5 6)))))
; (define b-castle-short (u64vector (apply u64-ior (map square->bit (list 60 61 62)))))
; (define w-castle-long (u64vector (apply u64-ior (map square->bit (list 2 3 4)))))
; (define b-castle-long (u64vector (apply u64-ior (map square->bit (list 58 59 60)))))
; (define w-castle-short-mask (u64vector (apply u64-ior (map square->bit (list 5 6)))))
; (define b-castle-short-mask (u64vector (apply u64-ior (map square->bit (list 61 62)))))
; (define w-castle-long-mask (u64vector (apply u64-ior (map square->bit (list 1 2 3)))))
; (define b-castle-long-mask (u64vector (apply u64-ior (map square->bit (list 57 58 59)))))
(define w-castle-short (apply u64-ior (map square->bit (list 4 5 6))))
(define b-castle-short (apply u64-ior (map square->bit (list 60 61 62))))
(define w-castle-long (apply u64-ior (map square->bit (list 2 3 4))))
(define b-castle-long (apply u64-ior (map square->bit (list 58 59 60))))
(define w-castle-short-mask (apply u64-ior (map square->bit (list 5 6))))
(define b-castle-short-mask (apply u64-ior (map square->bit (list 61 62))))
(define w-castle-long-mask (apply u64-ior (map square->bit (list 1 2 3))))
(define b-castle-long-mask (apply u64-ior (map square->bit (list 57 58 59))))

(define ranks-bitbrd
  ; (list->vector (map (lambda (s) (u64vector (u64-shift #xFF s))) (iota 8 0 8))))
  (list->u64vector (map (lambda (s) (u64-shift #xFF s)) (iota 8 0 8))))

(define files-bitbrd
  ; (list->vector (map (lambda (bit) (u64vector (file-fill bit))) (map square->bit (iota 8 24)))))
  (list->u64vector (map (lambda (bit) (file-fill bit)) (map square->bit (iota 8 24)))))

(define king-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (bit)
        ; (u64vector
        ;   (u64-ior
        ;     (u64-shift bit 8)
        ;     (u64-and (u64-shift bit  9) (u64-not (u64-ref0 (vector-ref files-bitbrd 0))))
        ;     (u64-and (u64-shift bit  1) (u64-not (u64-ref0 (vector-ref files-bitbrd 0))))
        ;     (u64-and (u64-shift bit -7) (u64-not (u64-ref0 (vector-ref files-bitbrd 0))))
        ;     (u64-shift bit -8)
        ;     (u64-and (u64-shift bit -9) (u64-not (u64-ref0 (vector-ref files-bitbrd 7))))
        ;     (u64-and (u64-shift bit -1) (u64-not (u64-ref0 (vector-ref files-bitbrd 7))))
        ;     (u64-and (u64-shift bit  7) (u64-not (u64-ref0 (vector-ref files-bitbrd 7)))))))
        (u64-ior
          (u64-shift bit 8)
          (u64-and (u64-shift bit  9) (u64-not (u64vector-ref files-bitbrd 0)))
          (u64-and (u64-shift bit  1) (u64-not (u64vector-ref files-bitbrd 0)))
          (u64-and (u64-shift bit -7) (u64-not (u64vector-ref files-bitbrd 0)))
          (u64-shift bit -8)
          (u64-and (u64-shift bit -9) (u64-not (u64vector-ref files-bitbrd 7)))
          (u64-and (u64-shift bit -1) (u64-not (u64vector-ref files-bitbrd 7)))
          (u64-and (u64-shift bit  7) (u64-not (u64vector-ref files-bitbrd 7)))))
      (map square->bit (iota 64)))))

(define knight-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (bit)
        ; (u64vector
          (u64-ior
            ; (u64-and (u64-shift bit 17) (u64-not (u64-ref0 (vector-ref files-bitbrd 0))))
            ; (u64-and (u64-shift bit 15) (u64-not (u64-ref0 (vector-ref files-bitbrd 7))))
            ; (u64-and (u64-shift bit 10)
            ;   (u64-not (u64-ior (u64-ref0 (vector-ref files-bitbrd 0)) (u64-ref0 (vector-ref files-bitbrd 1)))))
            ; (u64-and (u64-shift bit -6)
            ;   (u64-not (u64-ior (u64-ref0 (vector-ref files-bitbrd 0)) (u64-ref0 (vector-ref files-bitbrd 1)))))
            ; (u64-and (u64-shift bit -15) (u64-not (u64-ref0 (vector-ref files-bitbrd 0))))
            ; (u64-and (u64-shift bit -17) (u64-not (u64-ref0 (vector-ref files-bitbrd 7))))
            ; (u64-and (u64-shift bit 6)
            ;   (u64-not (u64-ior (u64-ref0 (vector-ref files-bitbrd 7)) (u64-ref0 (vector-ref files-bitbrd 6)))))
            ; (u64-and (u64-shift bit -10)
            ;   (u64-not (u64-ior (u64-ref0 (vector-ref files-bitbrd 7)) (u64-ref0 (vector-ref files-bitbrd 6)))))))
            (u64-and (u64-shift bit 17) (u64-not (u64vector-ref files-bitbrd 0)))
            (u64-and (u64-shift bit 15) (u64-not (u64vector-ref files-bitbrd 7)))
            (u64-and (u64-shift bit 10)
              (u64-not (u64-ior (u64vector-ref files-bitbrd 0) (u64vector-ref files-bitbrd 1))))
            (u64-and (u64-shift bit -6)
              (u64-not (u64-ior (u64vector-ref files-bitbrd 0) (u64vector-ref files-bitbrd 1))))
            (u64-and (u64-shift bit -15) (u64-not (u64vector-ref files-bitbrd 0)))
            (u64-and (u64-shift bit -17) (u64-not (u64vector-ref files-bitbrd 7)))
            (u64-and (u64-shift bit 6)
              (u64-not (u64-ior (u64vector-ref files-bitbrd 7) (u64vector-ref files-bitbrd 6))))
            (u64-and (u64-shift bit -10)
              (u64-not (u64-ior (u64vector-ref files-bitbrd 7) (u64vector-ref files-bitbrd 6))))))
          ; )
      (map square->bit (iota 64)))))

(define (squares->bitboard square-list)
  ; (u64vector
    (fold u64-ior 0 (map square->bit square-list)))
  ; )

(define rays-n-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        (squares->bitboard (iota (- 8 (square-rank square)) (+ square 8) 8)))
      (iota 64))))

(define rays-ne-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        (squares->bitboard
          (iota (min (- 8 (square-rank square)) (- 8 (square-file square))) (+ square 9) 9)))
      (iota 64))))

(define rays-e-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        (squares->bitboard (iota (- 8 (square-file square)) (+ square 1) 1)))
      (iota 64))))

(define rays-se-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        (squares->bitboard
          (iota (min (- (square-rank square) 1) (- 8 (square-file square))) (- square 7) -7)))
      (iota 64))))

(define rays-s-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        (squares->bitboard (iota (- (square-rank square) 1) (- square 8) -8)))
      (iota 64))))

(define rays-sw-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        (squares->bitboard
          (iota (min (- (square-rank square) 1) (- (square-file square) 1)) (- square 9) -9)))
      (iota 64))))

(define rays-w-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        (squares->bitboard (iota (- (square-file square) 1) (- square 1) -1)))
      (iota 64))))

(define rays-nw-bitbrd
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        (squares->bitboard
          (iota (min (- 8 (square-rank square)) (- (square-file square) 1)) (+ square 7) 7)))
      (iota 64))))

; (define (attacks-bitbrd-for-square piece square blockers)
; 	(let ((attacks 0))
; 		(cond
; 			((equal? piece rook)
; 				(u64-ior 
; 					(let* ((ray (u64-ref0 (vector-ref rays-n-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks ray)
; 							(u64-and (u64-ior attacks ray)
; 								(u64-not (u64-ref0 (vector-ref rays-n-bitbrd (bitscan-fwd blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-e-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks ray)
; 							(u64-and (u64-ior attacks ray)
; 								(u64-not (u64-ref0 (vector-ref rays-e-bitbrd (bitscan-fwd blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-s-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks ray)
; 							(u64-and (u64-ior attacks ray)
; 								(u64-not (u64-ref0 (vector-ref rays-s-bitbrd (bitscan-rev blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-w-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks ray)
; 							(u64-and (u64-ior attacks ray)
; 								(u64-not (u64-ref0 (vector-ref rays-w-bitbrd (bitscan-rev blk)))))))))
; 			((equal? piece bishop)
; 				(u64-ior 
; 					(let* ((ray (u64-ref0 (vector-ref rays-ne-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks ray)
; 							(u64-and (u64-ior attacks ray)
; 								(u64-not (u64-ref0 (vector-ref rays-ne-bitbrd (bitscan-fwd blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-nw-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks ray)
; 							(u64-and (u64-ior attacks ray)
; 								(u64-not (u64-ref0 (vector-ref rays-nw-bitbrd (bitscan-fwd blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-se-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks ray)
; 							(u64-and (u64-ior attacks ray)
; 								(u64-not (u64-ref0 (vector-ref rays-se-bitbrd (bitscan-rev blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-sw-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks ray)
; 							(u64-and (u64-ior attacks ray)
; 								(u64-not (u64-ref0 (vector-ref rays-sw-bitbrd (bitscan-rev blk)))))))))
; 			(else 0))))
(define (attacks-bitbrd-for-square piece square blockers)
	(let ((attacks 0))
		(cond
			((equal? piece rook)
				(u64-ior 
					(let* ((ray (u64vector-ref rays-n-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks ray)
							(u64-and (u64-ior attacks ray)
								(u64-not (u64vector-ref rays-n-bitbrd (bitscan-fwd blk))))))
					(let* ((ray (u64vector-ref rays-e-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks ray)
							(u64-and (u64-ior attacks ray)
								(u64-not (u64vector-ref rays-e-bitbrd (bitscan-fwd blk))))))
					(let* ((ray (u64vector-ref rays-s-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks ray)
							(u64-and (u64-ior attacks ray)
								(u64-not (u64vector-ref rays-s-bitbrd (bitscan-rev blk))))))
					(let* ((ray (u64vector-ref rays-w-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks ray)
							(u64-and (u64-ior attacks ray)
								(u64-not (u64vector-ref rays-w-bitbrd (bitscan-rev blk))))))))
			((equal? piece bishop)
				(u64-ior 
					(let* ((ray (u64vector-ref rays-ne-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks ray)
							(u64-and (u64-ior attacks ray)
								(u64-not (u64vector-ref rays-ne-bitbrd (bitscan-fwd blk))))))
					(let* ((ray (u64vector-ref rays-nw-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks ray)
							(u64-and (u64-ior attacks ray)
								(u64-not (u64vector-ref rays-nw-bitbrd (bitscan-fwd blk))))))
					(let* ((ray (u64vector-ref rays-se-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks ray)
							(u64-and (u64-ior attacks ray)
								(u64-not (u64vector-ref rays-se-bitbrd (bitscan-rev blk))))))
					(let* ((ray (u64vector-ref rays-sw-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks ray)
							(u64-and (u64-ior attacks ray)
								(u64-not (u64vector-ref rays-sw-bitbrd (bitscan-rev blk))))))))
			(else 0))))

; (define (attacks-bitbrd-for-square-no-borders piece square blockers)
; 	(let ((attacks 0))
; 		(cond
; 			((equal? piece rook)
; 				(u64-ior 
; 					(let* ((ray (u64-ref0 (vector-ref rays-n-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0)
; 							(u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 7)))))
; 							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 7)))))
; 								(u64-not (u64-ref0 (vector-ref rays-n-bitbrd (bitscan-fwd blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-e-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref files-bitbrd 7)))))
; 							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref files-bitbrd 7)))))
; 								(u64-not (u64-ref0 (vector-ref rays-e-bitbrd (bitscan-fwd blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-s-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 0)))))
; 							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 0)))))
; 								(u64-not (u64-ref0 (vector-ref rays-s-bitbrd (bitscan-rev blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-w-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref files-bitbrd 0)))))
; 							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 0)))))
; 								(u64-not (u64-ref0 (vector-ref rays-w-bitbrd (bitscan-rev blk)))))))))
; 			((equal? piece bishop)
; 				(u64-ior 
; 					(let* ((ray (u64-ref0 (vector-ref rays-ne-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 7))) (u64-not (u64-ref0 (vector-ref files-bitbrd 7)))))
; 							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 7))) (u64-not (u64-ref0 (vector-ref files-bitbrd 7)))))
; 								(u64-not (u64-ref0 (vector-ref rays-ne-bitbrd (bitscan-fwd blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-nw-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 7))) (u64-not (u64-ref0 (vector-ref files-bitbrd 0)))))
; 							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 7))) (u64-not (u64-ref0 (vector-ref files-bitbrd 0)))))
; 								(u64-not (u64-ref0 (vector-ref rays-nw-bitbrd (bitscan-fwd blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-se-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 0))) (u64-not (u64-ref0 (vector-ref files-bitbrd 7)))))
; 							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 0))) (u64-not (u64-ref0 (vector-ref files-bitbrd 7)))))
; 								(u64-not (u64-ref0 (vector-ref rays-se-bitbrd (bitscan-rev blk)))))))
; 					(let* ((ray (u64-ref0 (vector-ref rays-sw-bitbrd square))) (blk (u64-and ray blockers)))
; 						(if (equal? blk 0) 
; 							(u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 0))) (u64-not (u64-ref0 (vector-ref files-bitbrd 0)))))
; 							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64-ref0 (vector-ref ranks-bitbrd 0))) (u64-not (u64-ref0 (vector-ref files-bitbrd 0)))))
; 								(u64-not (u64-ref0 (vector-ref rays-sw-bitbrd (bitscan-rev blk)))))))))
; 			(else 0))))
(define (attacks-bitbrd-for-square-no-borders piece square blockers)
	(let ((attacks 0))
		(cond
			((equal? piece rook)
				(u64-ior 
					(let* ((ray (u64vector-ref rays-n-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0)
							(u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 7))))
							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 7))))
								(u64-not (u64vector-ref rays-n-bitbrd (bitscan-fwd blk))))))
					(let* ((ray (u64vector-ref rays-e-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks (u64-and ray (u64-not (u64vector-ref files-bitbrd 7))))
							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64vector-ref files-bitbrd 7))))
								(u64-not (u64vector-ref rays-e-bitbrd (bitscan-fwd blk))))))
					(let* ((ray (u64vector-ref rays-s-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 0))))
							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 0))))
								(u64-not (u64vector-ref rays-s-bitbrd (bitscan-rev blk))))))
					(let* ((ray (u64vector-ref rays-w-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks (u64-and ray (u64-not (u64vector-ref files-bitbrd 0))))
							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 0))))
								(u64-not (u64vector-ref rays-w-bitbrd (bitscan-rev blk))))))))
			((equal? piece bishop)
				(u64-ior 
					(let* ((ray (u64vector-ref rays-ne-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 7)) (u64-not (u64vector-ref files-bitbrd 7))))
							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 7)) (u64-not (u64vector-ref files-bitbrd 7))))
								(u64-not (u64vector-ref rays-ne-bitbrd (bitscan-fwd blk))))))
					(let* ((ray (u64vector-ref rays-nw-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 7)) (u64-not (u64vector-ref files-bitbrd 0))))
							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 7))) (u64-not (u64vector-ref files-bitbrd 0)))
								(u64-not (u64vector-ref rays-nw-bitbrd (bitscan-fwd blk))))))
					(let* ((ray (u64vector-ref rays-se-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 0)) (u64-not (u64vector-ref files-bitbrd 7))))
							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 0)) (u64-not (u64vector-ref files-bitbrd 7))))
								(u64-not (u64vector-ref rays-se-bitbrd (bitscan-rev blk))))))
					(let* ((ray (u64vector-ref rays-sw-bitbrd square)) (blk (u64-and ray blockers)))
						(if (equal? blk 0) 
							(u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 0)) (u64-not (u64vector-ref files-bitbrd 0))))
							(u64-and (u64-ior attacks (u64-and ray (u64-not (u64vector-ref ranks-bitbrd 0)) (u64-not (u64vector-ref files-bitbrd 0))))
								(u64-not (u64vector-ref rays-sw-bitbrd (bitscan-rev blk))))))))
			(else 0))))

(define bishop-mask
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        ; (u64vector
          (attacks-bitbrd-for-square-no-borders bishop square 0))
        ; )
      (iota 64))))

(define rook-mask
  ; (list->vector
  (list->u64vector
    (map
      (lambda (square)
        ; (u64vector
          (attacks-bitbrd-for-square-no-borders rook square 0))
        ; )
      (iota 64))))

(define sq-magic ; 128
  ; (list->vector
  (list->u64vector
    ; (map u64vector 
      (list
      ; rooks
        #x00A801F7FBFEFFFF #xFFB000810113FFFF #x0060001F9FF3FFFF #x00600C005FF9FFFF #xFFA003005FF9FFFF #x7F600280089FFFF1 #x80B000B0029FFFFC #x7F50003004C5FFFF
        #xD7FFA0002FFFFFA0 #xFF005000FDE8FFFF #xDAFFA0005FEBFFFF #xD9FF400CDF3FFF28 #xFFFF600F600AFFFF #xFFFEC00FC13FFFCE #xFFFEC003EEBFFFF1 #xD48150004A07FFFC
        #xD79F003000E7FFFA #xDA680030020BFFFF #x732BD20017A1BFFB #x01306060094FFFFF #x1BA98060061CFFFF #x01C1806003005FFF #x021BFD00040BFFFA #x02F5EE00007BFFC5
        #xF8FFF1918007C000 #x03FFF037D000C005 #xB18060320019BFF9 #x0C35F03A000C1FFC #x3C7FFF0100100804 #x330FFF0100080402 #x0097FFE0C0060009 #x0119FFE4B0050004
        #x03FFFD3000600061 #x03FFFEB800600060 #xFFFFFF8C00600060 #xDBFFFE240A001830 #x72AFFFA040600C00 #x03CFFFA03FA00600 #x00AFFFEC82C00C05 #xC49FFFF8CC8000C5
        #x01FFFDFDD8005000 #x03FFFDFE6BFFB000 #xFFFFFFDF603F6000 #xF41FFE5D3E660010 #xFF7FEFF1EDF1FFF7 #x02FFFF9FFDBFA006 #x01FFFFEEBFFEC005 #xD64FFFFDF3FEB001
        #xD8FFFF39FF484A00 #xDF7FFF3FFF486300 #xDD3FFF99FFAC2E00 #xDE9FFF31FF2A6A00 #xDC3FFF19FF15B600 #xB0FFFFF5FFF28600 #xDA8FFFE2DFFBFEE0 #xDB3FFFF5F63C96A0
        #x00FFFF5DFF65CFB6 #x037FFFBAFFD1C5AE #x033FFF71FF6CBCEA #x015FFFD9FFD4756E #xFFFFFFF5FFF338E6 #xF7FFFFFE852AC7D5 #x017FFFEF27EEBE74 #x05FFFFFF23FF605E
      ; bishops
        #xED06EFF5367FF600 #xF0345835BA77FF2B #xEC145F68A3F5DAB6 #xEEFA1863FB56F21D #xEF52EB6BFE9D93CD #xED40A2927F3420D6 #xF3774BCD9C7FEC97 #xEE6034FE99F9FFFF
        #xF3D0746F8D6717F6 #xF288BACB32E1A3F7 #xED1AF83CAF1FFB8A #xECCE061867F17067 #xEC0C238EE0CCF92E #xEF9760A2937F926E #xF28086C9AA93FF14 #xF2180399B5E5BF87
        #xF0400F342C951FFC #xF0A0230579ED8FF0 #xE40300860032FFFD #xE4DC00080B17FD46 #xE4440022031C1FFB #xE4DFA00FD30BFF79 #xF16C00A4BC9AFFDF #xF34200085E9CFFDA
        #xEF9A14560A3DBFBD #xEE1A18157B9EAFD1 #xE466030086002FFE #xDCC006000C009010 #xDC1A002042008040 #xE4B1CE000470FFC0 #xEFFD0ACE50BF3F8D #xEE3980648434EFD1
        #xEEDFBD7670982A0D #xECFFC30301D81A0F #xE4FFFB782F7C00C1 #xDC0440C800008200 #xDCFFE83400060066 #xE4FB7DDF0FFE1EFF #xEC5F92F861DF4A0A #xEDDFD19BAD98A289
        #xF10FD6AA751E400C #xF07FF2A63AE9600C #xE40FFF6EE50E4A00 #xE4B7FFFD2704CE04 #xE4FFFFCCC1500300 #xE4FFFF4AF05000A0 #xF0FFA66283556403 #xF2FFE31969AEC201
        #xF2FFFDFC18AC14BB #xF3FFFB96FB568A47 #xEFD333EFAEEC954D #xEFD9A39BF8DC0383 #xEC921FFF3A814490 #xED61FDC595CF62A6 #xF1FF23D3342897AC #xF0FFEE36EEE1565C
        #xEC2FFF3E99FCCCC7 #xF3FFBFECFCFAC5FE #xEF8FFF4FF97F7453 #xEDBFFF83E7F8DC03 #xECBFFF77FEFA8146 #xEC5FFF7ED3E2EF60 #xF2FF7F47243ADCD6 #xEDFFB65AFABFB3B5)))
  ; )

(define magic-bishop-idx (fx* 64 512)) ; 9 bits (so you shift by 64-9 = 55)
(define magic-rook-idx (fx* 64 4096)) ; 12 bits (so you shift by 64-12 = 52)
(define magic-db
  ; (make-vector (+ magic-rook-idx magic-bishop-idx) (u64vector 0)))
  (make-u64vector (+ magic-rook-idx magic-bishop-idx) 0))

; (define (magic-rook-attacks-set! square occupied)
;   (let* ((blockers (u64-and occupied (u64-ref0 (vector-ref rook-mask square))))
;          (key (u64-shift (u64-mult blockers (u64-ref0 (vector-ref sq-magic square))) -52))
;          (idx (+ key (fx* square 4096))))
;     (if (< idx (vector-length magic-db))
;       (vector-set! magic-db idx
;         (u64vector (attacks-bitbrd-for-square rook square occupied)))
;       (pretty-print (list 'rook-overflow square occupied blockers key idx)))))
(define (magic-rook-attacks-set! square occupied)
  (let* ((blockers (u64-and occupied (u64vector-ref rook-mask square)))
         (key (u64-shift (u64-mult blockers (u64vector-ref sq-magic square)) -52))
         (idx (+ key (fx* square 4096))))
    (if (< idx (u64vector-length magic-db))
      (u64vector-set! magic-db idx
        (attacks-bitbrd-for-square rook square occupied))
      (pretty-print (list 'rook-overflow square occupied blockers key idx)))))

; (define (magic-bishop-attacks-set! square occupied)
;   (let* ((blockers (bitwise-and occupied (u64-ref0 (vector-ref bishop-mask square))))
;          (key (u64-shift (u64-mult blockers (u64-ref0 (vector-ref sq-magic (+ 64 square)))) -55))
;          (idx (+ key (fx* square 512) magic-rook-idx)))
;     (if (< idx (vector-length magic-db))
;       (vector-set! magic-db idx
;         (u64vector (attacks-bitbrd-for-square bishop square occupied)))
;       (pretty-print (list 'bishop-overflow square occupied blockers key idx)))))
(define (magic-bishop-attacks-set! square occupied)
  (let* ((blockers (bitwise-and occupied (u64vector-ref bishop-mask square)))
         (key (u64-shift (u64-mult blockers (u64vector-ref sq-magic (+ 64 square))) -55))
         (idx (+ key (fx* square 512) magic-rook-idx)))
    (if (< idx (u64vector-length magic-db))
      (u64vector-set! magic-db idx
        (attacks-bitbrd-for-square bishop square occupied))
      (pretty-print (list 'bishop-overflow square occupied blockers key idx)))))

(define (blockers-from-idx idx mask)
  (let loop ((blockers 0) (bits (bits->squares mask)))
    (cond
      ((null? bits) blockers)
      ((any-bits-set? idx (u64-shift 1 (- (length bits) 1)))
        (loop (u64-ior blockers (u64-shift 1 (car bits))) (cdr bits)))
      (else (loop blockers (cdr bits))))))

; (define (init-magic-db-rook)
;   (do ((square 0 (+ 1 square)))
;     ((= square 64) 'ok)
;     (do ((blk-idx 0 (+ 1 blk-idx)))
;       ((= blk-idx (u64-shift 1 12)) 'ok)
;       (let ((blockers (blockers-from-idx blk-idx (u64-ref0 (vector-ref rook-mask square)))))
;         (magic-rook-attacks-set! square blockers)))))
(define (init-magic-db-rook)
  (do ((square 0 (+ 1 square)))
    ((= square 64) 'ok)
    (do ((blk-idx 0 (+ 1 blk-idx)))
      ((= blk-idx (u64-shift 1 12)) 'ok)
      (let ((blockers (blockers-from-idx blk-idx (u64vector-ref rook-mask square))))
        (magic-rook-attacks-set! square blockers)))))
(init-magic-db-rook)

(define (magic-bishop-attacks square occupied)
	(u64vector-ref magic-db
		(fx+ (fx* square 512) magic-rook-idx
			 (u64-shift	(u64-mult (u64-and occupied (u64vector-ref bishop-mask square))
				(u64vector-ref sq-magic (fx+ 64 square))) -55))))

(define (magic-rook-attacks square occupied)
	(u64vector-ref magic-db
		(fx+ (fx* square 4096)
			 (u64-shift (u64-mult (u64-and occupied (u64vector-ref rook-mask square))
				(u64vector-ref sq-magic square)) -52))))

; (define (init-magic-db-bishop)
;   (do ((square 0 (+ 1 square)))
;     ((= square 64) 'ok)
;     (do ((blk-idx 0 (+ 1 blk-idx)))
;       ((= blk-idx (u64-shift 1 9)) 'ok)
;       (let ((blockers (blockers-from-idx blk-idx (u64-ref0 (vector-ref bishop-mask square)))))
;         (magic-bishop-attacks-set! square blockers)))))
(define (init-magic-db-bishop)
  (do ((square 0 (+ 1 square)))
    ((= square 64) 'ok)
    (do ((blk-idx 0 (+ 1 blk-idx)))
      ((= blk-idx (u64-shift 1 9)) 'ok)
      (let ((blockers (blockers-from-idx blk-idx (u64vector-ref bishop-mask square))))
        (magic-bishop-attacks-set! square blockers)))))
(init-magic-db-bishop)

(define (init-pseudo-random)
	(random-source-pseudo-randomize! default-random-source 22 03))
(define (random-64bit) (random-integer (+ #xFFFFFFFFFFFFFFFF 1)))
; (define (v-side-index side)
; 	(if (fx= side white) (fx- (fx* 64 6) 4) (fx- (fx* 64 6) 3)))
; (define (v-both-side-index) (fx- (fx* 64 6) 2)) ; stores xor between white & black
(define (v-side-index side)
	(if (fx= side white) 60 61))
(define v-both-side-index 62) ; stores xor between white & black

(define (init-zobrist-vector)
	(init-pseudo-random)
	(let loop ((zobrist-vector (make-u64vector ; (make-vector 
              768)) (i 0))
		(if (< i 768)
			(begin
        ; (vector-set! zobrist-vector i (u64vector (random-64bit)))
        (u64vector-set! zobrist-vector i (random-64bit))
				(loop zobrist-vector (+ 1 i)))
			(begin
				; (vector-set! zobrist-vector (v-both-side-index) 
				(u64vector-set! zobrist-vector v-both-side-index 
					; (u64vector
            (u64-xor
						; (u64-ref0 (vector-ref zobrist-vector (v-side-index white)))
						; (u64-ref0 (vector-ref zobrist-vector (v-side-index black))))))
						(u64vector-ref zobrist-vector (v-side-index white))
						(u64vector-ref zobrist-vector (v-side-index black))))
          ; )
				zobrist-vector))))

(define zhash (init-zobrist-vector))

(define (lines-idx sq1 sq2) (fx+ sq2 (fx* 64 sq1)))

(define lines (make-u64vector (fx* 64 64) 0))
(define (init-lines) 
	(for-each
		(lambda (sq1)
			(let ((rook-bits (magic-rook-attacks sq1 0))
						(bishop-bits (magic-bishop-attacks sq1 0)))
				(for-each
					(lambda (sq2)
						(if (any-bits-set? rook-bits (square->bit sq2))
							(u64vector-set! lines (lines-idx sq1 sq2)
								(u64-ior (square->bit sq1) (square->bit sq2) 
									(u64-and rook-bits (magic-rook-attacks sq2 0)))))
						(if (any-bits-set? bishop-bits (square->bit sq2))
							(u64vector-set! lines (lines-idx sq1 sq2)
								(u64-ior (square->bit sq1) (square->bit sq2) 
									(u64-and bishop-bits (magic-bishop-attacks sq2 0))))))
					(iota 64))))
		(iota 64)))
(init-lines)

(define (between-idx sq1 sq2) (fx+ sq2 (fx* 64 sq1)))

(define segments (make-u64vector (fx* 64 64) 0))

(define (in-between sq1 sq2)
	(cond
		((fx= (square-file sq1) (square-file sq2))
			(if (fx< sq1 sq2)
				(u64-and (u64vector-ref rays-n-bitbrd sq1) (u64vector-ref rays-s-bitbrd sq2))
				(u64-and (u64vector-ref rays-s-bitbrd sq1) (u64vector-ref rays-n-bitbrd sq2))))
		((fx= (square-rank sq1) (square-rank sq2))
			(if (fx< sq1 sq2)
				(u64-and (u64vector-ref rays-e-bitbrd sq1) (u64vector-ref rays-w-bitbrd sq2))
				(u64-and (u64vector-ref rays-w-bitbrd sq1) (u64vector-ref rays-e-bitbrd sq2))))
		((fx> (square-file sq1) (square-file sq2))
			(if (fx< (square-rank sq1) (square-rank sq2))
				(u64-and (u64vector-ref rays-nw-bitbrd sq1) (u64vector-ref rays-se-bitbrd sq2))
				(u64-and (u64vector-ref rays-sw-bitbrd sq1) (u64vector-ref rays-ne-bitbrd sq2))))
		((fx< (square-file sq1) (square-file sq2))
			(if (fx< (square-rank sq1) (square-rank sq2))
				(u64-and (u64vector-ref rays-ne-bitbrd sq1) (u64vector-ref rays-sw-bitbrd sq2))
				(u64-and (u64vector-ref rays-se-bitbrd sq1) (u64vector-ref rays-nw-bitbrd sq2))))
		(else 0)))

(define (init-segments) 
	(for-each
		(lambda (sq1)
			(for-each
				(lambda (sq2)
					(u64vector-set! segments (between-idx sq1 sq2)
						(in-between sq1 sq2)))
				(iota 64)))
		(iota 64)))
(init-segments)

(with-output-to-file 
  (list path: "bitmap-vectors.scm" create: 'maybe)
  (lambda ()
    (display "(define w-castle-short ")
    (write w-castle-short)
    (display ")")
    (newline)

    (display "(define b-castle-short ")
    (write b-castle-short)
    (display ")")
    (newline)

    (display "(define w-castle-long ")
    (write w-castle-long)
    (display ")")
    (newline)

    (display "(define b-castle-long ")
    (write b-castle-long)
    (display ")")
    (newline)

    (display "(define w-castle-short-mask ")
    (write w-castle-short-mask)
    (display ")")
    (newline)

    (display "(define b-castle-short-mask ")
    (write b-castle-short-mask)
    (display ")")
    (newline)

    (display "(define w-castle-long-mask ")
    (write w-castle-long-mask)
    (display ")")
    (newline)

    (display "(define b-castle-long-mask ")
    (write b-castle-long-mask)
    (display ")")
    (newline)

    (display "(define lines ")
    (write lines)
    (display ")")
    (newline)

    (display "(define segments ")
    (write segments)
    (display ")")
    (newline)

    (display "(define king-bitbrd ")
    (write king-bitbrd)
    (display ")")
    (newline)

    (display "(define knight-bitbrd ")
    (write knight-bitbrd)
    (display ")")
    (newline)

    (display "(define bishop-mask ")
    (write bishop-mask)
    (display ")")
    (newline)

    (display "(define rook-mask ")
    (write rook-mask)
    (display ")")
    (newline)

    (display "(define sq-magic ")
    (write sq-magic)
    (display ")")
    (newline)

    (display "(define magic-db ")
    (write magic-db)
    (display ")")
    (newline)

    (display "(define zhash ")
    (write zhash)
    (display ")")
    (newline)))
