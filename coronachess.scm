(declare
	(standard-bindings)
	(extended-bindings)
	(not safe)
	(block))

;; ---- helper functions ----

(define (flatten x)
  (cond ((null? x) '())
		((not (pair? x)) (list x))
    (else (append (flatten (car x)) (flatten (cdr x))))))

(define (any-list->string x)
	(append-strings
		(map
			(lambda (x)
				(cond
					((string? x) (append-strings (list " " x)))
					((number? x) (append-strings (list " " (number->string x))))
					((symbol? x) (append-strings (list " " (symbol->string x))))
					(else x)))
			(flatten x))))

(define (filter op xs)
  (fold-right
    (lambda (next result) (if (op next) (cons next result) result))
    '()
    xs))

(define (any? pred l)
	(cond
		((null? l) #f)
		((pred (car l)) #t)
		(else (any? pred (cdr l)))))

(define (all? pred l)
	(cond
		((null? l) #t)
		((pred (car l)) (all? pred (cdr l)))
		(else #f)))

(define (string-split str sep)
	(call-with-input-string
		str
		(lambda (p)
			(read-all p (lambda (p) (read-line p sep))))))

(define (qsort lte lst)
  (if (or (null? lst) (<= (length lst) 1)) lst
    (let loop ((left '()) (right '())
               (pivot (car lst)) (rest (cdr lst)))
      (if (null? rest)
        (append (append (qsort lte left) (list pivot)) (qsort lte right))
        (if (lte (car rest) pivot)
          (loop (append left (list (car rest))) right pivot (cdr rest))
          (loop left (append right (list (car rest))) pivot (cdr rest)))))))

;; ---- chess data types and functions ----

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
(define promotions (list queen rook bishop knight))

(define (make-piece piece square) (s8vector piece square))

(define (piece-square piece) (s8vector-ref piece 1))

(define (get-piece piece) (s8vector-ref piece 0))

(define (is-white? piece) (fx< 0 (get-piece piece)))
(define (is-black? piece) (fx> 0 (get-piece piece)))
(define (piece-color piece) (if (is-white? piece) white black))
(define (piece-type piece) (abs (get-piece piece)))
(define (piece-file piece) (fx+ 1 (fxremainder (piece-square piece) 8)))
(define (piece-rank piece) (fx+ 1 (fxquotient (piece-square piece) 8)))
(define (square-file square) (fx+ 1 (fxremainder square 8)))
(define (square-rank square) (fx+ 1 (fxquotient square 8)))

(define (is-piece-at? piece square)
  (fx= square (piece-square piece)))

(define (make-move from to type)
 	(vector type from to))

(define (move-from move) (vector-ref move 1))

(define (move-to move) (vector-ref move 2))

(define (move-type move) (vector-ref move 0))

(define (is-capture? move)
	(equal? (move-type move) 'capture))

(define (is-short-castle? move)
	(equal? (move-type move) 'castle-short))

(define (is-long-castle? move)
	(equal? (move-type move) 'castle-long))

(define (is-castle? move)
	(or (is-short-castle? move) (is-long-castle? move)))

(define (is-promotion? move)
	(not (fx= (get-piece (move-from move)) (get-piece (move-to move)))))

(define (is-en-passant? move cp)
	(and
		(not (null? (chessp-ep cp))) (fx= pawn (piece-type (move-from move)))
		(is-capture? move) (fx= (piece-square (chessp-ep cp)) (piece-square (move-to move)))))

(define (piece-at square board)
  (cond
    ((null? board) '())
    ((is-piece-at? (car board) square) (car board))
    (else (piece-at square (cdr board)))))

(define (all-castle-rigths)
	(list
		(make-piece (fx* white king) 6)
		(make-piece (fx* white king) 2)
		(make-piece (fx* black king) 62)
		(make-piece (fx* black king) 58)))

(define (opposite-side side) (fx* -1 side))
(define (can-castle-to? square castle-rights)
	(not (null? (piece-at square castle-rights))))
(define (side-already-castled? move rights)
	(not (any? (lambda (c) (fx= (piece-color c) (piece-color (move-from move)))) rights)))

(define (start-board)
  (list
    (make-piece (* white rook) 0)
    (make-piece (* white knight) 1)
    (make-piece (* white bishop) 2)
    (make-piece (* white queen) 3)
    (make-piece (* white king) 4)
    (make-piece (* white bishop) 5)
    (make-piece (* white knight) 6)
    (make-piece (* white rook) 7)
    (make-piece (* white pawn) 8)
    (make-piece (* white pawn) 9)
    (make-piece (* white pawn) 10)
    (make-piece (* white pawn) 11)
    (make-piece (* white pawn) 12)
    (make-piece (* white pawn) 13)
    (make-piece (* white pawn) 14)
    (make-piece (* white pawn) 15)
    (make-piece (* black pawn) 48)
    (make-piece (* black pawn) 49)
    (make-piece (* black pawn) 50)
    (make-piece (* black pawn) 51)
    (make-piece (* black pawn) 52)
    (make-piece (* black pawn) 53)
    (make-piece (* black pawn) 54)
    (make-piece (* black pawn) 55)
    (make-piece (* black rook) 56)
    (make-piece (* black knight) 57)
    (make-piece (* black bishop) 58)
    (make-piece (* black queen) 59)
    (make-piece (* black king) 60)
    (make-piece (* black bishop) 61)
    (make-piece (* black knight) 62)
    (make-piece (* black rook) 63)))

(define (flatten-move-list x)
  (cond 
		((null? x) '())
		((vector? x) (list x))
    (else (append (flatten-move-list (car x)) (flatten-move-list (cdr x))))))

;; ---- chess position record ----

(define-structure chessp side castle ep hm fm ply zhash repetitions score bitbrd null-move?)

;; ---- bitboards functions ----

(include "bitmap-vectors.scm")

; used to access lines bitboard vector
(define (lines-idx sq1 sq2) (fx+ sq2 (fx* 64 sq1)))

; used to access segments bitboard vector
(define (between-idx sq1 sq2) (fx+ sq2 (fx* 64 sq1)))

; used to access files and ranks in lines bitboard vector
(define (file-idx file-nbr) (lines-idx file-nbr (fx+ file-nbr 8)))
(define (rank-idx rank-nbr) (lines-idx (fx* rank-nbr 8) (fx+ (fx* rank-nbr 8) 1)))

(define u64-and bitwise-and)
(define u64-ior bitwise-ior)
(define u64-xor bitwise-xor)
(define u64-not bitwise-not)
(define (u64-shift n i)	(u64-and #xFFFFFFFFFFFFFFFF (arithmetic-shift n i)))
(define (u64-mult a b) (u64-and #xFFFFFFFFFFFFFFFF (* a b)))
(define bitwise-bit-count bit-count)
(define bitscan-fwd first-bit-set)
(define (reset-ls1b bits) (u64-and bits (- bits 1)))

; return a bitmap
(define (square->bit square)
	(u64-shift 1 square))
(define (piece->bit piece)
	(u64-shift 1 (piece-square piece)))

(define (bits->squares bits)
	(if (zero? bits) '() (cons (bitscan-fwd bits) (bits->squares (reset-ls1b bits)))))

; return the front fill for the given bits of side
(define (front-fill side bits)
	(fold	(lambda (shift b) (u64-ior b (u64-shift b (fx* side shift))))
		bits '(8 16 32)))

(define (file-fill bits) (u64-ior (front-fill white bits) (front-fill black bits)))
(define (file-set bits) (u64-and #xFF (front-fill black bits)))

; frequently used files bitmap
(define ALL-FILES-BUT-A (u64-not (u64vector-ref lines (file-idx 0))))
(define ALL-FILES-BUT-H (u64-not (u64vector-ref lines (file-idx 7))))

;; ---- move & attacks generation functions ----

(define occupied-idx 0)
(define white-pieces-idx 13)
(define black-pieces-idx 14)
(define white-attacks-idx 15)
(define black-attacks-idx 16)
(define white-blockers-idx 17)
(define black-blockers-idx 18)

(define (bitbrd-idx2 side piece-type)
	; (if (fx= side white)
	; 	piece-type
	; 	(fx+ 6 piece-type)))
	(fx+ (fx* (fx- 1 side) 3) piece-type))

(define (bitbrd-idx piece)
	(bitbrd-idx2 (piece-color piece) (piece-type piece)))

(define (update-bitbrd-from-piece-bitbrd bitbrd)
	(u64vector-set! bitbrd white-pieces-idx
		(u64-ior
			(u64vector-ref bitbrd 1) (u64vector-ref bitbrd 2) (u64vector-ref bitbrd 3)
			(u64vector-ref bitbrd 4) (u64vector-ref bitbrd 5) (u64vector-ref bitbrd 6)))
	(u64vector-set! bitbrd black-pieces-idx
		(u64-ior
			(u64vector-ref bitbrd 7) (u64vector-ref bitbrd 8) (u64vector-ref bitbrd 9)
			(u64vector-ref bitbrd 10) (u64vector-ref bitbrd 11) (u64vector-ref bitbrd 12)))
	(u64vector-set! bitbrd occupied-idx
		(u64-ior
			(u64vector-ref bitbrd white-pieces-idx) (u64vector-ref bitbrd black-pieces-idx)))
	(u64vector-set! bitbrd white-attacks-idx
		(gen-all-attacks-bitbrd-for-side white bitbrd))
	(u64vector-set! bitbrd black-attacks-idx
		(gen-all-attacks-bitbrd-for-side black bitbrd))
	(u64vector-set! bitbrd white-blockers-idx
		(sliders-blockers bitbrd white (u64vector-ref bitbrd occupied-idx)))
	(u64vector-set! bitbrd black-blockers-idx
		(sliders-blockers bitbrd black (u64vector-ref bitbrd occupied-idx)))
	bitbrd)

; used for testing/debugging
(define (validate-bitbrd bitbrd)
	(if (zero? (u64vector-ref bitbrd (bitbrd-idx2 white king)))
		(error 'validate-bitbrd "White king missing"))
	(if (zero? (u64vector-ref bitbrd (bitbrd-idx2 black king)))
		(error 'validate-bitbrd "Black king missing"))
	(if (not (zero? 
				(u64-xor
					(u64-ior
						(u64vector-ref bitbrd 1) (u64vector-ref bitbrd 2) (u64vector-ref bitbrd 3)
						(u64vector-ref bitbrd 4) (u64vector-ref bitbrd 5) (u64vector-ref bitbrd 6))
					(u64vector-ref bitbrd white-pieces-idx))))
		(error 'validate-bitbrd (list "Incorrect White bitboard"
			(bits->squares (u64-xor
				(u64-ior
					(u64vector-ref bitbrd 1) (u64vector-ref bitbrd 2) (u64vector-ref bitbrd 3)
					(u64vector-ref bitbrd 4) (u64vector-ref bitbrd 5) (u64vector-ref bitbrd 6))
				(u64vector-ref bitbrd white-pieces-idx))))))
	(if (not (zero? 
				(u64-xor
					(u64-ior
						(u64vector-ref bitbrd 7) (u64vector-ref bitbrd 8) (u64vector-ref bitbrd 9)
						(u64vector-ref bitbrd 10) (u64vector-ref bitbrd 11) (u64vector-ref bitbrd 12))
					(u64vector-ref bitbrd black-pieces-idx))))
		(error 'validate-bitbrd (list "Incorrect Black bitboard"
			(bits->squares
				(u64-xor
					(u64-ior
						(u64vector-ref bitbrd 7) (u64vector-ref bitbrd 8) (u64vector-ref bitbrd 9)
						(u64vector-ref bitbrd 10) (u64vector-ref bitbrd 11) (u64vector-ref bitbrd 12))
					(u64vector-ref bitbrd black-pieces-idx))))))
	(if (not (zero? 
				(u64-xor
					(u64-ior
						(u64vector-ref bitbrd white-pieces-idx) (u64vector-ref bitbrd black-pieces-idx))
					(u64vector-ref bitbrd occupied-idx))))
		(error 'validate-bitbrd (list "Incorrect Occupied bitboard"
			(bits->squares
				(u64-xor
					(u64-ior
						(u64vector-ref bitbrd white-pieces-idx) (u64vector-ref bitbrd black-pieces-idx))
					(u64vector-ref bitbrd occupied-idx))))))
	bitbrd)

(define (init-bitbrd-from-board board)
	(let ((bitbrd (make-u64vector 19 0)))
		(for-each
			(lambda (piece)
				(u64vector-set!
					bitbrd
					(bitbrd-idx piece)
					(u64-ior
						(u64vector-ref bitbrd (bitbrd-idx piece))
						(piece->bit piece))))
			board)
		(update-bitbrd-from-piece-bitbrd bitbrd)))

(define (gen-pawn-moves-list cp pawn-set mask)
	(append (gen-pawn-moves-1sq cp pawn-set mask) (gen-pawn-moves-2sq cp pawn-set mask)))

(define (pawn-moves-available? cp pawn-set mask)
	(let* ((side (chessp-side cp))
				(space (u64-not (u64vector-ref (chessp-bitbrd cp) occupied-idx))))
		(or
			(not (zero? (u64-and space (u64-shift pawn-set (fx* side 8)) mask)))
			(let ((at-home (u64-and pawn-set (u64vector-ref lines (rank-idx (if (fx= side white) 1 6))))))
				(not (zero? (u64-and space mask
											(u64-shift 
												(u64-and space (u64-shift at-home (fx* side 8))) (fx* side 8)))))))))

(define (gen-pawn-moves-1sq cp pawn-set mask)
	(let* ((side (chessp-side cp))
				(pawns pawn-set)
				(space (u64-not (u64vector-ref (chessp-bitbrd cp) occupied-idx))))
		(map
			(lambda (square)
				(if (or (fx< square 8) (fx> square 55))
					(map
						(lambda (piece)
							(make-move
								(make-piece (fx* side pawn) (fx+ (fx* -8 side) square))
								(make-piece (fx* side piece) square)
								'move))
						promotions)
					(make-move
						(make-piece (fx* side pawn) (fx+ (fx* -8 side) square))
						(make-piece (fx* side pawn) square)
						'move)))
			(bits->squares (u64-and space (u64-shift pawns (fx* side 8)) mask)))))

(define (gen-pawn-moves-2sq cp pawn-set mask)
	(let* ((side (chessp-side cp))
				(space (u64-not (u64vector-ref (chessp-bitbrd cp) occupied-idx)))
				(at-home (u64-and pawn-set (u64vector-ref lines (rank-idx (if (fx= side white) 1 6))))))
		(map
			(lambda (square)
				(make-move
					(make-piece (fx* side pawn) (fx+ (fx* -16 side) square))
					(make-piece (fx* side pawn) square)
					'move))
			(bits->squares
				(u64-and space mask
					(u64-shift 
						(u64-and space (u64-shift at-home (fx* side 8))) (fx* side 8)))))))

(define (gen-pawn-attacks-bitbrd-east pawns side)
	(u64-and
		(u64-shift pawns (if (fx= side white) 9 -7))
		ALL-FILES-BUT-A))

(define (gen-pawn-attacks-bitbrd-west pawns side)
	(u64-and
		(u64-shift pawns (if (fx= side white) 7 -9))
		ALL-FILES-BUT-H))

(define (pawn-captures-list-east side pawns captures)
	(map
		(lambda (square)
			(if (fx= side white)
				(if (fx> square 55)
					(map (lambda (p)
								(make-move
									(make-piece (fx* white pawn) (fx- square 9))
									(make-piece (fx* white p) square) 'capture))
						promotions)
					(make-move
						(make-piece (fx* white pawn) (fx- square 9))
						(make-piece (fx* white pawn) square) 'capture))
				(if (fx< square 8)
					(map (lambda (p)
								(make-move
									(make-piece (fx* black pawn) (fx+ square 7))
									(make-piece (fx* black p) square) 'capture))
						promotions)
					(make-move
						(make-piece (fx* black pawn) (fx+ square 7))
						(make-piece (fx* black pawn) square) 'capture))))
		(bits->squares captures)))

(define (pawn-captures-list-west side pawns captures)
	(map
		(lambda (square)
			(if (fx= side white)
				(if (fx> square 55)
					(map (lambda (p)
									(make-move
										(make-piece (fx* white pawn) (fx- square 7))
										(make-piece (fx* white p) square) 'capture))
							promotions)
					(make-move
						(make-piece (fx* white pawn) (fx- square 7))
						(make-piece (fx* white pawn) square) 'capture))
				(if (fx< square 8)
					(map (lambda (p)
								(make-move
									(make-piece (fx* black pawn) (fx+ square 9))
									(make-piece (fx* black p) square) 'capture))
							promotions)
					(make-move
						(make-piece (fx* black pawn) (fx+ square 9))
						(make-piece (fx* black pawn) square) 'capture))))
		(bits->squares captures)))

(define (pawn-captures-available? cp pawn-set mask)
	(let* ((side (chessp-side cp))
				 (east (gen-pawn-attacks-bitbrd-east pawn-set side))
				 (west (gen-pawn-attacks-bitbrd-west pawn-set side))
				 (opponent (u64vector-ref (chessp-bitbrd cp)
				 															 (if (fx= side white) black-pieces-idx white-pieces-idx)))
				 (ep (if (null? (chessp-ep cp)) 0 (piece->bit (chessp-ep cp))))
				 (mask-ep (if (any-bits-set? (u64-shift ep (fx* side -8)) mask) (u64-ior ep mask) mask))) ; in case the mask includes a pawn to be captured, it should be captured also via ep if possible
		(if (not (zero? ep))
			(let ((w-ep (u64-and west ep)) (e-ep (u64-and east ep)))
				(cond
					((zero? (u64-xor w-ep e-ep))
						(or
							(not (zero? (u64-and east (u64-ior ep opponent) mask-ep)))
							(not (zero? (u64-and west (u64-ior ep opponent) mask-ep)))))
					((zero? w-ep) 
						; special verification for corner case
						(let* ((sq-victim (fx+ (piece-square (chessp-ep cp)) (fx* side -8)))
										(sq-k (bitscan-fwd (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side king))))
										(sq-attkr (fx+ (piece-square (chessp-ep cp)) (if (fx= side white) -9 7)))
										(blockers (u64-xor (u64vector-ref (chessp-bitbrd cp) occupied-idx) ep (square->bit sq-victim) (square->bit sq-attkr))))
							(if (zero? (slider-attackers-to-square (chessp-bitbrd cp) (opposite-side side) sq-k blockers))
								(or
									(not (zero? (u64-and east (u64-ior ep opponent) mask-ep)))
									(not (zero? (u64-and west (u64-ior ep opponent) mask-ep))))
								(or
									(not (zero? (u64-and east opponent mask)))
									(not (zero? (u64-and west opponent mask)))))))
					(else 
						; special verification for corner case
						(let* ((sq-victim (fx+ (piece-square (chessp-ep cp)) (fx* side -8)))
										(sq-k (bitscan-fwd (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side king))))
										(sq-attkr (fx+ (piece-square (chessp-ep cp)) (if (fx= side white) -7 9)))
										(blockers (u64-xor (u64vector-ref (chessp-bitbrd cp) occupied-idx) ep (square->bit sq-victim) (square->bit sq-attkr))))
							(if (zero? (slider-attackers-to-square (chessp-bitbrd cp) (opposite-side side) sq-k blockers))
								(or
									(not (zero? (u64-and east (u64-ior ep opponent) mask-ep)))
									(not (zero? (u64-and west (u64-ior ep opponent) mask-ep))))
								(or
									(not (zero? (u64-and east opponent mask)))
									(not (zero? (u64-and west opponent mask)))))))))
			(or
				(not (zero? (u64-and east opponent mask)))
				(not (zero? (u64-and west opponent mask)))))))	

(define (gen-pawn-captures-list cp pawn-set mask)
	(let* ((side (chessp-side cp))
				 (east (gen-pawn-attacks-bitbrd-east pawn-set side))
				 (west (gen-pawn-attacks-bitbrd-west pawn-set side))
				 (opponent (u64vector-ref (chessp-bitbrd cp)
				 															 (if (fx= side white) black-pieces-idx white-pieces-idx)))
				 (ep (if (null? (chessp-ep cp)) 0 (piece->bit (chessp-ep cp))))
				 (mask-ep (if (any-bits-set? (u64-shift ep (fx* side -8)) mask) (u64-ior ep mask) mask))) ; in case the mask includes a pawn to be captured, it should be captured also via ep if possible
		(if (not (zero? ep))
			(let ((w-ep (u64-and west ep)) (e-ep (u64-and east ep)))
				(cond
					((zero? (u64-xor w-ep e-ep))
						; (pretty-print "both side attack to ep")
						(append
							(pawn-captures-list-east side pawn-set (u64-and east (u64-ior ep opponent) mask-ep))
							(pawn-captures-list-west side pawn-set (u64-and west (u64-ior ep opponent) mask-ep))))
					((zero? w-ep)
						; (pretty-print "west side attack to ep")
						; special verification for corner case
						(let* ((sq-victim (fx+ (piece-square (chessp-ep cp)) (fx* side -8)))
										(sq-k (bitscan-fwd (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side king))))
										(sq-attkr (fx+ (piece-square (chessp-ep cp)) (if (fx= side white) -9 7)))
										(blockers (u64-xor (u64vector-ref (chessp-bitbrd cp) occupied-idx) ep (square->bit sq-victim) (square->bit sq-attkr))))
							; (pretty-print (list "victim sq" sq-victim "attkr sq" sq-attkr "calculated blockers" blockers
							; 	"sliders found" (slider-attackers-to-square (chessp-bitbrd cp) (opposite-side side) sq-k blockers)))
							(if (zero? (slider-attackers-to-square (chessp-bitbrd cp) (opposite-side side) sq-k blockers))
								(append
									(pawn-captures-list-east side pawn-set (u64-and east (u64-ior ep opponent) mask-ep))
									(pawn-captures-list-west side pawn-set (u64-and west (u64-ior ep opponent) mask-ep)))
								(append
									(pawn-captures-list-east side pawn-set (u64-and east opponent mask))
									(pawn-captures-list-west side pawn-set (u64-and west opponent mask))))))
					(else
						; (pretty-print "east side attack to ep")
						; special verification for corner case
						(let* ((sq-victim (fx+ (piece-square (chessp-ep cp)) (fx* side -8)))
										(sq-k (bitscan-fwd (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side king))))
										(sq-attkr (fx+ (piece-square (chessp-ep cp)) (if (fx= side white) -7 9)))
										(blockers (u64-xor (u64vector-ref (chessp-bitbrd cp) occupied-idx) ep (square->bit sq-victim) (square->bit sq-attkr))))
							; (pretty-print (list "victim sq" sq-victim "attkr sq" sq-attkr "calculated blockers" blockers
							; 	"sliders found" (slider-attackers-to-square (chessp-bitbrd cp) (opposite-side side) sq-k blockers)))
							(if (zero? (slider-attackers-to-square (chessp-bitbrd cp) (opposite-side side) sq-k blockers))
								(append
									(pawn-captures-list-east side pawn-set (u64-and east (u64-ior ep opponent) mask-ep))
									(pawn-captures-list-west side pawn-set (u64-and west (u64-ior ep opponent) mask-ep)))
								(append
									(pawn-captures-list-east side pawn-set (u64-and east opponent mask))
									(pawn-captures-list-west side pawn-set (u64-and west opponent mask))))))))
			(append
				(pawn-captures-list-east side pawn-set (u64-and east opponent mask))
				(pawn-captures-list-west side pawn-set (u64-and west opponent mask))))))	

(define (gen-piece-attacks-bitbrd bitbrd side piece blockers)
	(fold
		(lambda (square attacks)
			(u64-ior attacks (magic-attacks-bitbrd-for-square piece square blockers)))
		0
		(bits->squares (u64vector-ref bitbrd (bitbrd-idx2 side piece)))))

; side is the side of the attackers to be found
(define (attackers-to-square bitbrd side square blockers piece-types)
	(fold
		(lambda (piece attackers)
			(u64-ior attackers
				(if (fx= piece pawn)
					(let ((sq-bitbrd (square->bit square)))
						(u64-ior 
							(u64-and
								(u64-shift sq-bitbrd (if (fx= side white) -9 7))
								ALL-FILES-BUT-H
								blockers
								(u64vector-ref bitbrd (bitbrd-idx2 side pawn)))
							(u64-and
								(u64-shift sq-bitbrd (if (fx= side white) -7 9))
								ALL-FILES-BUT-A
								blockers
								(u64vector-ref bitbrd (bitbrd-idx2 side pawn)))))
					(u64-and
						blockers
						(u64vector-ref bitbrd (bitbrd-idx2 side piece))
						(magic-attacks-bitbrd-for-square piece square blockers)))))
		0
		piece-types))

(define (all-attackers-to-square bitbrd side square blockers)
	(attackers-to-square bitbrd side square blockers (list pawn knight bishop rook queen king)))

(define (slider-attackers-to-square bitbrd side square blockers)
	(attackers-to-square bitbrd side square blockers (list bishop rook queen)))

(define (gen-piece-moves-only piece side blockers squares mask)
	(map 
		(lambda (from-sq)
			(let ((attks (magic-attacks-bitbrd-for-square piece from-sq blockers))
						(p (fx* side piece)))
				(map
					(lambda (to-sq)
						(make-move (make-piece p from-sq)	(make-piece p to-sq) 'move))
					(bits->squares (u64-and (u64-not blockers) attks mask)))))
		squares))

(define (gen-piece-captures-only piece side opponent-bitbrd blockers squares mask)
	(map
		(lambda (from-sq)
			(let ((attks (magic-attacks-bitbrd-for-square piece from-sq blockers))
						(p (fx* side piece)))
				(map
					(lambda (to-sq)
						(make-move (make-piece p from-sq) (make-piece p to-sq) 'capture))
					(bits->squares (u64-and opponent-bitbrd attks mask)))))
		squares))

; opponent-bitbrd contains the opponent pieces
; blockers contains the the occupied squares
; mask resticts the available squares
(define (piece-moves-available? piece side opponent-bitbrd blockers squares mask)
	(any?
		(lambda (from-sq)
			(let* ((attks (magic-attacks-bitbrd-for-square piece from-sq blockers))
						 (moves (u64-ior
						 					(u64-and opponent-bitbrd attks mask)				; captures
											(u64-and (u64-not blockers) attks mask))))	; moves
				(not (zero? moves))))
		squares))

(define (gen-pawn-attacks-bitbrd bitbrd side)
	(u64-ior
		(gen-pawn-attacks-bitbrd-east (u64vector-ref bitbrd (bitbrd-idx2 side pawn)) side)
		(gen-pawn-attacks-bitbrd-west (u64vector-ref bitbrd (bitbrd-idx2 side pawn)) side)))

(define (piece-type-from-bitbrd-w-param bitbrd cp side pieces)
	(cond
		((null? pieces) 0) ; zero is returned when no bit set is found for the given pieces
		((and (fx= pawn (car pieces)) (not (null? (chessp-ep cp)))
					(any-bits-set? bitbrd (u64-ior 
																	(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side pawn))
																	(piece->bit (chessp-ep cp)))))
			pawn)
		((any-bits-set? bitbrd (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side (car pieces))))
			(car pieces))
		(else (piece-type-from-bitbrd-w-param bitbrd cp side (cdr pieces)))))

; return the smallest valued type of any attacker for any bits in the provided bitboard
; used in see 
(define (piece-type-from-bitbrd bitbrd cp side)
	(piece-type-from-bitbrd-w-param bitbrd cp side (list pawn knight bishop rook queen king)))

; used in move generation for pinners
(define (is-slider? bit cp side)
	(let ((type (piece-type-from-bitbrd-w-param bit cp side (list bishop rook queen))))
		(not (zero? type))))

; return bitboard with squares the side king can not move to
(define (king-danger-bitbrd-for-side side bitbrd)
	(let ((opponent (opposite-side side))
				(blockers (u64-xor (u64vector-ref bitbrd (bitbrd-idx2 side king))
													 (u64vector-ref bitbrd occupied-idx))))
		(u64-ior
			(gen-piece-attacks-bitbrd bitbrd opponent king blockers)
			(gen-piece-attacks-bitbrd bitbrd opponent queen blockers)
			(gen-piece-attacks-bitbrd bitbrd opponent rook blockers)
			(gen-piece-attacks-bitbrd bitbrd opponent bishop blockers)
			(gen-piece-attacks-bitbrd bitbrd opponent knight blockers)
			(gen-pawn-attacks-bitbrd bitbrd opponent))))

(define (aligned? sq1 sq2 sq3)
	(any-bits-set? (u64vector-ref lines (lines-idx sq1 sq2)) (square->bit sq3)))

(define (xray-rook-attacks occupied blockers square)
	(let ((attacks (magic-rook-attacks square occupied)))
		(u64-xor attacks
			(magic-rook-attacks square (u64-xor occupied (u64-and blockers attacks))))))

(define (xray-bishop-attacks occupied blockers square)
	(let ((attacks (magic-bishop-attacks square occupied)))
		(u64-xor attacks
			(magic-bishop-attacks square (u64-xor occupied (u64-and blockers attacks))))))

(define (pinned-pieces cp side)
	(u64-and
		(u64vector-ref (chessp-bitbrd cp) (if (fx= side white) white-blockers-idx black-blockers-idx))
		(u64vector-ref (chessp-bitbrd cp) (if (fx= side white) white-pieces-idx black-pieces-idx))))

; return a bitboard including pieces of both sides that block slider attacks to the king of the give side
; blockers should be the occupied bitboard or any other restrictive mask (maybe I should remove this param...)
(define (sliders-blockers bitbrd side blockers)
	(let ((k-sq (bitscan-fwd (u64vector-ref bitbrd (bitbrd-idx2 side king))))
				(all-pieces (u64vector-ref bitbrd occupied-idx)))
		(fold
			(lambda (square pinned)
				(u64-ior pinned (u64-and blockers (u64vector-ref segments (between-idx k-sq square)))))
			0
			(append
				(bits->squares
					(u64-and
						(xray-rook-attacks blockers all-pieces k-sq)
						(u64-ior
							(u64vector-ref bitbrd (bitbrd-idx2 (opposite-side side) rook))
							(u64vector-ref bitbrd (bitbrd-idx2 (opposite-side side) queen)))))
				(bits->squares
					(u64-and
						(xray-bishop-attacks blockers all-pieces k-sq)
						(u64-ior
							(u64vector-ref bitbrd (bitbrd-idx2 (opposite-side side) bishop))
							(u64vector-ref bitbrd (bitbrd-idx2 (opposite-side side) queen)))))))))

(define (pinned-moves-available? cp pinned side)
	(if (zero? pinned)
		#f
		(let* ((bitbrd (chessp-bitbrd cp))
					(occ (u64-xor (u64vector-ref bitbrd occupied-idx) pinned))
					(k-sq (bitscan-fwd (u64vector-ref bitbrd (bitbrd-idx2 side king))))
					(attk (slider-attackers-to-square bitbrd (opposite-side side) k-sq occ)))
			(any? 
				(lambda (attk-square)
					(let* ((pinner (square->bit attk-square))
								 (ray (u64-ior pinner (u64vector-ref segments (between-idx attk-square k-sq))))
								 (bit (u64-and pinned ray)) (square (bitscan-fwd bit)))
						(cond
							((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side pawn)))
								(or
									(pawn-captures-available? cp bit ray)
									(pawn-moves-available? cp bit ray)))
							((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side bishop)))
								(piece-moves-available? bishop side pinner occ (list square) ray))
							((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side rook)))
								(piece-moves-available? rook side pinner occ (list square) ray))
							((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side queen)))
								(piece-moves-available? queen side pinner occ (list square) ray))
							(else #f ))))
				(bits->squares attk)))))

(define (gen-pinned-moves cp pinned side)
	(if (zero? pinned)
		'()
		(let* ((bitbrd (chessp-bitbrd cp))
					(occ (u64-xor (u64vector-ref bitbrd occupied-idx) pinned))
					(k-sq (bitscan-fwd (u64vector-ref bitbrd (bitbrd-idx2 side king))))
					(attk (slider-attackers-to-square bitbrd (opposite-side side) k-sq occ)))
			(map 
				(lambda (attk-square)
					(let* ((pinner (square->bit attk-square))
								 (ray (u64-ior pinner (u64vector-ref segments (between-idx attk-square k-sq))))
								 (bit (u64-and pinned ray)) (square (bitscan-fwd bit)))
						(cond
							((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side pawn)))
								(append
									(gen-pawn-captures-list cp bit ray)
									(gen-pawn-moves-list cp bit ray)))
							((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side bishop)))
								(append
									(gen-piece-captures-only bishop side pinner occ (list square) ray)
									(gen-piece-moves-only bishop side occ (list square) ray)))
							((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side rook)))
								(append
									(gen-piece-captures-only rook side pinner occ (list square) ray)
									(gen-piece-moves-only rook side occ (list square) ray)))
							((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side queen)))
								(append
									(gen-piece-captures-only queen side pinner occ (list square) ray)
									(gen-piece-moves-only queen side occ (list square) ray)))
							(else '() ))))
				(bits->squares attk)))))

(define (gen-castle-moves cp)
	(append
		(if (and (fx= (chessp-side cp) white) (can-castle-to? 6 (chessp-castle cp)))
			(if (or
						(any-bits-set? (u64vector-ref (chessp-bitbrd cp) occupied-idx) w-castle-short-mask)
						(any-bits-set? (u64vector-ref (chessp-bitbrd cp) black-attacks-idx) w-castle-short))
				'()
				(list (make-move (make-piece (fx* white king) 4) (make-piece (fx* white king) 6) 'castle-short )))
			'())
		(if (and (fx= (chessp-side cp) white) (can-castle-to? 2 (chessp-castle cp)))
			(if (or
						(any-bits-set? (u64vector-ref (chessp-bitbrd cp) occupied-idx) w-castle-long-mask)
						(any-bits-set? (u64vector-ref (chessp-bitbrd cp) black-attacks-idx) w-castle-long))
				'()
				(list (make-move (make-piece (fx* white king) 4) (make-piece (fx* white king) 2) 'castle-long )))
			'())
		(if (and (fx= (chessp-side cp) black) (can-castle-to? 62 (chessp-castle cp)))
			(if (or
						(any-bits-set? (u64vector-ref (chessp-bitbrd cp) occupied-idx) b-castle-short-mask)
						(any-bits-set? (u64vector-ref (chessp-bitbrd cp) white-attacks-idx) b-castle-short))
				'()
				(list (make-move (make-piece (fx* black king) 60) (make-piece (fx* black king) 62) 'castle-short )))
			'())
		(if (and (fx= (chessp-side cp) black) (can-castle-to? 58 (chessp-castle cp)))
			(if (or
						(any-bits-set? (u64vector-ref (chessp-bitbrd cp) occupied-idx) b-castle-long-mask)
						(any-bits-set? (u64vector-ref (chessp-bitbrd cp) white-attacks-idx) b-castle-long))
				'()
				(list (make-move (make-piece (fx* black king) 60) (make-piece (fx* black king) 58) 'castle-long )))
			'())))

(define (print-bitbrd bits)
	(newline)
	(let loop ((i 56))
		(display (if (bit-set?    i    bits) "1" "0"))
		(display (if (bit-set? (+ i 1) bits) "1" "0"))
		(display (if (bit-set? (+ i 2) bits) "1" "0"))
		(display (if (bit-set? (+ i 3) bits) "1" "0"))
		(display (if (bit-set? (+ i 4) bits) "1" "0"))
		(display (if (bit-set? (+ i 5) bits) "1" "0"))
		(display (if (bit-set? (+ i 6) bits) "1" "0"))
		(display (if (bit-set? (+ i 7) bits) "1" "0"))
		(newline)
		(if (> i 0)
			(loop (- i 8))
			(newline))))

(define (bitbrd-piece-at square bitbrd)
	(let* ((bit (square->bit square))
				 (side (if (any-bits-set? bit (u64vector-ref bitbrd white-pieces-idx)) white black)))
		(cond
			((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side pawn)))
				(make-piece (fx* side pawn) square))
			((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side knight)))
				(make-piece (fx* side knight) square))
			((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side bishop)))
				(make-piece (fx* side bishop) square))
			((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side rook)))
				(make-piece (fx* side rook) square))
			((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side queen)))
				(make-piece (fx* side queen) square))
			((any-bits-set? bit (u64vector-ref bitbrd (bitbrd-idx2 side king)))
				(make-piece (fx* side king) square))
			(else
				(error 'bitbrd-piece-at (list square bitbrd)) ))))

;; ---- magic bitboards ----

(define magic-bishop-idx (fx* 64 512)) ; 9 bits (so you shift by 64-9 = 55)
(define magic-rook-idx (fx* 64 4096)) ; 12 bits (so you shift by 64-12 = 52)

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

(define (magic-attacks-bitbrd-for-square piece square blockers)
	(cond
		((fx= piece king) (u64vector-ref king-bitbrd square))
		((fx= piece knight) (u64vector-ref knight-bitbrd square))
		((fx= piece rook) (magic-rook-attacks square blockers))
		((fx= piece bishop) (magic-bishop-attacks square blockers))
		(else
			(u64-ior
				(magic-rook-attacks square blockers)
				(magic-bishop-attacks square blockers)))))

;; ---- eval functions ----

; score = materialWeight * (numWhitePieces - numBlackPieces) * who2move 
; https://www.chessprogramming.org/Simplified_Evaluation_Function
(define checkmate-score 2000000)
(define MATE (fx- checkmate-score 1000))
(define material-score (make-vector (fx+ 8 (fx* 64 14)) 0))
(define king-endgame 7)
(vector-set! material-score 0							1) ; mobility
(vector-set! material-score pawn				100)
(vector-set! material-score knight			320)
(vector-set! material-score bishop			330)
(vector-set! material-score rook				500)
(vector-set! material-score queen				900)
(vector-set! material-score king				  0)
(vector-set! material-score king-endgame  0)

(define (pt-idx piece square)
	(if (fx< piece 0)
		(fx+ 8 (fx* 64 (fx- (fx- piece) 1)) (fx* 64 7) square)
		(fx+ 8 (fx* 64 (fx- piece 1)) square)))

(define (pt-idx2 piece square cp)
	(if (fx= king (abs piece))
		(if (is-endgame? cp)
			(if (fx< piece 0)
				(pt-idx (fx- king-endgame) square)
				(pt-idx king-endgame square))
			(pt-idx piece square))
		(pt-idx piece square)))

; endgame requires no queen or queen and only one minor piece for both sides
(define (is-endgame? cp)
	(and	
		(or
			(zero? (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 white queen)))
			(and
				(zero? (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 white rook)))
				(fx= 1 (bitwise-bit-count 
												(u64-xor	(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 white bishop))
																	(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 white knight)))))))
		(or
			(zero? (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 black queen)))
			(and
				(zero? (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 black rook)))
				(fx= 1 (bitwise-bit-count 
												(u64-xor	(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 black bishop))
																	(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 black knight)))))))
	))

; return if side is playing with only king and pawns
; used to avoid null move pruning
(define (kp-only? cp)
	(zero?
		(u64-ior
			(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 (chessp-side cp) queen))
			(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 (chessp-side cp) rook))
			(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 (chessp-side cp) bishop))
			(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 (chessp-side cp) knight)))))

(define (init-pawn-piece-table)
	(do ((i 0 (+ 1 i))
			 (piece pawn)
			 (pt (list
	  0   0   0   0   0   0   0   0
	  5  10  10 -20 -20  10  10   5
	  5  -5 -10   0   0 -10  -5   5
	  0   0   0  20  20   0   0   0
	  5   5  10  25  25  10   5   5
	 10  10  20  30  30  20  10  10
	 50  50  50  50  50  50  50  50
	  0   0   0   0   0   0   0   0
	)))
		((= i 64) 'ok)
		(vector-set! material-score (pt-idx (* white piece) i)
			(fx+ (vector-ref material-score piece) (list-ref pt i)))
		(vector-set! material-score (pt-idx (* black piece) i)
			(fx+ (vector-ref material-score piece) (list-ref (reverse pt) i)))))
(init-pawn-piece-table)

(define (init-knight-piece-table)
	(do ((i 0 (+ 1 i))
			 (piece knight)
			 (pt (list
	-50 -40 -30 -30 -30 -30 -40 -50
	-40 -20   0   5   5   0 -20 -40
	-30   5  10  15  15  10   5 -30
	-30   0  15  20  20  15   0 -30
	-30   5  15  20  20  15   5 -30
	-30   0  10  15  15  10   0 -30
	-40 -20   0   0   0   0 -20 -40
	-50 -40 -30 -30 -30 -30 -40 -50
	)))
		((= i 64) 'ok)
		(vector-set! material-score (pt-idx (* white piece) i)
			(fx+ (vector-ref material-score piece) (list-ref pt i)))
		(vector-set! material-score (pt-idx (* black piece) i)
			(fx+ (vector-ref material-score piece) (list-ref (reverse pt) i)))))
(init-knight-piece-table)

(define (init-bishop-piece-table)
	(do ((i 0 (+ 1 i))
			 (piece bishop)
			 (pt (list
	-20 -10 -10 -10 -10 -10 -10 -20
	-10   5   0   0   0   0   5 -10
	-10  10  10  10  10  10  10 -10
	-10   0  10  10  10  10   0 -10
	-10   5   5  10  10   5   5 -10
	-10   0   5  10  10   5   0 -10
	-10   0   0   0   0   0   0 -10
	-20 -10 -10 -10 -10 -10 -10 -20
	)))
		((= i 64) 'ok)
		(vector-set! material-score (pt-idx (* white piece) i)
			(fx+ (vector-ref material-score piece) (list-ref pt i)))
		(vector-set! material-score (pt-idx (* black piece) i)
			(fx+ (vector-ref material-score piece) (list-ref (reverse pt) i)))))
(init-bishop-piece-table)

(define (init-rook-piece-table)
	(do ((i 0 (+ 1 i))
			 (piece rook)
			 (pt (list
	  0   0   0   5   5   0   0   0
	 -5   0   0   0   0   0   0  -5
	 -5   0   0   0   0   0   0  -5
	 -5   0   0   0   0   0   0  -5
	 -5   0   0   0   0   0   0  -5
	 -5   0   0   0   0   0   0  -5
	  5  10  10  10  10  10  10   5
		0   0   0   0   0   0   0   0
	)))
		((= i 64) 'ok)
		(vector-set! material-score (pt-idx (* white piece) i)
			(fx+ (vector-ref material-score piece) (list-ref pt i)))
		(vector-set! material-score (pt-idx (* black piece) i)
			(fx+ (vector-ref material-score piece) (list-ref (reverse pt) i)))))
(init-rook-piece-table)

(define (init-queen-piece-table)
	(do ((i 0 (+ 1 i))
			 (piece queen)
			 (pt (list
	-20 -10 -10  -5  -5 -10 -10 -20
	-10   0   5   0   0   0   0 -10
	-10   5   5   5   5   5   0 -10
	  0   0   5   5   5   5   0  -5
	 -5   0   5   5   5   5   0  -5
	-10   0   5   5   5   5   0 -10
	-10   0   0   0   0   0   0 -10
	-20 -10 -10  -5  -5 -10 -10 -20
	)))
		((= i 64) 'ok)
		(vector-set! material-score (pt-idx (* white piece) i)
			(fx+ (vector-ref material-score piece) (list-ref pt i)))
		(vector-set! material-score (pt-idx (* black piece) i)
			(fx+ (vector-ref material-score piece) (list-ref (reverse pt) i)))))
(init-queen-piece-table)

(define (init-king-piece-table)
	(do ((i 0 (+ 1 i))
			 (piece king)
			 (pt (list
	 20  30  10   0   0  10  30  20
	 20  20   0   0   0   0  20  20
	-10 -20 -20 -20 -20 -20 -20 -10
	-20 -30 -30 -40 -40 -30 -30 -20
	-30 -40 -40 -50 -50 -40 -40 -30
	-30 -40 -40 -50 -50 -40 -40 -30
	-30 -40 -40 -50 -50 -40 -40 -30
	-30 -40 -40 -50 -50 -40 -40 -30
	)))
		((= i 64) 'ok)
		(vector-set! material-score (pt-idx (* white piece) i)
			(fx+ (vector-ref material-score piece) (list-ref pt i)))
		(vector-set! material-score (pt-idx (* black piece) i)
			(fx+ (vector-ref material-score piece) (list-ref (reverse pt) i)))))
(init-king-piece-table)

(define (init-king-endgame-piece-table)
	(do ((i 0 (+ 1 i))
			 (piece king-endgame)
			 (pt (list
	-50 -40 -30 -20 -20 -30 -40 -50
	-30 -20 -10   0   0 -10 -20 -30
	-30 -10  20  30  30  20 -10 -30
	-30 -10  30  40  40  30 -10 -30
	-30 -10  30  40  40  30 -10 -30
	-30 -10  20  30  30  20 -10 -30
	-30 -30   0   0   0   0 -30 -30
	-50 -30 -30 -30 -30 -30 -30 -50
	)))
		((= i 64) 'ok)
		(vector-set! material-score (pt-idx (* white piece) i)
			(fx+ (vector-ref material-score piece) (list-ref pt i)))
		(vector-set! material-score (pt-idx (* black piece) i)
			(fx+ (vector-ref material-score piece) (list-ref (reverse pt) i)))))
(init-king-endgame-piece-table)

(define (set-board-score! cp)
	(chessp-score-set! cp
		(apply +
			(map
				(lambda (piece)
					(fx* (piece-color piece) 
						(vector-ref material-score
							(pt-idx2 (get-piece piece) (piece-square piece) cp))))
				(chessp->board cp))))
	cp)

(define (passed-pawns side white-pawns black-pawns)
	(let* ((front (front-fill (opposite-side side) (if (fx= white side) black-pawns white-pawns)))
					(front-e (u64-and ALL-FILES-BUT-A (u64-shift front 1)))
					(front-w (u64-and ALL-FILES-BUT-H (u64-shift front -1))))
		(bitwise-bit-count
			(if (fx= white side)
				(u64-and white-pawns (u64-not (u64-ior front front-e front-w)))
				(u64-and black-pawns (u64-not (u64-ior front front-e front-w)))))))

(define (isolated-pawns pawns)
	(let ((fill (file-fill pawns)))
		(bitwise-bit-count
			(u64-and
				pawns
				(u64-not (u64-and ALL-FILES-BUT-A (u64-shift fill 1)))
				(u64-not (u64-and ALL-FILES-BUT-H (u64-shift fill -1)))))))

(define white-space (u64-and
											(u64-ior (u64vector-ref lines (file-idx 2)) (u64vector-ref lines (file-idx 3))
																(u64vector-ref lines (file-idx 4)) (u64vector-ref lines (file-idx 5)))
											(u64-ior (u64vector-ref lines (rank-idx 1)) (u64vector-ref lines (rank-idx 2))
																(u64vector-ref lines (rank-idx 3)))))
(define black-space (u64-and
											(u64-ior (u64vector-ref lines (file-idx 2)) (u64vector-ref lines (file-idx 3))
																(u64vector-ref lines (file-idx 4)) (u64vector-ref lines (file-idx 5)))
											(u64-ior (u64vector-ref lines (rank-idx 6)) (u64vector-ref lines (rank-idx 5))
																(u64vector-ref lines (rank-idx 4)))))

(define (king-safety side cp)
	(let* ((k-bit (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side king)))
				 (k-square (bitscan-fwd k-bit))
				 (k-squares (u64-ior k-bit (u64vector-ref king-bitbrd k-square)))
				 (k-attkd (u64-and k-squares (if (fx= white side)
				 																(u64vector-ref (chessp-bitbrd cp) black-attacks-idx)
				 																(u64vector-ref (chessp-bitbrd cp) white-attacks-idx))))
;				 (blockers (u64vector-ref (chessp-bitbrd cp) occupied-idx)))
				 (blockers 0))
		(fx*
			(bitwise-bit-count k-attkd)
			(fxquotient
				(fold
					(lambda (piece danger)
						(fx+ danger
							(fold
								(lambda (square danger)
									(if (any-bits-set? k-squares (magic-attacks-bitbrd-for-square piece square blockers))
										(fx+ danger (vector-ref material-score piece))
										danger))
								0
								(bits->squares (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 (opposite-side side) piece))))))
					0
					(list queen rook bishop knight))
				-100))))

(define (bit-board side piece cp) (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side piece)))

(define (square-color bit) (u64-and 1 (u64-shift #xAA55AA55AA55AA55 (fx- bit))))

; https://www.chessprogramming.org/Draw_Evaluation
; https://www.chessprogramming.org/KPK
(define (draw-evaluation cp)
	(if	(and (zero? (bit-board white pawn cp))  (zero? (bit-board black pawn cp))
					 (zero? (bit-board white queen cp)) (zero? (bit-board white rook cp))
					 (zero? (bit-board black queen cp)) (zero? (bit-board black rook cp)))
		(let ((wb (bits->squares (bit-board white bishop cp))) (bb (bits->squares (bit-board black bishop cp))))
			(and
				(not (and (> (length wb) 1) (null? bb) (not (fx= (square-color (car wb)) (square-color (cadr wb))))))
				(not (and (> (length bb) 1) (null? wb) (not (fx= (square-color (car bb)) (square-color (cadr bb))))))))
		#f))

(define (score-estimate cp)	(fx* (chessp-side cp) (chessp-score cp)))

(define (score cp)
	(if (draw-evaluation cp)
		0
		(let ((end-game? (is-endgame? cp)))
			(fx* (chessp-side cp)
				(fx+
					(fx* (vector-ref material-score 0) 	
						(fx- (bitwise-bit-count (u64vector-ref (chessp-bitbrd cp) white-attacks-idx))
								(bitwise-bit-count (u64vector-ref (chessp-bitbrd cp) black-attacks-idx))))
					(fx* (if end-game? 50 20) 
						(fx-
							(passed-pawns white
								(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 white pawn))
								(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 black pawn)))
							(passed-pawns black
								(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 white pawn))
								(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 black pawn)))))
					(fx* (if end-game? -15 -25)
						(fx-
							(isolated-pawns (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 white pawn)))
							(isolated-pawns (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 black pawn)))))
					(fx* (if end-game? 1 3)
						(fx- (bitwise-bit-count (u64-xor white-space (u64vector-ref (chessp-bitbrd cp) black-attacks-idx)))
								(bitwise-bit-count (u64-xor black-space (u64vector-ref (chessp-bitbrd cp) white-attacks-idx)))))
					(fx- (king-safety white cp) (king-safety black cp))
					(chessp-score cp))))))

; https://www.chessprogramming.org/Static_Exchange_Evaluation
(define (see-recur cp square side blockers piece gain)
	(let* ((opp-side (opposite-side side))
				 (attkr (all-attackers-to-square (chessp-bitbrd cp) side square blockers)))
		(if (zero? attkr)
			(fx- gain)
			(let ((new-gain (fx- (vector-ref material-score (pt-idx (fx* side piece) square)) gain)))
				; (pretty-print (list "see" side piece new-gain))
				(if (fx> (fx- (max (fx- gain) new-gain)) 0) ; if the capture is losing material stop here
					(fx- gain)
					(let*	((small-attkr (piece-type-from-bitbrd attkr cp side))
								 (all-attkr (u64-and attkr
														(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side small-attkr))))
								 (curr-attkr (if (> (bitwise-bit-count all-attkr) 1)
															(square->bit (bitscan-fwd all-attkr)) all-attkr)))
						(fx- (max (fx- new-gain)
							(see-recur cp square opp-side (u64-xor blockers curr-attkr) small-attkr new-gain)))))))))

(define (see move cp)
	(let* ((opp-side (opposite-side (chessp-side cp)))
				 (square (piece-square (move-to move)))
				 (piece (piece-type-from-bitbrd (square->bit square) cp opp-side))
				 (blockers (u64vector-ref (chessp-bitbrd cp) occupied-idx))
				 (gain (vector-ref material-score (pt-idx (fx* opp-side piece) square))))
		; (pretty-print (list "see" (chessp-side cp) piece (piece-type (move-from move)) gain))
		(fx- (max (fx- gain)
			(see-recur cp square opp-side (u64-xor blockers (piece->bit (move-from move))) (piece-type (move-from move)) gain)))))

;; ---- board functions ----

(define (chessp->board cp)
	(map (lambda (square) (bitbrd-piece-at square (chessp-bitbrd cp)))
		(bits->squares (u64vector-ref (chessp-bitbrd cp) occupied-idx))))

(define (board-new)
	(let* ((b (start-board)) (z-hash (zobrist-hash white '() (all-castle-rigths) b)))
		(set-board-score! (make-chessp
			white (all-castle-rigths) '() 0 1 0 z-hash (list z-hash) 0 (init-bitbrd-from-board b) #f))))

(define (gen-legal-moves cp)
	(if (legal-moves-available? cp)
		(apply append (map force (gen-legal-moves-delayed 'all cp)))
		'()))

(define (legal-moves-available? cp)
	(let* ((side (chessp-side cp))
				 (opp (u64vector-ref (chessp-bitbrd cp)
				 			 (if (fx= side white) black-pieces-idx white-pieces-idx)))
				 (blockers (u64vector-ref (chessp-bitbrd cp) occupied-idx))
				 (pinned (pinned-pieces cp side))
				 (k-list (bits->squares (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side king))))
				 (q-list (bits->squares
				 						(u64-xor
											(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side queen))
											(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side queen))))))
				 (r-list (bits->squares
				 						(u64-xor
											(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side rook))
											(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side rook))))))
				 (b-list (bits->squares
				 						(u64-xor
											(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side bishop))				 
										 	(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side bishop))))))
				 (n-list (bits->squares
				 						(u64-xor
											(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side knight))
											(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side knight))))))
				 (pawn-set (u64-xor
										(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side pawn))
										(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side pawn)))))
				 (k-safe (u64-not (king-danger-bitbrd-for-side side (chessp-bitbrd cp)))))
		(if (not (under-check? cp))
			(or
				(pawn-moves-available? cp pawn-set #xFFFFFFFFFFFFFFFF)
				(piece-moves-available? queen side opp blockers q-list #xFFFFFFFFFFFFFFFF)
				(piece-moves-available? rook side opp blockers r-list #xFFFFFFFFFFFFFFFF)
				(piece-moves-available? bishop side opp blockers b-list #xFFFFFFFFFFFFFFFF)
				(piece-moves-available? knight side opp blockers n-list #xFFFFFFFFFFFFFFFF)
				(piece-moves-available? king side opp blockers k-list k-safe)
				(pawn-captures-available? cp pawn-set #xFFFFFFFFFFFFFFFF)
				(fx> (length (gen-castle-moves cp)) 0)
				(pinned-moves-available? cp pinned side))
			(let ((checkers (all-attackers-to-square (chessp-bitbrd cp) (opposite-side side)
				  						(car k-list) blockers)))
				(if (fx> (bitwise-bit-count checkers) 1) ; double check
					(piece-moves-available? king side opp blockers k-list k-safe)
					(if (is-slider? checkers cp (opposite-side side))
						(let* ((ray (u64-ior checkers
													(u64vector-ref segments
														(between-idx (bitscan-fwd checkers) (car k-list))))))
							(or
								(piece-moves-available? king side opp blockers k-list k-safe)
								(pawn-moves-available? cp pawn-set ray)
								(piece-moves-available? queen side checkers blockers q-list ray)
								(piece-moves-available? rook side checkers blockers r-list ray)
								(piece-moves-available? bishop side checkers blockers b-list ray)
								(piece-moves-available? knight side checkers blockers n-list ray)
								(pawn-captures-available? cp pawn-set checkers)))
						(or
							(piece-moves-available? king side opp blockers k-list k-safe)
							(pawn-captures-available? cp pawn-set checkers)
							(piece-moves-available? queen side checkers blockers q-list checkers)
							(piece-moves-available? rook side checkers blockers r-list checkers)
							(piece-moves-available? bishop side checkers blockers b-list checkers)
							(piece-moves-available? knight side checkers blockers n-list checkers))))))))

; type is 'qsearch for only quiesce search relevant moves
;         else for regular search moves
(define (gen-legal-moves-delayed type cp)
	(let* ((side (chessp-side cp))
				 (opp (u64vector-ref (chessp-bitbrd cp)
				 			 (if (fx= side white) black-pieces-idx white-pieces-idx)))
				 (blockers (u64vector-ref (chessp-bitbrd cp) occupied-idx))
				 (pinned (pinned-pieces cp side))
				 (k-list (bits->squares (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side king))))
				 (q-list (bits->squares
				 						(u64-xor
											(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side queen))
											(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side queen))))))
				 (r-list (bits->squares
				 						(u64-xor
											(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side rook))
											(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side rook))))))
				 (b-list (bits->squares
				 						(u64-xor
											(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side bishop))				 
										 	(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side bishop))))))
				 (n-list (bits->squares
				 						(u64-xor
											(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side knight))
											(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side knight))))))
				 (pawn-promovable
				 				(u64-and
								 	(u64-not (u64vector-ref lines (rank-idx (if (fx= side white) 6 1))))
				 					(u64-xor
										(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side pawn))
										(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side pawn))))))
				 (pawn-set (u64-xor
										pawn-promovable
										(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side pawn))
										(u64-and pinned (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 side pawn)))))
				 (k-safe (u64-not (king-danger-bitbrd-for-side side (chessp-bitbrd cp)))))
		(if (not (under-check? cp))
			(list
				(delay
					(flatten-move-list (append
						(gen-piece-captures-only king side opp blockers k-list k-safe)
						(gen-piece-captures-only queen side opp blockers q-list #xFFFFFFFFFFFFFFFF)
						(gen-piece-captures-only rook side opp blockers r-list #xFFFFFFFFFFFFFFFF)
						(gen-piece-captures-only bishop side opp blockers b-list #xFFFFFFFFFFFFFFFF)
						(gen-piece-captures-only knight side opp blockers n-list #xFFFFFFFFFFFFFFFF)
						(gen-pawn-captures-list cp pawn-set #xFFFFFFFFFFFFFFFF)
						(gen-pawn-captures-list cp pawn-promovable #xFFFFFFFFFFFFFFFF)
						(gen-pinned-moves cp pinned side))))
				(if (equal? type 'qsearch)
					(delay
						(flatten-move-list
							(gen-pawn-moves-list cp pawn-promovable #xFFFFFFFFFFFFFFFF)))
					(delay
						(flatten-move-list (append
							(gen-pawn-moves-list cp pawn-promovable #xFFFFFFFFFFFFFFFF)
							(gen-castle-moves cp)
							(gen-piece-moves-only king side blockers k-list k-safe)
							(gen-piece-moves-only queen side blockers q-list #xFFFFFFFFFFFFFFFF)
							(gen-piece-moves-only rook side blockers r-list #xFFFFFFFFFFFFFFFF)
							(gen-piece-moves-only bishop side blockers b-list #xFFFFFFFFFFFFFFFF)
							(gen-piece-moves-only knight side blockers n-list #xFFFFFFFFFFFFFFFF)
							(gen-pawn-moves-list cp pawn-set #xFFFFFFFFFFFFFFFF))))))
			(let ((checkers (all-attackers-to-square (chessp-bitbrd cp) (opposite-side side)
				  						(car k-list) blockers)))
				(if (fx> (bitwise-bit-count checkers) 1) ; double check
					(list
						(if (equal? type 'qsearch)
							(delay
								(flatten-move-list 
									(gen-piece-captures-only king side opp blockers k-list k-safe)))
							(delay
								(flatten-move-list (append
									(gen-piece-captures-only king side opp blockers k-list k-safe)
									(gen-piece-moves-only king side blockers k-list k-safe))))))
					(if (is-slider? checkers cp (opposite-side side))
						(let ((ray (u64-ior checkers
												(u64vector-ref segments
													(between-idx (bitscan-fwd checkers) (car k-list))))))
							(list
								(delay
									(flatten-move-list (append
										(gen-piece-captures-only king side opp blockers k-list k-safe)
										(gen-piece-captures-only queen side checkers blockers q-list ray)
										(gen-piece-captures-only rook side checkers blockers r-list ray)
										(gen-piece-captures-only bishop side checkers blockers b-list ray)
										(gen-piece-captures-only knight side checkers blockers n-list ray)
										(gen-pawn-captures-list cp pawn-set checkers)
										(gen-pawn-captures-list cp pawn-promovable checkers))))
								(if (equal? type 'qsearch)
									(delay
										(flatten-move-list
											(gen-pawn-moves-list cp pawn-promovable ray)))
									(delay
										(flatten-move-list (append
											(gen-pawn-moves-list cp pawn-promovable ray)
											(gen-piece-moves-only king side blockers k-list k-safe)
											(gen-piece-moves-only queen side blockers q-list ray)
											(gen-piece-moves-only rook side blockers r-list ray)
											(gen-piece-moves-only bishop side blockers b-list ray)
											(gen-piece-moves-only knight side blockers n-list ray)
											(gen-pawn-moves-list cp pawn-set ray)))))))
						(list
							(delay
								(flatten-move-list (append
									(gen-piece-captures-only king side opp blockers k-list k-safe)
									(gen-piece-captures-only queen side checkers blockers q-list checkers)
									(gen-piece-captures-only rook side checkers blockers r-list checkers)
									(gen-piece-captures-only bishop side checkers blockers b-list checkers)
									(gen-piece-captures-only knight side checkers blockers n-list checkers)
									(gen-pawn-captures-list cp pawn-set checkers)
									(gen-pawn-captures-list cp pawn-promovable checkers)
									(gen-piece-moves-only king side blockers k-list k-safe)))))))))))

(define (gen-all-attacks-bitbrd-for-side side bitbrd)
	(u64-ior
		(gen-piece-attacks-bitbrd bitbrd side king (u64vector-ref bitbrd occupied-idx))
		(gen-piece-attacks-bitbrd bitbrd side queen (u64vector-ref bitbrd occupied-idx))
		(gen-piece-attacks-bitbrd bitbrd side rook (u64vector-ref bitbrd occupied-idx))
		(gen-piece-attacks-bitbrd bitbrd side bishop (u64vector-ref bitbrd occupied-idx))
		(gen-piece-attacks-bitbrd bitbrd side knight (u64vector-ref bitbrd occupied-idx))
		(gen-pawn-attacks-bitbrd bitbrd side)))

;; ---- moves formatting functions ----

(define (find-ambiguous-moves move all-moves)
  (filter
    (lambda (mv)
      (and
        (not (equal? (move-from move) (move-from mv)))
        (equal? (move-to move) (move-to mv))))
      all-moves))

(define (disambiguates move ambiguous)
  (cond
		((null? ambiguous) '())
    ((= pawn (piece-type (move-from move))) '())
    (else
			(let ((eq-file (any? (lambda (x)
                            (fx= (piece-file (move-from move))
  	        		                (piece-file (move-from x))))
                          ambiguous))
          	(eq-rank (any? (lambda (x)
                            (fx= (piece-rank (move-from move))
                		            (piece-rank (move-from x))))
                          ambiguous)))
    		(cond
        	((not eq-file)
          	(list (integer->char
            	(+ (- (char->integer #\a) 1) (piece-file (move-from move))))))
        	((not eq-rank)
          	(list (integer->char
            	(+ (- (char->integer #\1) 1) (piece-rank (move-from move))))))
        	(else (list
          	      (integer->char
            	      (+ (- (char->integer #\a) 1) (piece-file (move-from move))))
              	  (integer->char
                	  (+ (- (char->integer #\1) 1) (piece-rank (move-from move)))))))))))

(define (algebraic-notation move all-moves)
	(with-output-to-string
		(lambda ()
			(cond
				((and (is-capture? move) (= pawn (piece-type (move-from move))))
					(write-char (integer->char (+ (- (char->integer #\a) 1) (piece-file (move-from move))))))
				((and (= king (piece-type (move-from move)))
							(not (is-castle? move)))     					(write-char #\K))
				((= queen (piece-type (move-from move)))    (write-char #\Q))
				((= bishop (piece-type (move-from move)))   (write-char #\B))
				((= knight (piece-type (move-from move)))   (write-char #\N))
				((= rook (piece-type (move-from move)))     (write-char #\R)))
			(for-each write-char (disambiguates move (find-ambiguous-moves move all-moves)))
			(if (is-capture? move) (write-char #\x))
			(cond
				((is-short-castle? move)
					(write-char #\O) (write-char #\-) (write-char #\O))
				((is-long-castle? move)
					(write-char #\O) (write-char #\-) (write-char #\O) (write-char #\-) (write-char #\O))
				(else
					(write-char (integer->char (+ (- (char->integer #\a) 1) (piece-file (move-to move)))))
					(write-char (integer->char (+ (- (char->integer #\1) 1) (piece-rank (move-to move)))))))
			(if (is-promotion? move)
				(begin
					(write-char #\=)
					(cond
						((= queen (piece-type (move-to move)))    (write-char #\Q))
						((= bishop (piece-type (move-to move)))   (write-char #\B))
						((= knight (piece-type (move-to move)))   (write-char #\N))
						((= rook (piece-type (move-to move)))     (write-char #\R))))))))

; return a san string for the given move and position, including check/checkmate symbol
(define (san move cp)
	(cond
		((is-checkmate? (make-board move cp)) (append-strings (list (algebraic-notation move (board-moves cp)) "#")))
		((is-check-move? move cp) (append-strings (list (algebraic-notation move (board-moves cp)) "+")))
		(else (algebraic-notation move (board-moves cp)))))

(define (has-check-or-mate-symbol? s)
	(or
		(char=? #\+ (string-ref s (- (string-length s) 1)))
		(char=? #\# (string-ref s (- (string-length s) 1)))))

; compare two san strings excluding the check/checkmate symbol
(define (san-equal? m1 m2)
	(let* ((len1 (string-length m1)) (len2 (string-length m2))) 
		(string=?
			(substring m1 0 (if (has-check-or-mate-symbol? m1) (- len1 2) (- len1 1)))
			(substring m2 0 (if (has-check-or-mate-symbol? m2) (- len2 2) (- len2 1))))))

; return a string move in uci notation
(define (uci-notation move)
	(with-output-to-string
		(lambda ()
      (write-char (integer->char (fx+ (fx- (char->integer #\a) 1) (piece-file (move-from move)))))
      (write-char (integer->char (fx+ (fx- (char->integer #\1) 1) (piece-rank (move-from move)))))
      (write-char (integer->char (fx+ (fx- (char->integer #\a) 1) (piece-file (move-to move)))))
      (write-char (integer->char (fx+ (fx- (char->integer #\1) 1) (piece-rank (move-to move)))))
			(if (is-promotion? move)
				(cond
					((fx= queen (piece-type (move-to move)))    (write-char #\q))
					((fx= bishop (piece-type (move-to move)))   (write-char #\b))
					((fx= knight (piece-type (move-to move)))   (write-char #\n))
					((fx= rook (piece-type (move-to move)))     (write-char #\r)))))))

;; ---- fen & printing functions ----

(define (coord->square file rank)
	(fx+ (fx* 8 (fx- rank 1)) (fx- file 1)))

(define (print-moves x)
	(if (chessp? x)
		(pretty-print	(map (lambda (m) (san m x)) (board-moves x)))
		(pretty-print	(map (lambda (m) (algebraic-notation m x)) x))))

(define (white-on-top) 
	(list
		 7  6  5  4  3  2  1  0 #\/
		15 14 13 12 11 10  9  8 #\/
		23 22 21 20 19 18 17 16 #\/
		31 30 29 28 27 26 25 24 #\/
		39 38 37 36 35 34 33 32 #\/
		47 46 45 44 43 42 41 40 #\/
		55 54 53 52 51 50 49 48 #\/
		63 62 61 60 59 58 57 56 )) 

(define (fen-piece piece)
	(cond
		((null? piece) 1)
		((= (get-piece piece) (* white king))		#\K)
		((= (get-piece piece) (* white queen))	#\Q)
		((= (get-piece piece) (* white bishop))	#\B)
		((= (get-piece piece) (* white knight))	#\N)
		((= (get-piece piece) (* white rook))		#\R)
		((= (get-piece piece) (* white pawn))		#\P)
		((= (get-piece piece) (* black king))		#\k)
		((= (get-piece piece) (* black queen))	#\q)
		((= (get-piece piece) (* black bishop))	#\b)
		((= (get-piece piece) (* black knight))	#\n)
		((= (get-piece piece) (* black rook))		#\r)
		((= (get-piece piece) (* black pawn))		#\p)))

(define (sort-for-white board)
	(map 
		(lambda (x)	(if (char? x) x (fen-piece (piece-at x board))))
		(white-on-top)))

(define (accumulate-numbers n ret lst)
	(cond
		((null? lst)
			(if (> n 0)
				(cons (integer->char (+ (char->integer #\0) n)) ret)
				ret))
		((number? (car lst)) (accumulate-numbers (+ n 1) ret (cdr lst)))
		((eq? (car lst) #\/)
			(if (> n 0)
				(accumulate-numbers 0
					(cons #\/ (cons (integer->char (+ (char->integer #\0) n)) ret))
					(cdr lst))
				(accumulate-numbers 0	(cons #\/ ret) (cdr lst))))
		(else 
			(if (> n 0)
				(accumulate-numbers 0
					(cons (car lst) (cons (integer->char (+ (char->integer #\0) n)) ret))
					(cdr lst))
				(accumulate-numbers 0	(cons (car lst) ret) (cdr lst))))))

(define (board-to-fen cp)
	(with-output-to-string
		(lambda ()
			(for-each write-char (accumulate-numbers 0 '() (sort-for-white (chessp->board cp))))
			(write-char #\space)
			(write-char (if (= white (chessp-side cp)) #\w #\b))
			(write-char #\space)
			(if (null? (chessp-castle cp))
				(write-char #\-)
				(for-each
					(lambda (m)
						(cond
							((and (is-white? m) (= (piece-file m) 7)) (write-char #\K)) 
							((and (is-white? m) (= (piece-file m) 3)) (write-char #\Q)) 
							((and (is-black? m) (= (piece-file m) 7)) (write-char #\k)) 
							((and (is-black? m) (= (piece-file m) 3)) (write-char #\q)))) 
					(chessp-castle cp)))
			(write-char #\space)
			(if (null? (chessp-ep cp))
				(write-char #\-)
				(begin
					(write-char (integer->char (+ (- (char->integer #\a) 1) (piece-file (chessp-ep cp)))))
					(write-char (integer->char (+ (char->integer #\0) (piece-rank (chessp-ep cp)))))))
			(write-char #\space)
			(write (chessp-hm cp))
			(write-char #\space)
			(write (chessp-fm cp)))))

(define (print-fen cp)
	(if (chessp? cp) (pretty-print (board-to-fen cp))))

(define (parse-side str)
	(cond
		((string=? str "w") white)
		((string=? str "b") black)
		(else (error 'parse-side "Invalid FEN string"))))

(define (parse-castle str)
	(cond
		((string=? str "-") '())
		(else 
			(if (>= 4 (string-length str))
				(map
					(lambda (c)
						(cond 
							((char=? c #\K) (make-piece (* white king) 6))
							((char=? c #\Q) (make-piece (* white king) 2))
							((char=? c #\k) (make-piece (* black king) 62))
							((char=? c #\q) (make-piece (* black king) 58))
							(else (error 'parse-castle "Invalid FEN string"))))
					(string->list str))))))

(define (parse-square str)
	(let ((file (- (char->integer (string-ref str 0)) (- (char->integer #\a) 1)))
				(rank (- (char->integer (string-ref str 1)) (char->integer #\0))))
		(cond
			((and (>= file 1) (<= file 8) (= rank 3))
				(make-piece (* white pawn) (coord->square file rank)))
			((and (>= file 1) (<= file 8) (= rank 6))
				(make-piece (* black pawn) (coord->square file rank)))
			(else (error 'parse-square "Invalid FEN string")))))

(define (parse-ep str)
	(cond
		((string=? str "-") '())
		(else (parse-square str))))

(define (parse-board str)
	(let loop ((in (open-input-string str)) (board '()) (file 1) (rank 8))
		(let ((c (read-char in)))
			(cond
				((eof-object? c) board)
				((char-numeric? c)
					(loop in board (+ file (- (char->integer c) (char->integer #\0))) rank))
				((char=? c #\/) (loop in board 1 (- rank 1)))
				((char=? c #\K)
					(loop in (cons (make-piece (* white king) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\Q)
					(loop in (cons (make-piece (* white queen) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\R)
					(loop in (cons (make-piece (* white rook) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\N)
					(loop in (cons (make-piece (* white knight) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\B)
					(loop in (cons (make-piece (* white bishop) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\P)
					(loop in (cons (make-piece (* white pawn) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\k)
					(loop in (cons (make-piece (* black king) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\q)
					(loop in (cons (make-piece (* black queen) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\r)
					(loop in (cons (make-piece (* black rook) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\n)
					(loop in (cons (make-piece (* black knight) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\b)
					(loop in (cons (make-piece (* black bishop) (coord->square file rank)) board) (+ file 1) rank))
				((char=? c #\p)
					(loop in (cons (make-piece (* black pawn) (coord->square file rank)) board) (+ file 1) rank))
				(else (error 'parse-board "Invalid FEN string"))))))

(define (board-from-fen fen-string)
	(let* ((lst (string-split fen-string #\space))
					(len (length lst)))
		(if (>= len 4)
			(let* ((board (parse-board (list-ref lst 0))) (side (parse-side (list-ref lst 1)))
					 	 (ep (parse-ep (list-ref lst 3))) (castle (parse-castle (list-ref lst 2)))
						 (z-hash (zobrist-hash side ep castle board)))
				(set-board-score!
					(make-chessp
						side castle	ep
						(if (>= len 5) (string->number (list-ref lst 4)) 0)
						(if (>= len 6) (string->number (list-ref lst 5)) 1)
						0
						z-hash
						(list z-hash)
						0
						(init-bitbrd-from-board board)
						#f)))
			(error 'parse-from-fen "Invalid FEN string"))))

;; ---- check/attacks functions (verify legal) ----

; gen-legal-moves generates only valid moves
(define (board-moves cp)
	(gen-legal-moves cp))

; check if the given move gives check to the opposite king
(define (is-check-move? move cp)
	(let* ((piece (piece-type (move-to move))) (from-sq (piece-square (move-from move)))
				 (to-sq (piece-square (move-to move))) (side (chessp-side cp)) (to-bit (square->bit to-sq))
				 (blockers (u64-ior to-bit (u64-xor (square->bit from-sq) (u64vector-ref (chessp-bitbrd cp) occupied-idx))))
				 (k-bit (u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 (opposite-side (chessp-side cp)) king))))
		(or
			; direct attacks
			(cond
				((fx= piece pawn)
					(any-bits-set? k-bit
						(u64-ior
							(gen-pawn-attacks-bitbrd-east to-bit side)
							(gen-pawn-attacks-bitbrd-west to-bit side))))
				((not (fx= piece king))
					(any-bits-set? k-bit (magic-attacks-bitbrd-for-square piece to-sq blockers)))
				((is-castle? move)
					(if (is-short-castle? move)
						(any-bits-set? k-bit (magic-attacks-bitbrd-for-square rook (if (fx= side white) 5 61) blockers))
						(any-bits-set? k-bit (magic-attacks-bitbrd-for-square rook (if (fx= side white) 3 59) blockers))))
				(else	#f))
			; discovered attacks
			(if (u64-and
						(u64vector-ref (chessp-bitbrd cp) (if (fx= side white) white-pieces-idx black-pieces-idx))
						(u64vector-ref (chessp-bitbrd cp) (if (fx= side white) black-blockers-idx white-blockers-idx)))
				(not (aligned? from-sq to-sq (bitscan-fwd k-bit)))			
				#f)

			(if (is-en-passant? move cp)
				(not (zero?	(u64-and
					(u64-shift to-bit (fx* side -8))
					(u64vector-ref (chessp-bitbrd cp) (if (fx= side white) black-pieces-idx white-pieces-idx))
					(u64vector-ref (chessp-bitbrd cp) (if (fx= side white) black-blockers-idx white-blockers-idx)))))
				#f))))

; test if the side to move is under check
(define (under-check? cp)
	(any-bits-set?
		(u64vector-ref (chessp-bitbrd cp) (bitbrd-idx2 (chessp-side cp) king))
		(u64vector-ref (chessp-bitbrd cp)
					(if (fx= white (chessp-side cp)) black-attacks-idx white-attacks-idx))))

(define (is-checkmate? cp)
	(and (not (legal-moves-available? cp)) (under-check? cp)))

(define (is-stalemate? cp)
	(and (not (legal-moves-available? cp)) (not (under-check? cp))))

;; ---- hash & transposition table functions ----
;; size of hash keys = (64 * 12) = 768
;; 2^64 - 1 = 18446744073709551615

(define (v-piece-index piece)
	(fx+
		(piece-square piece)
		(fx* 64 (fx- (piece-type piece) 1))
		(if (fx= (piece-color piece) white) 0 384)))

(define (v-ep-index ep)
	(fx+
		(fx- (piece-file ep) 1)
		(if (fx= (piece-color ep) white) 0 384))) ; rank 1 not used for pawns

(define (v-castle-index castle)
	(fx+
		(cond
			((fx= (piece-square castle) 6) 56) ; rank 8 not used for pawns
			((fx= (piece-square castle) 2) 57)
			((fx= (piece-square castle) 62) 58)
			((fx= (piece-square castle) 58) 59))))

(define (v-side-index side)
	(if (fx= side white) 60 61))
(define v-both-side-index 62) ; stores xor between white & black

; only used in testing
(define (zobrist-hash-from-cp vect cp)
	(u64-xor (u64vector-ref vect (v-side-index (chessp-side cp))) 
		(u64-xor (if (null? (chessp-ep cp)) 0 (u64vector-ref vect (v-ep-index (chessp-ep cp)))) 
			(fold
				(lambda (c hash) (u64-xor hash (u64vector-ref vect (v-castle-index c))))
				(fold
					(lambda (p hash) (u64-xor hash (u64vector-ref vect (v-piece-index p))))
					0 (chessp->board cp))
				(chessp-castle cp)))))

; used in position/board initialization
(define (zobrist-hash side ep castle board)
	(u64-xor (u64vector-ref zhash (v-side-index side)) 
		(u64-xor (if (null? ep) 0 (u64vector-ref zhash (v-ep-index ep))) 
			(fold
				(lambda (c hash) (u64-xor hash (u64vector-ref zhash (v-castle-index c))))
				(fold
					(lambda (p hash) (u64-xor hash (u64vector-ref zhash (v-piece-index p))))
					0 board)
				castle))))

(define transposition-table (make-table))

;; (tt-ref cp) -> val | #f 
(define (tt-ref cp)
		(table-ref transposition-table (chessp-zhash cp) #f))
(define (tt-set! cp val)
	(if (can-search?) ; avoid polluting tt while aborting search
		(table-set! transposition-table (chessp-zhash cp) val)))
(define (tt-delete! cp)
	(table-delete! transposition-table (chessp-zhash cp)))
(define (tt-size)
	(table-length transposition-table))
(define (tt-dump)
	(table-for-each (lambda (k v) (pretty-print (list k v))) transposition-table))
(define (tt-reset)
	(set! transposition-table (make-table)))

;; ---- update position functions ----

(define (update-bitbrd-zhash-score! move prev-cp new-cp)
	(let* ((bitbrd (u64vector-copy (chessp-bitbrd prev-cp))) (side (piece-color (move-from move)))
				 (opposite (opposite-side side)) (ep (chessp-ep prev-cp)) (new-score (chessp-score prev-cp))
				 (z-hash 0))
		(cond
			((is-en-passant? move prev-cp)
				(if (fx= side white)
					(let* ((square (fx- (piece-square (move-to move)) 8))
								(bit (square->bit square)))
						(u64vector-set! bitbrd (bitbrd-idx2 black pawn)
							(u64-xor bit (u64vector-ref bitbrd (bitbrd-idx2 black pawn))))
						(u64vector-set! bitbrd black-pieces-idx
							(u64-xor bit (u64vector-ref bitbrd black-pieces-idx)))
						(u64vector-set! bitbrd occupied-idx
							(u64-xor bit (u64vector-ref bitbrd occupied-idx)))
						(set! new-score (fx+ new-score (vector-ref material-score	(pt-idx (fx* black pawn) square))))
						(set! z-hash (u64-xor z-hash (u64vector-ref zhash (v-piece-index (bitbrd-piece-at square (chessp-bitbrd prev-cp)))))))
					(let* ((square (fx+ (piece-square (move-to move)) 8))
							  (bit (square->bit square)))
						(u64vector-set! bitbrd (bitbrd-idx2 white pawn)
							(u64-xor bit (u64vector-ref bitbrd (bitbrd-idx2 white pawn))))
						(u64vector-set! bitbrd white-pieces-idx
							(u64-xor bit (u64vector-ref bitbrd white-pieces-idx)))
						(u64vector-set! bitbrd occupied-idx
							(u64-xor bit (u64vector-ref bitbrd occupied-idx)))
						(set! new-score (fx+ new-score (fx* -1 (vector-ref material-score	(pt-idx (fx* white pawn) square)))))
						(set! z-hash (u64-xor z-hash (u64vector-ref zhash (v-piece-index (bitbrd-piece-at square (chessp-bitbrd prev-cp)))))))))
			((is-capture? move)
				(let* ((piece (bitbrd-piece-at (piece-square (move-to move)) bitbrd))
							 (bit (piece->bit piece))
							 (side-idx (if (fx= side white) black-pieces-idx white-pieces-idx)))
					(u64vector-set! bitbrd (bitbrd-idx piece)
						(u64-xor bit (u64vector-ref bitbrd (bitbrd-idx piece))))
					(u64vector-set! bitbrd side-idx
						(u64-xor bit (u64vector-ref bitbrd side-idx)))
					(set! new-score (fx+ new-score (fx* side (vector-ref material-score
						(pt-idx (get-piece piece) (piece-square piece))))))
					(set! z-hash (u64-xor z-hash (u64vector-ref zhash (v-piece-index piece))))))
			((is-short-castle? move)
				(if (fx= side white)
					(let ((rook-swap (u64-xor (square->bit 7) (square->bit 5))))
						(u64vector-set! bitbrd (bitbrd-idx2 white rook)
							(u64-xor rook-swap
								(u64vector-ref bitbrd (bitbrd-idx2 white rook))))
						(u64vector-set! bitbrd white-pieces-idx
							(u64-xor rook-swap (u64vector-ref bitbrd white-pieces-idx)))
						(u64vector-set! bitbrd occupied-idx
							(u64-xor rook-swap (u64vector-ref bitbrd occupied-idx)))
						(set! new-score (fx+ new-score
															(fx* -1 
																(vector-ref material-score
																	(pt-idx (fx* white rook) 7)))
															(fx* +1
																(vector-ref material-score
																	(pt-idx (fx* white rook) 5)))))
						(set! z-hash
							(u64-xor z-hash
								(u64vector-ref zhash (v-piece-index (make-piece (fx* white rook) 7)))
								(u64vector-ref zhash (v-piece-index (make-piece (fx* white rook) 5))))))
					(let ((rook-swap (u64-xor (square->bit 63) (square->bit 61))))
						(u64vector-set! bitbrd (bitbrd-idx2 black rook)
							(u64-xor rook-swap
								(u64vector-ref bitbrd (bitbrd-idx2 black rook))))
						(u64vector-set! bitbrd black-pieces-idx
							(u64-xor rook-swap (u64vector-ref bitbrd black-pieces-idx)))
						(u64vector-set! bitbrd occupied-idx
							(u64-xor rook-swap (u64vector-ref bitbrd occupied-idx)))
						(set! new-score (fx+ new-score
															(fx* +1 
																(vector-ref material-score
																	(pt-idx (fx* black rook) 63)))
															(fx* -1
																(vector-ref material-score
																	(pt-idx (fx* black rook) 61)))))
						(set! z-hash
							(u64-xor z-hash
								(u64vector-ref zhash (v-piece-index (make-piece (fx* black rook) 63)))
								(u64vector-ref zhash (v-piece-index (make-piece (fx* black rook) 61))))))))
			((is-long-castle? move)
				(if (fx= side white)
					(let ((rook-swap (u64-xor (square->bit 0) (square->bit 3))))
						(u64vector-set! bitbrd (bitbrd-idx2 white rook)
							(u64-xor rook-swap
								(u64vector-ref bitbrd (bitbrd-idx2 white rook))))
						(u64vector-set! bitbrd white-pieces-idx
							(u64-xor rook-swap (u64vector-ref bitbrd white-pieces-idx)))
						(u64vector-set! bitbrd occupied-idx
							(u64-xor rook-swap (u64vector-ref bitbrd occupied-idx)))
						(set! new-score (fx+ new-score
															(fx* -1 
																(vector-ref material-score
																	(pt-idx (fx* white rook) 0)))
															(fx* +1
																(vector-ref material-score
																	(pt-idx (fx* white rook) 3)))))
						(set! z-hash
							(u64-xor z-hash
								(u64vector-ref zhash (v-piece-index (make-piece (fx* white rook) 0)))
								(u64vector-ref zhash (v-piece-index (make-piece (fx* white rook) 3))))))
					(let ((rook-swap (u64-xor (square->bit 56) (square->bit 59))))
						(u64vector-set! bitbrd (bitbrd-idx2 black rook)
							(u64-xor rook-swap
								(u64vector-ref bitbrd (bitbrd-idx2 black rook))))
						(u64vector-set! bitbrd black-pieces-idx
							(u64-xor rook-swap (u64vector-ref bitbrd black-pieces-idx)))
						(u64vector-set! bitbrd occupied-idx
							(u64-xor rook-swap (u64vector-ref bitbrd occupied-idx)))
						(set! new-score (fx+ new-score
															(fx* +1 
																(vector-ref material-score
																	(pt-idx (fx* black rook) 56)))
															(fx* -1
																(vector-ref material-score
																	(pt-idx (fx* black rook) 59)))))
						(set! z-hash
							(u64-xor z-hash
								(u64vector-ref zhash (v-piece-index (make-piece (fx* black rook) 56)))
								(u64vector-ref zhash (v-piece-index (make-piece (fx* black rook) 59)))))))))
		(chessp-score-set! new-cp
			(fx+ new-score
				(fx* opposite
					(vector-ref material-score
						(pt-idx2 (get-piece (move-from move)) (piece-square (move-from move)) prev-cp)))
				(fx* side
					(vector-ref material-score
						(pt-idx2 (get-piece (move-to move)) (piece-square (move-to move)) prev-cp)))))
		(let* ((from-bit (piece->bit (move-from move))) (to-bit (piece->bit (move-to move)))
						(move-bits (u64-ior from-bit to-bit))
						(side-idx (if (fx= side white) white-pieces-idx black-pieces-idx)))
			(u64vector-set! bitbrd (bitbrd-idx (move-from move)) 
				(u64-xor from-bit
					(u64vector-ref bitbrd (bitbrd-idx (move-from move)))))
			(u64vector-set! bitbrd (bitbrd-idx (move-to move))
				(u64-ior to-bit
					(u64vector-ref bitbrd (bitbrd-idx (move-to move)))))
			(u64vector-set! bitbrd side-idx
				(u64-xor move-bits (u64vector-ref bitbrd side-idx)))
			(u64vector-set! bitbrd occupied-idx
				(u64-xor from-bit
					(u64vector-ref bitbrd occupied-idx)))
			(if (is-en-passant? move prev-cp)
				(u64vector-set! bitbrd occupied-idx
					(u64-xor to-bit (u64vector-ref bitbrd occupied-idx)))
				(if (not (is-capture? move))
					(u64vector-set! bitbrd occupied-idx
						(u64-xor to-bit (u64vector-ref bitbrd occupied-idx)))))
			(u64vector-set! bitbrd white-attacks-idx (gen-all-attacks-bitbrd-for-side white bitbrd))
			(u64vector-set! bitbrd black-attacks-idx (gen-all-attacks-bitbrd-for-side black bitbrd)))
			(u64vector-set! bitbrd white-blockers-idx (sliders-blockers bitbrd white (u64vector-ref bitbrd occupied-idx)))
			(u64vector-set! bitbrd black-blockers-idx (sliders-blockers bitbrd black (u64vector-ref bitbrd occupied-idx)))
		; (validate-bitbrd bitbrd)
		(chessp-bitbrd-set! new-cp bitbrd)
		(chessp-zhash-set! new-cp
			(u64-xor z-hash
				(u64vector-ref zhash (v-piece-index (move-to move))) ; add moving piece
				(u64vector-ref zhash (v-piece-index (move-from move))) ; remove moving piece
				(fold ; remove missing castle rights
					(lambda (c hash) (u64-xor hash (u64vector-ref zhash (v-castle-index c))))
					0 (filter	(lambda (c) (equal? #f (member c (chessp-castle new-cp)))) (chessp-castle prev-cp)))
				(if (null? (chessp-ep new-cp)) 0 (u64vector-ref zhash (v-ep-index (chessp-ep new-cp)))) ; add new ep
		 		(if (null? (chessp-ep prev-cp)) 0 (u64vector-ref zhash (v-ep-index (chessp-ep prev-cp)))) ; remove old ep 
				(u64vector-ref zhash v-both-side-index) ; update side to move
				(chessp-zhash prev-cp)))
		(chessp-repetitions-set! new-cp (update-repetitions move (chessp-zhash new-cp) prev-cp))
		new-cp))

(define (remove-opposite-rights move rights)
	(if (is-capture? move)
		(let ((square (piece-square (move-to move))))
			(cond
				((fx= 7 square)
					(filter (lambda (p) (not (fx= 6 (piece-square p)))) rights))
				((fx= 0 square)
					(filter (lambda (p) (not (fx= 2 (piece-square p)))) rights))
				((fx= 63 square)
					(filter (lambda (p) (not (fx= 62 (piece-square p)))) rights))
				((fx= 56 square)
					(filter (lambda (p) (not (fx= 58 (piece-square p)))) rights))
				(else rights)))
		rights))

(define (update-castle-rights move rights)
	(if (null? rights)
		rights
		(let ((rights (remove-opposite-rights move rights)))
			(if (side-already-castled? move rights)
				rights
				(cond
					((fx= rook (piece-type (move-from move)))
						(let ((square (piece-square (move-from move))))
							(if (is-white? (move-from move))
								(cond
									((fx= 7 square)
										(filter (lambda (p) (not (fx= 6 (piece-square p)))) rights))
									((fx= 0 square)
										(filter (lambda (p) (not (fx= 2 (piece-square p)))) rights))
									(else rights))
								(cond
									((fx= 63 square)
										(filter (lambda (p) (not (fx= 62 (piece-square p)))) rights))
									((fx= 56 square)
										(filter (lambda (p) (not (fx= 58 (piece-square p)))) rights))
									(else rights)))))
					((fx= king (piece-type (move-from move)))
						(if (is-white? (move-from move))
							(filter is-black? rights)
							(filter is-white? rights)))
					((is-castle? move)
						(if (is-white? (move-from move))
							(filter is-black? rights)
							(filter is-white? rights)))
					(else rights))))))

(define (update-en-passant move)
	(if (and (fx= pawn (piece-type (move-from move)))
			(fx> (abs (fx- (piece-rank (move-to move)) (piece-rank (move-from move)))) 1))
		(if (is-white? (move-from move))
			(make-piece (fx* white pawn) (fx+ (piece-square (move-from move)) 8))
			(make-piece (fx* black pawn) (fx- (piece-square (move-from move)) 8)))
		'()))	

(define (update-half-move-clock move hm)
	(if (or (is-capture? move) (fx= pawn (piece-type (move-from move))))
		0
		(fx+ 1 hm)))

(define (update-full-move move fm)
	(if (is-black? (move-from move))
		(fx+ 1 fm)
		fm))

(define (update-repetitions move z-hash cp)
	(cond
		((fx= pawn (piece-type (move-from move)))
			(list z-hash))
		((is-castle? move)
			(list z-hash))
		((is-capture? move)
			(list z-hash))
		(else (append (list z-hash) (chessp-repetitions cp)))))

(define (make-board the-move cp)
	(let* ((ep (update-en-passant the-move))
				 (castle (update-castle-rights the-move (chessp-castle cp))))
		(update-bitbrd-zhash-score! the-move cp 
			(make-chessp
				(opposite-side (chessp-side cp))
				castle
				ep
				(update-half-move-clock the-move (chessp-hm cp))
				(update-full-move the-move (chessp-fm cp))
				(fx+ 1 (chessp-ply cp))
				0	0	0	0 #f))
	))

(define (make-null-move-board cp)
	(let ((z-hash
					(u64-xor
						(if (null? (chessp-ep cp)) 0 (u64vector-ref zhash (v-ep-index (chessp-ep cp)))) ; remove old ep 
						(u64vector-ref zhash v-both-side-index) ; update side to move
						(chessp-zhash cp))))
			(make-chessp
				(opposite-side (chessp-side cp))
				(chessp-castle cp)
				'()
				(fx+ 1 (chessp-hm cp))
				(fx+ (if (fx= black (chessp-side cp)) 1 0) (chessp-fm cp))
				(fx+ 1 (chessp-ply cp))
				z-hash
				(append (list z-hash) (chessp-repetitions cp))
				(chessp-score cp)
				(chessp-bitbrd cp)
				#t)
	))

(define (reset-ply cp)
	(make-chessp (chessp-side cp)	(chessp-castle cp) (chessp-ep cp) (chessp-hm cp)
	(chessp-fm cp) 0 (chessp-zhash cp) (chessp-repetitions cp) (chessp-score cp) (chessp-bitbrd cp) (chessp-null-move? cp) ))

(define (enter-move algstr cp)
	(let* ((mv (board-moves cp))
					(alg-moves (map (lambda (m) (cons (algebraic-notation m mv) m)) mv))
					(the-move (assoc algstr alg-moves)))
		(if (equal? #f the-move)
			(error 'enter-move "invalid move entered" algstr)
			(make-board (cdr the-move) cp))))

;; ---- search functions ----

(define (can-search?) (thread-specific (current-thread)))

(define (make-counter n) (lambda () (set! n (fx+ 1 n)) n))
(define node-counter (make-counter 0))
(define (visited-nodes)	(fx- (node-counter) 1))
(define (reset-visited-nodes) (set! node-counter (make-counter 0)) node-counter)

(define (draw-by-repetition? cp)
	(fx<= 3
		(fold
			(lambda (z-hash n) (if (equal? z-hash (chessp-zhash cp)) (fx+ 1 n) n))
			0
			(chessp-repetitions cp))))

(define (unordered-moves moves cp)
	(list->vector (map (lambda (m) (cons -1 m)) moves)))

(define (order-moves moves cp)
	(list->vector
		(map
			(lambda (m)
				(cond
					((is-capture? m)
						(cons (fx+ 1000 (see m cp)) m)) ; good capture before bad captures
					((is-promotion? m) (cons 50 m)) ; promotions
					; ((is-check-move? m cp) (cons 30 m)) ; move that gives check. this is the most expensive test
																								; in move ordering (-50% in search speed) and I am not
																								; sure this adds anything useful to the search.
																								; qualifying moves as checks is useful in lmr though
					(else (cons -1 m)))) ; all other moves follows
			moves)))

(define (gain-moves-only moves cp)
	(list->vector
		(filter
			(lambda (m) (fx> (car m) 0))	; qsearch uses only good captures and promotions
			(map													; dropping other moves helps in sorting speed
				(lambda (m)
					(cond
						((is-capture? m) (cons (see m cp) m))
						((is-promotion? m) (cons 10 m))
						(else (cons -1 m))))
				moves))))

(define (pv-line-tt type cp)
	(if (equal? type 'qsearch)
		'()
		(let ((pv (tt-ref cp)))
			(if (and (not (equal? #f pv)) (not (null? (cadddr pv))))
				(cons (fx+ (car pv) (fx* 10000 (caddr pv))) (car (cadddr pv)))
				'()))))

(define (moves-sorter-tt type cp)
	(let ((pv (pv-line-tt type cp))
				(move-gen (delayed-moves-sorter type cp))
				(serve-pv #t))
		(lambda ()
			(if serve-pv
				(begin
					(set! serve-pv #f)
					(if (null? pv)
						(move-gen)
						pv))
				(let ((move (move-gen)))
					(if (and (not (null? pv)) (equal? (cdr pv) (cdr move)))
						(move-gen)
						move))))))

(define (delayed-moves-sorter type cp)
	(letrec ((idx 0) (keep-sorting? #t) (move-gen (gen-legal-moves-delayed type cp))
					 (order-func
							(cond
								((equal? type 'qsearch) gain-moves-only)
								((equal? type 'unordered) unordered-moves)
								(else order-moves)))
					 (vect-move '())
				 		(next-batch (lambda ()
							(if (null? move-gen)
								(set! vect-move '())
								(let ((v (order-func (force (car move-gen)) cp)))
									(if (zero? (vector-length v))
										(begin
											(set! move-gen (cdr move-gen))
											(next-batch))
										(begin
											(set! move-gen (cdr move-gen))
											(set! vect-move v))))))))
		(next-batch)
		(lambda ()
			(cond
				((null? vect-move)
					(cons -1 '())) ; null move as end of stream
				((fx>= idx (vector-length vect-move))
					(next-batch)
					(if (null? vect-move)
						(cons -1 '()) ; null move as end of stream
						(let ((move (selection-sort vect-move 0 0 1)))
							(set! keep-sorting? (fx>= (car move) 0)) ; stop sorting when no more tactical moves are present
							(set! idx 1)
							move)))
				(keep-sorting?
					(let ((move (selection-sort vect-move idx idx (fx+ 1 idx))))
						(set! keep-sorting? (fx>= (car move) 0)) ; stop sorting when no more tactical moves are present
						(set! idx (fx+ 1 idx))
						move))
				(else
					(let ((move (vector-ref vect-move idx)))
						(set! idx (fx+ 1 idx))
						move))))))

(define (selection-sort vect-moves start best current)
	(cond
		((fx>= current (vector-length vect-moves))
			(if (fx> best start)
				(let ((tmp (vector-ref vect-moves start)))
					(vector-set! vect-moves start (vector-ref vect-moves best))
					(vector-set! vect-moves best tmp)))
			(vector-ref vect-moves start))
		((fx> (car (vector-ref vect-moves current)) (car (vector-ref vect-moves best)))
			(selection-sort vect-moves start current (fx+ 1 current)))
		(else (selection-sort vect-moves start best (fx+ 1 current)))))

; late move reductions - prune depth for non tactical moves
(define (lmr depth move-nbr move cp)
	(fx- depth
		(cond
			((fx< move-nbr 4) 1)  		; always full search first 3 moves
			((fx<= depth 3) 1)				; suppress lmr for low depth
			((under-check? cp) 1)			; under check suppress lmr
			((fx< (car move) 0)
				(if (fx< depth 7) 2			; prune 1 ply for non tactical moves if 4 <= depth <= 6
					3))										; prune 2 ply for non tactical moves if depth >= 7
			(else 1))))								; do not prune promotions, pv moves, captures ... what about checks? they are expensive!

(define (null-move-allowed? depth cp)
	(and (fx> depth 3)
		(not (or (chessp-null-move? cp) (under-check? cp) (kp-only? cp)))))

; http://members.home.nl/matador/Inside%20Rebel.pdf
; https://www.chessprogramming.org/Quiescence_Search
(define (quiesce a b cp)
	(node-counter)
	(let ((stand-pat (score cp)))
		(cond
			((not (legal-moves-available? cp))
				(if (under-check? cp)
					(cons (fx- (fx- checkmate-score (chessp-ply cp))) '())
					(cons 0 '())))
      ((or (fx>= (chessp-hm cp) 100) (draw-by-repetition? cp))
				(cons 0 '()))
			((fx> stand-pat b) (cons stand-pat '()))
			(else (quiesce-moves stand-pat (max a stand-pat) b cp (moves-sorter-tt 'qsearch cp) '())))))

(define (quiesce-moves stand-pat alpha b cp next-move best-pv)
	(let ((curr-move (next-move)))
		(cond
			((or (null? (cdr curr-move)) (fx< (car curr-move) 0))
				(cons alpha best-pv))
			((and (is-capture? (cdr curr-move))  ; delta pruning using see values
;						(not (is-endgame? cp))
						(fx>= alpha	(fx+ (car curr-move) stand-pat)))
				(quiesce-moves stand-pat alpha b cp next-move best-pv))
			(else	(let* ((result (quiesce (fx- b) (fx- alpha) (make-board (cdr curr-move) cp)))
									(new-score (fx- (car result))))
				(cond
					((fx>= new-score b)
						(cons new-score (cons (cdr curr-move) (cdr result))))
					((fx> new-score alpha)
						(quiesce-moves stand-pat new-score b cp next-move (cons (cdr curr-move) (cdr result))))
					(else	(quiesce-moves stand-pat alpha b cp next-move best-pv))))))))

;; from https://www.gamedev.net/forums/topic.asp?topic_id=503234
; An alpha-beta search has 3 possible outcomes:
; 1) The true score is alpha or below, and the score returned is an upper bound of the true score.
; In case 1, we can improve beta if the score is below beta.
; 2) The true score is between alpha and beta, and it's returned exactly.
; Case 2 is the easy one, where we can simply return the score.
; 3) The true score is beta or above, and the score returned is a lower bound of the true score.
; In case 3, we can improve alpha if the score is above alpha.
; If after these checks you find out that alpha >= beta, then you can return almost anything (in my chess program I return alpha).
;
; There is at least one more piece of information that you should store in your hash table entries:
; Which move was found to be the best. This is either the move that returned the best score,
; or the one that produced a beta cut.
; This information can be used when you find this position again, even if the new search is deeper
; than what you stored: You simply try that move first when you loop through all available moves.
; This will significantly improve your move ordering and therefore the efficiency of alpha-beta.
(define (search-with-tt depth a b cp)
	(if (can-search?)
		(begin
			(node-counter)
			(let ((lookup (tt-ref cp)))
				(if (equal? lookup #f)
					(pvs-move-search-tt depth a b cp)
					(if (or (fx<= depth 0) (fx>= (caddr lookup) depth))
						(cond
							((equal? (cadr lookup) 'exact) (cons (car lookup) (cadddr lookup)))
							((and (equal? (cadr lookup) 'lower)	(fx> (car lookup) a) (fx>= (car lookup) b))
								(cons (car lookup) (cadddr lookup)))
							((and (equal? (cadr lookup) 'upper)	(fx< (car lookup) b) (fx>= a (car lookup)))
								(cons (car lookup) (cadddr lookup)))
							((and (equal? (cadr lookup) 'lower) (fx> (car lookup) a))
								(pvs-move-search-tt depth (car lookup) b cp))
							((and (equal? (cadr lookup) 'upper)	(fx< (car lookup) b))
								(pvs-move-search-tt depth a (car lookup) cp))
							(else (pvs-move-search-tt depth a b cp)))
						(pvs-move-search-tt depth a b cp)))))
		(cons 0 '()))) ; abort search, score will not be recorded in tt

(define (pvs-move-search-tt depth a b cp)
	(if (fx<= depth 0) ; depth can be less than zero because of null move pruning
		(quiesce a b cp)
			(cond
				((not (legal-moves-available? cp))
					(if (under-check? cp)
						(begin (tt-set! cp (list (fx- (fx- checkmate-score (chessp-ply cp))) 'exact depth '()))
							(cons (fx- (fx- checkmate-score (chessp-ply cp))) '()))
						(begin (tt-set! cp (list 0 'exact depth '()))
							(cons 0 '()))))
				((or (fx>= (chessp-hm cp) 100) (draw-by-repetition? cp))
					(tt-set! cp (list 0 'exact depth '()))
					(cons 0 '()))
				((null-move-allowed? depth cp)
					(null-move-search depth a b cp (moves-sorter-tt 'all cp) 1))
				(else	(pvs-tt depth a b cp '() #t (fx- checkmate-score) (moves-sorter-tt 'all cp) 1)))))

(define pvsTT search-with-tt)

;; https://www.chessprogramming.org/Principal_Variation_Search
(define (pvs-tt depth alpha beta cp best-pv no-pv best-score next-move move-nbr)
	(let ((curr-move (next-move)))
		(if (and (fx<= (chessp-ply cp) 0) (not (null? (cdr curr-move))))
			(uci-info (list (cons 'depth depth) (cons 'currmove (cdr curr-move)) (cons 'currmovenumber move-nbr))))
		(cond
			((null? (cdr curr-move))
				(cond
					((fx<= best-score alpha)	(tt-set! cp (list best-score 'upper depth best-pv)))	
					(else 										(tt-set! cp (list best-score 'exact depth best-pv))))
				(cons best-score best-pv))
			(no-pv
				(let* ((result (pvsTT (lmr depth move-nbr curr-move cp) (fx- beta) (fx- alpha) (make-board (cdr curr-move) cp)))
							(new-val (fx- (car result))))
					(cond
						((and (fx>= new-val beta) (fx> new-val best-score))
							(tt-set! cp (list new-val 'lower depth (cons (cdr curr-move) (cdr result))))
							(cons new-val (cons (cdr curr-move) (cdr result))))
						((and (fx> new-val alpha) (fx> new-val best-score))
							(pvs-tt depth new-val beta cp (cons (cdr curr-move) (cdr result)) #f new-val next-move (fx+ 1 move-nbr)))
						((fx> new-val best-score)
							(pvs-tt depth alpha beta cp (cons (cdr curr-move) (cdr result)) no-pv new-val next-move (fx+ 1 move-nbr)))
						(else	(pvs-tt depth alpha beta cp best-pv no-pv best-score next-move (fx+ 1 move-nbr))))))
			(else
				(let* ((current-cp (make-board (cdr curr-move) cp))
							(result (pvsTT (fx- depth 1) (fx- (fx- alpha) 1) (fx- alpha) current-cp))
							(new-val (fx- (car result))) (new-alpha (max new-val alpha)))
					(cond
						((and (fx> new-val alpha) (fx< new-val beta))
							(let* ((result (pvsTT (lmr depth move-nbr curr-move cp) (fx- beta) (fx- alpha) current-cp))
										(new-val (fx- (car result))) (new-alpha (max new-val alpha)))
								(cond
									((and (fx>= new-val beta) (fx> new-val best-score))
										(tt-set! cp (list new-val 'lower depth (cons (cdr curr-move) (cdr result))))
										(cons new-val (cons (cdr curr-move) (cdr result))))
									((fx> new-val best-score)
										(pvs-tt depth new-alpha beta cp (cons (cdr curr-move) (cdr result)) #f new-val next-move (fx+ 1 move-nbr)))
									(else (pvs-tt depth alpha beta cp best-pv #f best-score next-move (fx+ 1 move-nbr))))))
						((and (fx>= new-val beta) (fx> new-val best-score))
							(tt-set! cp (list new-val 'lower depth (cons (cdr curr-move) (cdr result))))
							(cons new-val (cons (cdr curr-move) (cdr result))))
						((fx> new-val best-score)
							(pvs-tt depth new-alpha beta cp (cons (cdr curr-move) (cdr result)) #f new-val next-move (fx+ 1 move-nbr)))
						(else (pvs-tt depth alpha beta cp best-pv #f best-score next-move (fx+ 1 move-nbr)))))))))

; https://mediocrechess.blogspot.com/2007/01/guide-null-moves.html
(define (null-move-search depth a b cp next-move move-nbr)
	(define R 2)
	(let* ((result (pvsTT (fx- depth 1 R) (fx- b) (fx+ 1 (fx- b)) (make-null-move-board cp)))
				(new-val (fx- (car result))))
		(if (>= new-val b)
			(cons new-val '())
			(pvs-tt depth a b cp '() #t (fx- checkmate-score) next-move move-nbr))))

; https://chess.stackexchange.com/questions/8658/how-to-evaluate-the-position-inside-the-search-function
; https://www.ics.uci.edu/~eppstein/180a/990202b.html

; https://web.archive.org/web/20071026090003/http://www.brucemo.com/compchess/programming/index.htm
; https://groups.google.com/forum/#!msg/rec.games.chess.computer/p8GbiiLjp0o/81vZ3czsthIJ
(define (deep depth first-guess w cp debug)
	(let loop ((d 1) (guess first-guess) (window w) (best-pv '()) (max-time 0))
		(cond
			((and (<= d depth) (can-search?) (time-left? (* max-time 3.5)))
				(if debug
					(uci-info (list (cons 'string (list "depth=" d "guess=" guess "window=" window "tt size=" (tt-size))))))
				(reset-visited-nodes) 
				(let* ((t1 (msec-time))
							 (result (pvsTT d (- guess window) (+ guess window) cp))
							 (t2 (- (msec-time) t1))
							 (n (visited-nodes)))
					(cond
						((>= (car result) (+ guess window))
							(uci-info	(list
													(cons 'lowerbound 0) (cons 'depth d) (cons 'ms t2) (cons 'nodes n)
													(cons 'score (car result)) (cons 'pv (cdr result))))
							(loop d (car result) (* window 2) (cdr result) (max max-time t2)))
						((<= (car result) (- guess window))
							(uci-info (list
													(cons 'upperbound 0) (cons 'depth d) (cons 'ms t2) (cons 'nodes n)
													(cons 'score (car result)) (cons 'pv (cdr result))))
							(loop d (car result) (* window 2) (cdr result) (max max-time t2)))
						(else
							(uci-info	(list
													(cons 'depth d)	(cons 'ms t2)	(cons 'nodes n)	(cons 'score (car result))
													(cons 'pv (cdr result))))
							(loop (+ 1 d) (car result)
								(if (equal? window checkmate-score) w window) (cdr result) (max max-time t2))))))
			(else (cons guess best-pv)))))

;; ---- time management functions ----

(define (msec-time) (floor (* 1000 (real-time))))

; https://www.chessprogramming.org/Time_Management

	;  nMoves =  min( numberOfMovesOutOfBook, 10 );
  ;  factor = 2 -  nMoves / 10 
  ;  target = timeLeft / numberOfMovesUntilNextTimeControl
  ;  time   = factor * target

; this variable is set to the full move number of the first uci position command received after ucinewgame
(define moves-in-book 0)

; store system ms the search should stop, null avoid to check time during search
(define ms-time-limit '())
(define ponderhit-time-limit '()) ; used in ponderhit to recover original time control in the go command

(define (time-not-expired?)
	(or (null? ms-time-limit) (< (msec-time) ms-time-limit)))

(define (time-left? msecs)
	(or (null? ms-time-limit) (< (+ (* msecs 1) (msec-time)) ms-time-limit)))

(define (clock cp ms-togo moves-togo)
	(let* ((move-nbr (min 10 (fx- (chessp-fm cp) moves-in-book)))
				 (factor (- 2 (/ move-nbr 10.)))
				 (target (/ ms-togo moves-togo)))
		(floor (* factor target))))

; moves are the moves to go to the next time control, if zero it is sudden death
; if moves is not specified (= 0) assume 40 full-move game duration
; if already over 40 moves assume the game will last 5 moves more
; inc is the fischer increment
(define (moves-togo cp moves inc)
	(cond
		((and (zero? moves) (zero? inc)) (if (fx<= (fx- 40 (chessp-fm cp)) 0) 5 (fx- 40 (chessp-fm cp))))
		((and (zero? moves) (not (zero? inc))) 2) ; assume 2 moves to be played in the current time
		(else moves)))

(define (uci-time cp params)
	(let ((time-limit
					(cond
						((not (equal? #f (assoc 'infinite params)))
							'())
						((and (fx= white (chessp-side cp)) (not (equal? #f (assoc 'wtime params))))
							(let ((ms (cdr (assoc 'wtime params)))
										(inc (if (not (equal? #f (assoc 'winc params))) (cdr (assoc 'winc params)) 0))
										(moves (if (not (equal? #f (assoc 'movestogo params))) (cdr (assoc 'movestogo params)) 0)))
								(clock cp ms (moves-togo cp moves inc))))
						((and (fx= black (chessp-side cp)) (not (equal? #f (assoc 'btime params))))
							(let ((ms (cdr (assoc 'btime params)))
										(inc (if (not (equal? #f (assoc 'binc params))) (cdr (assoc 'binc params)) 0))
										(moves (if (not (equal? #f (assoc 'movestogo params))) (cdr (assoc 'movestogo params)) 0)))
								(clock cp ms (moves-togo cp moves inc))))
						((not (equal? #f (assoc 'movetime params)))
							(cdr (assoc 'movetime params)))
						(else '()))))
		(if (not (equal? #f (assoc 'ponder params)))
			(begin
				(set! ponderhit-time-limit time-limit) ; save pre-calculated time limit
				'()) ; return infinite time to start pondering
			time-limit)))

(define (set-clock! cp params)
	(let ((time-limit (uci-time cp params)))
		(set! ms-time-limit (if (null? time-limit) '() (+ (msec-time) time-limit)))))

;; ---- uci protocol functions ----

(define (cmd-uci)
	(display "id name CoronaChess") (newline)
	(display "id author pmon") (newline)
	(display "option name MaxDepth type spin default 10 min 1 max 20") (newline)
	(display "option name Ponder type check default false") (newline)
	(display "uciok") (newline) (force-output))

(define (cmd-setoption params max-depth ponder)
	(list
		(if (equal? #f (assoc 'maxdepth params))
			max-depth
			(string->number (cdr (assoc 'maxdepth params))))
		(if (equal? #f (assoc 'ponder params))
			ponder
			(string=? (cdr (assoc 'ponder params)) "true"))))

(define (cmd-isready) 
	(display "readyok") (newline) (force-output))

(define (enter-uci-move ucistr cp)
	(let* ((mv (board-moves cp))
					(uci-moves (map (lambda (m) (cons (uci-notation m) m)) mv))
					(the-move (assoc ucistr uci-moves)))
		(if (equal? #f the-move)
			(error 'enter-move "invalid move entered" ucistr)
			(make-board (cdr the-move) cp))))

(define (from-uci-position params)
	(cond
		((not (equal? #f (assoc 'startpos params))) 
			(board-new))
		((not (equal? #f (assoc 'fen params))) 
			(board-from-fen 
				(substring (cdr (assoc 'fen params)) 0 (- (string-length (cdr (assoc 'fen params))) 1))))
		(else (board-new))))

(define (cmd-position params)
	(let ((cp
					(if (equal? #f (assoc 'moves params))
						(from-uci-position params)
						(let loop ((cp (from-uci-position params)) (moves (cdr (assoc 'moves params))))
							(if	(null? moves) (reset-ply cp) (loop (enter-uci-move (car moves) cp) (cdr moves)))))))
		(if (zero? moves-in-book) (set! moves-in-book (chessp-fm cp)))
		cp))

(define (uci-info lst)
	(cond
		((and (not (equal? #f (assoc 'nodes lst))) (can-search?)) ; avoid report pv while aborting search
			(display "info ")
			(if (>= (abs (cdr (assoc 'score lst))) MATE)
				(begin
					(display "score mate ")
					(if (>= (cdr (assoc 'score lst)) MATE)
						(display (inexact->exact (quotient (+ 1 (- checkmate-score (cdr (assoc 'score lst)))) 2)))
						(display (inexact->exact (quotient (- (- (cdr (assoc 'score lst))) checkmate-score) 2)))))
				(begin
					(display "score cp ")
					(display (inexact->exact (round (cdr (assoc 'score lst)))))))
			(if (not (equal? #f (assoc 'upperbound lst)))
				(display " upperbound"))
			(if (not (equal? #f (assoc 'lowerbound lst)))
				(display " lowerbound"))
			(display " depth ")
			(display (cdr (assoc 'depth lst)))
			(display " nodes ")
			(display (cdr (assoc 'nodes lst)))
			(display " time ")
			(display (inexact->exact (cdr (assoc 'ms lst))))
			(if (not (null? (cdr (assoc 'pv lst))))
				(begin
					(display " pv")
					(for-each (lambda (s) (display " ") (display s))
						(map (lambda (m) (uci-notation m)) (cdr (assoc 'pv lst))))))
			(newline)
			(if (> (/ (cdr (assoc 'ms lst)) 1000.) 0.)
				(begin
					(display "info nps ")
					(display (inexact->exact (round (/ (cdr (assoc 'nodes lst)) (/ (cdr (assoc 'ms lst)) 1000.)))))
					(newline))))
	((not (equal? #f (assoc 'bestmove lst)))
		(display "bestmove ")
		(display (uci-notation (cdr (assoc 'bestmove lst))))
		(if (not (equal? #f (assoc 'ponder lst)))
			(begin
				(display " ponder ")
				(display (uci-notation (cdr (assoc 'ponder lst))))))
		(newline))
	((not (equal? #f (assoc 'string lst)))
		(display "info string") (display (any-list->string (cdr (assoc 'string lst)))) (newline))
	((not (equal? #f (assoc 'currmove lst)))
		(display "info depth ")
		(display (cdr (assoc 'depth lst)))
		(display " currmove ")
		(display (uci-notation (cdr (assoc 'currmove lst)))) 
		(display " currmovenumber ")
		(display (cdr (assoc 'currmovenumber lst)))
		(newline)))
	(force-output))

(define (spawn thunk)
  (let ((t (make-thread thunk)))
    (thread-specific-set! t #t)
    (thread-start! t)
    t))

(define (stop! thread)
	(thread-specific-set! thread #f)
  (thread-join! thread))

(define (report-bestmove cp ponder)
	(let ((lookup (tt-ref cp)))
		(if (not (equal? #f lookup))
			(if (null? (cadddr lookup))
				(uci-info (list (cons 'score (car lookup))))
				(if (and ponder (> (length (cadddr lookup)) 1))
					(uci-info	(list
						(cons 'score (car lookup))
						(cons 'bestmove (car (cadddr lookup)))
						(cons 'ponder (cadr (cadddr lookup)))
					))
					(uci-info (list
						(cons 'score (car lookup))
						(cons 'bestmove (car (cadddr lookup)))))))))
	#f)

(define (engine-thunk params cp max-depth ponder debug)
	(define window 25)
	(lambda ()
		(let ((lookup (tt-ref cp)))
			(if debug (uci-info (list (cons 'string (list "tt-lookup=" lookup)))))
			(if (and (time-not-expired?)
						(cond
							((not (equal? #f (assoc 'mate params)))
								(if (not (equal? #f lookup))
									(if (and (< (abs (car lookup)) MATE) (< (caddr lookup) (* 2 (cdr (assoc 'mate params)))))
										(begin (deep (+ 1 (caddr lookup)) (car lookup) window cp debug)
											(can-search?))
										#f)
									(begin (deep 1 MATE checkmate-score cp debug)
										(can-search?))))
							((and (not (equal? #f (assoc 'ponder params))) (not (equal? #f lookup)))
								(if (and (< (abs (car lookup)) MATE)
												(< (caddr lookup)
													(if (equal? #f (assoc 'depth params)) max-depth (cdr (assoc 'depth params)))))
									(begin (deep (+ 1 (caddr lookup)) (car lookup) window cp debug)
										(can-search?))
									(begin (thread-yield!)
										(and (can-search?) (null? ms-time-limit))))) ; not allowed to stop while in ponder mode
							((not (equal? #f lookup))
								(if (and (< (abs (car lookup)) MATE)
												(< (caddr lookup)
													(if (equal? #f (assoc 'depth params)) max-depth (cdr (assoc 'depth params)))))
									(begin (deep (+ 1 (caddr lookup)) (car lookup) window cp debug)
										(can-search?))
									#f))
							((and (not (equal? #f lookup)) (equal? (abs (car lookup)) checkmate-score))
								#f)
							(else	(not (equal? (abs (car (deep 1 0 checkmate-score cp debug))) checkmate-score)))))
				#t
				(report-bestmove cp ponder)))))

(define (go! engine)
	(lambda ()
		(let loop ()
			(if (equal? #f (engine)) (thread-specific-set! (current-thread) #f))
			(thread-yield!)
			(if (thread-specific (current-thread)) (loop)))))

(define (cmd-go params cp max-depth ponder debug)
	(set-clock! cp params)
	(spawn (go! (engine-thunk params cp max-depth ponder debug))))

; start the clock after ponderhit command
(define (cmd-ponderhit)
	(set! ms-time-limit (if (null? ponderhit-time-limit) '() (+ (msec-time) ponderhit-time-limit))))

(define (cmd-perft params cp)
	(let* ((t1 (msec-time))
				 (result (perft (cdr (assoc 'depth params)) cp))
				 (t2 (- (msec-time) t1)))
		(uci-info (list (cons 'string (list "depth=" (cdr (assoc 'depth params)) "nodes=" result "time=" t2
								"nps=" (inexact->exact (round (/ result (if (zero? (/ t2 1000.)) 1. (/ t2 1000.)))))))))))

(define (cmd-divide params cp)
	(let* ((t1 (msec-time))
				 (result (divide (cdr (assoc 'depth params)) cp))
				 (t2 (- (msec-time) t1)))
		(uci-info (list (cons 'string (list "depth=" (cdr (assoc 'depth params)) "nodes=" result "time=" t2
								"nps=" (inexact->exact (round (/ result (if (zero? (/ t2 1000.)) 1. (/ t2 1000.)))))))))))

(define (cmd-newgame)
	(set! moves-in-book 0)
	(tt-reset) (##gc))

(define (cmd-fen cp)
	(uci-info (list (cons 'string (board-to-fen cp)))))

(define (make-test-verifier epd)
	(cond
		((not (equal? #f (assoc 'bm epd)))
			(lambda (move)
				(any?	(lambda (m) (san-equal? m (san move (board-from-fen (cdr (assoc 'fen epd))))))
					(cdr (assoc 'bm epd)))))
		((not (equal? #f (assoc 'am epd)))
			(lambda (move)
				(all?	(lambda (m) (not (san-equal? m (san move (board-from-fen (cdr (assoc 'fen epd)))))))
					(cdr (assoc 'am epd)))))
		(else (error 'make-test-verifier (list "unsupported epd opcode in" epd)))))

(define (solve-epd epd params max-depth ponder debug)
	(cmd-newgame)
	(uci-info (list (cons 'string (list "testing position" (assoc 'id epd)))))
	(let* ((position (board-from-fen (cdr (assoc 'fen epd)))) (engine (engine-thunk params position max-depth ponder debug))
				 (test-verifier (make-test-verifier epd)))
		(set-clock! position params)
		(let loop ((solving (engine)))
			(if solving
				(loop (engine))
				(let ((lookup (tt-ref position)))
					(and (not (equal? #f lookup)) (not (null? (cadddr lookup))) (test-verifier (car (cadddr lookup)))))))))

(define (cmd-epdscore params max-depth ponder debug)
	(let* ((epds (map parse-epd (call-with-input-file (cdr (assoc 'filename params)) (lambda (p)	(read-all p (lambda (p) (read-line p)))))))
				 (solved-epds (filter (lambda (epd) (solve-epd epd params max-depth ponder debug)) epds)))
		(uci-info (list (cons 'string (list "total tested positions =" (length epds) "total solved positions =" (length solved-epds)))))
		(for-each
			(lambda (epd)
				(if (equal? #f (member epd solved-epds))
					(uci-info (list (cons 'string (list "unsolved =" epd))))
					(uci-info (list (cons 'string (list "  solved =" epd))))))
			epds)))

(define (index-of-char c str)
	(let loop ((i 0))
		(cond
			((= i (string-length str)) -1)
			((char=? c (string-ref str i)) i)
			(else (loop (+ 1 i))))))

(define (parse-epd str)
	(let* ((tokens (call-with-input-string str (lambda (p) (read-all p (lambda (p) (read-line p #\space))))))
				 (fen (append-strings (list (list-ref tokens 0) " " (list-ref tokens 1) " " (list-ref tokens 2) " " (list-ref tokens 3))))) 
		(let loop ((rest (cdddr tokens)) (opcodes '()) (op '()))
			(cond
				((null? rest) (cons (cons 'fen fen) opcodes))
				((string=? "bm" (car rest)) (loop (cdr rest) opcodes (cons 'bm '())))
				((string=? "am" (car rest)) (loop (cdr rest) opcodes (cons 'am '())))
				((string=? "id" (car rest)) (loop (cdr rest) opcodes (cons 'id '())))
				((not (null? op))
					(let ((idx (index-of-char #\; (car rest))))
						(if (>= idx 0)
							(loop (cdr rest) (cons (cons (car op) (cons (substring (car rest) 0 idx) (cdr op))) opcodes) '())
							(loop (cdr rest) opcodes (cons (car op) (cons (car rest) (cdr op)))))))
				(else (loop (cdr rest) opcodes op))))))

(define (parse-uci-option lst)
	(cons 
		(string->symbol (list->string (map char-downcase (string->list (car lst)))))
		(if (null? (cdr lst))
			0
			(if (string=? (cadr lst) "value")	(caddr lst)	0))))

(define (parse-uci-options lst)
	(cond
		((null? lst) '())
		((string=? (car lst) "name")
			(cons (parse-uci-option (cdr lst)) (parse-uci-options (cdr lst))))
		(else (parse-uci-options (cdr lst)))))

(define (build-fen-string lst)
	(cond
		((null? lst) '())
		((string=? "moves" (car lst)) '())
		(else	(cons (string-append (car lst) " ") (build-fen-string (cdr lst))))))

(define (parse-uci-position lst)
	(cond
		((null? lst) '())
		((string=? "fen" (car lst))
			(cons
				(cons 'fen 
					(fold
						(lambda (s acc) (string-append acc s))
						""
						(flatten (build-fen-string (cdr lst)))))
				(parse-uci-position (cdr lst))))
		((string=? "startpos" (car lst))
			(cons (cons 'startpos 0) (parse-uci-position (cdr lst))))
		((string=? "moves" (car lst))
			(cons (cons 'moves (cdr lst)) '() ))
		(else (parse-uci-position (cdr lst)))))

(define (parse-uci-go-option lst)
	(cons 
		(string->symbol (car lst))
		(if (null? (cdr lst))
			0
			(string->number (cadr lst)))))

(define (parse-uci-go lst)
	(cond
		((null? lst) '())
		((string=? (car lst) "infinite")
			(cons (cons 'infinite 0) (parse-uci-go (cdr lst))))
		((string=? (car lst) "wtime")
			(cons (parse-uci-go-option lst) (parse-uci-go (cdr lst))))
		((string=? (car lst) "btime")
			(cons (parse-uci-go-option lst) (parse-uci-go (cdr lst))))
		((string=? (car lst) "winc")
			(cons (parse-uci-go-option lst) (parse-uci-go (cdr lst))))
		((string=? (car lst) "binc")
			(cons (parse-uci-go-option lst) (parse-uci-go (cdr lst))))
		((string=? (car lst) "movestogo")
			(cons (parse-uci-go-option lst) (parse-uci-go (cdr lst))))
		((string=? (car lst) "movetime")
			(cons (parse-uci-go-option lst) (parse-uci-go (cdr lst))))
		((string=? (car lst) "depth")
			(cons (parse-uci-go-option lst) (parse-uci-go (cdr lst))))
		((string=? (car lst) "mate")
			(cons (parse-uci-go-option lst) (parse-uci-go (cdr lst))))
		((string=? (car lst) "ponder")
			(cons (parse-uci-go-option lst) (parse-uci-go (cdr lst))))
		(else (parse-uci-go (cdr lst)))))

(define (parse-epdscore lst)
	(cons (cons 'filename (car lst)) (parse-uci-go (cdr lst))))

(define (parse-uci-debug lst)
	(string=? (car lst) "on"))

(define (parse-line str debug)
	(if (string? str)
		(let* ((lst (string-split str #\space))
						(len (length lst)))
      (if debug
        (with-output-to-file (list path: "./uci-log.txt" create: 'maybe append: #t )
          (lambda ()
            (display (list "in:" str))
            (newline))))
			(cond
				((zero? len) '())
				((string=? "c" (list-ref lst 0)) (list (cons 'c 0)))
				((string=? "t" (list-ref lst 0)) (list (cons 't 0)))
				((string=? "fen" (list-ref lst 0)) (list (cons 'fen 0)))
				((string=? "uci" (list-ref lst 0)) (list (cons 'uci 0)))
				((string=? "ucinewgame" (list-ref lst 0)) (list (cons 'ucinewgame 0)))
				((string=? "isready" (list-ref lst 0)) (list (cons 'isready 0)))
				((string=? "ponderhit" (list-ref lst 0)) (list (cons 'ponderhit 0)))
				((string=? "stop" (list-ref lst 0)) (list (cons 'stop 0)))
				((string=? "quit" (list-ref lst 0)) (list (cons 'quit 0)))
				((string=? "setoption" (list-ref lst 0))
					(list (cons 'setoption (parse-uci-options (cdr lst)))))
				((string=? "position" (list-ref lst 0))
					(list (cons 'position (parse-uci-position (cdr lst)))))
				((string=? "go" (list-ref lst 0))
					(list (cons 'go (parse-uci-go (cdr lst)))))
				((string=? "debug" (list-ref lst 0))
					(list (cons 'debug (parse-uci-debug (cdr lst)))))
				((string=? "perft" (list-ref lst 0))
					(list (cons 'perft (parse-uci-go (cdr lst)))))
				((string=? "divide" (list-ref lst 0))
					(list (cons 'divide (parse-uci-go (cdr lst)))))
				((string=? "epdscore" (list-ref lst 0))
					(list (cons 'epdscore (parse-epdscore (cdr lst)))))
				(else '())))
		(list (cons 'quit 0))))

(define (uci-interface)
	(thread-specific-set! (current-thread) #t)
	(let loop ( (cp '()) (engine '()) (max-depth 10) (ponder #f) (debug #f))
		(if (can-search?)
			(let ((cmd (parse-line (read-line) debug)))
				; (pretty-print (list cmd engine))
				(cond
					((null? cmd) #t)
					((not (equal? #f (assoc 'c cmd)))
						(if (thread? engine)
							(pretty-print (list "thread=" engine (thread-specific engine)))
							(pretty-print (list "thread not running"))))
					((not (equal? #f (assoc 't cmd)))
						(pretty-print (list "table size=" (tt-size))))
					((not (equal? #f (assoc 'debug cmd))) 
						(loop cp engine max-depth ponder (cdr (assoc 'debug cmd))))
					((not (equal? #f (assoc 'quit cmd))) 
						(if (thread? engine) (stop! engine))
						(thread-specific-set! (current-thread) #f))
					((and (not (equal? #f (assoc 'fen cmd))) (not (null? cp)))
						(cmd-fen cp))
					((not (equal? #f (assoc 'uci cmd))) (cmd-uci))
					((not (equal? #f (assoc 'ucinewgame cmd)))
							(cmd-newgame)
							(loop cp engine max-depth ponder debug))
					((not (equal? #f (assoc 'setoption cmd)))
						(let ((new-opt (cmd-setoption (cdr (assoc 'setoption cmd)) max-depth ponder)))
							(loop cp engine (car new-opt) (cadr new-opt) debug)))
					((not (equal? #f (assoc 'isready cmd))) (cmd-isready))
					((not (equal? #f (assoc 'position cmd)))
						(loop (cmd-position (cdr (assoc 'position cmd))) engine max-depth ponder debug))
					((and (not (equal? #f (assoc 'go cmd))) (not (null? cp)))
						(loop cp (cmd-go (cdr (assoc 'go cmd)) cp max-depth ponder debug) max-depth ponder debug))
					((and (not (equal? #f (assoc 'perft cmd))) (not (null? cp)))
						(cmd-perft (cdr (assoc 'perft cmd)) cp))
					((and (not (equal? #f (assoc 'divide cmd))) (not (null? cp)))
						(cmd-divide (cdr (assoc 'divide cmd)) cp))
					((not (equal? #f (assoc 'epdscore cmd)))
						(cmd-epdscore (cdr (assoc 'epdscore cmd)) max-depth ponder debug))
					((not (equal? #f (assoc 'stop cmd)))
						(if (thread? engine) (stop! engine))
						(loop cp '() max-depth ponder debug))
					((not (equal? #f (assoc 'ponderhit cmd)))
						(cmd-ponderhit))
					(else #t))
			(if (can-search?) (loop cp engine max-depth ponder debug))))))

;; ---- test functions ----

;test
(define (test-game moves)
	(let loop ((mv moves) (p (board-new)))
		(cond
			((null? mv) 
				(print-fen p)
				(pretty-print (score p))
				(print-moves (board-moves p)))
			(else
				(print-fen p)
				; (pretty-print (score p))
;					(print-moves (board-moves p))
				(pretty-print (car mv))
				(if (not (equal? (zobrist-hash-from-cp zhash p) (chessp-zhash p)))
					(error 'zobrist-hash (board-to-fen p)))
				(let ((lookup (tt-ref p)))
					(cond
						((equal? lookup #f) (tt-set! p p))
						(else
							(if (equal? (board-to-fen p) (board-to-fen lookup))
								'ok
								(error 'zobrist-collision (board-to-fen p))))))
				(loop (cdr mv) (enter-move (car mv) p))))))

(define (go-test-games)

; en passant check mate
; 1. e4 e6 2. d4 d5 3. e5 c5 4. c3 cxd4 5. cxd4 Bb4+ 6. Nc3 Nc6
; 7. Nf3 Nge7 8. Bd3 O-O 9. Bxh7+ Kxh7 10. Ng5+ Kg6 11. h4 Nxd4
; 12. Qg4 f5 13. h5+ Kh6 14. Nxe6+ g5 15. hxg6# 1-0
		(test-game 
			(list "e4" "e6" "d4" "d5" "e5" "c5" "c3" "cxd4" "cxd4" "Bb4" "Nc3" "Nc6"
						"Nf3" "Nge7" "Bd3" "O-O" "Bxh7" "Kxh7" "Ng5" "Kg6" "h4" "Nxd4"
						"Qg4" "f5" "h5" "Kh6" "Nxe6" "g5" "hxg6"))

; stealmate
; 1.d4 e5 2.Qd2 e4 3.Qf4 f5 4.h3 Bb4+ 5.Nd2 d6 6.Qh2 Be6 7.a4
; Qh4 8.Ra3 c5 9.Rg3 f4 10.f3 Bb3 11.d5 Ba5 12.c4 e3
		(test-game
			(list 
						"d4" "e5" "Qd2" "e4" "Qf4" "f5" "h3" "Bb4" "Nd2" "d6" "Qh2" "Be6" "a4"
						"Qh4" "Ra3" "c5" "Rg3" "f4" "f3" "Bb3" "d5" "Ba5" "c4" "e3"))

; promotions
; 1. d4 e6 2. c4 f5 3. g3 Nf6 4. Bg2 Bb4+ 5. Bd2 Be7 6. Nc3 O-O
; 7. Nf3 d6 8. O-O Qe8 9. Qc2 Qh5 10. e4 e5 11. dxe5 dxe5
; 12. Nd5 Nxd5 13. exd5 Bf6 14. Bc3 Nd7 15. Qd1 Qf7 16. Qe2 Re8
; 17. Rfe1 e4 18. Nd4 Nc5 19. b3 Bd7 20. Qc2 c6 21. dxc6 bxc6
; 22. f3 a6 23. Rad1 Nd3 24. Rf1 Qg6 25. Ne2 Bg5 26. Nf4 Bxf4
; 27. gxf4 c5 28. Kh1 Qh5 29. fxe4 fxe4 30. Rxd3 exd3 31. Qxd3
; Bf5 32. Bd5+ Kf8 33. Qg3 Ra7 34. Be5 Rd7 35. Qe3 Rxd5 36. cxd5
; Qg6 37. Re1 Rd8 38. d6 Bd7 39. Qg3 Qf7 40. Kg1 Re8 41. f5 g6
; 42. f6 Qe6 43. Qh4 Qg4+ 44. Qxg4 Bxg4 45. Kf2 Kf7 46. Bb2 Rxe1
; 47. Kxe1 Bd7 48. Kd2 Ke6 49. Be5 a5 50. Kc3 Bb5 51. a4 Be8
; 52. Bg3 Kd7 53. Kb2 Ke6 54. Kc2 c4 55. Be1 cxb3+ 56. Kxb3 Kxf6
; 57. Bxa5 Ke6 58. Bc7 g5 59. a5 g4 60. Kc4 h5 61. a6 h4 62. Kc5
; g3 63. hxg3 h3 64. g4 Bd7 65. a7 Bc6 66. g5 Kd7 67. g6 h2
; 68. g7 h1=Q 69. g8=Q Qc1+ 70. Qc4 Qxc4+ 71. Kxc4 Bh1 1/2-1/2
		(test-game
			(list 
						"d4" "e6" "c4" "f5" "g3" "Nf6" "Bg2" "Bb4" "Bd2" "Be7" "Nc3" "O-O"
						"Nf3" "d6" "O-O" "Qe8" "Qc2" "Qh5" "e4" "e5" "dxe5" "dxe5"
						"Nd5" "Nxd5" "exd5" "Bf6" "Bc3" "Nd7" "Qd1" "Qf7" "Qe2" "Re8"
						"Rfe1" "e4" "Nd4" "Nc5" "b3" "Bd7" "Qc2" "c6" "dxc6" "bxc6"
						"f3" "a6" "Rad1" "Nd3" "Rf1" "Qg6" "Ne2" "Bg5" "Nf4" "Bxf4"
						"gxf4" "c5" "Kh1" "Qh5" "fxe4" "fxe4" "Rxd3" "exd3" "Qxd3"
						"Bf5" "Bd5" "Kf8" "Qg3" "Ra7" "Be5" "Rd7" "Qe3" "Rxd5" "cxd5"
						"Qg6" "Re1" "Rd8" "d6" "Bd7" "Qg3" "Qf7" "Kg1" "Re8" "f5" "g6"
						"f6" "Qe6" "Qh4" "Qg4" "Qxg4" "Bxg4" "Kf2" "Kf7" "Bb2" "Rxe1"
						"Kxe1" "Bd7" "Kd2" "Ke6" "Be5" "a5" "Kc3" "Bb5" "a4" "Be8"
						"Bg3" "Kd7" "Kb2" "Ke6" "Kc2" "c4" "Be1" "cxb3" "Kxb3" "Kxf6"
						"Bxa5" "Ke6" "Bc7" "g5" "a5" "g4" "Kc4" "h5" "a6" "h4" "Kc5"
						"g3" "hxg3" "h3" "g4" "Bd7" "a7" "Bc6" "g5" "Kd7" "g6" "h2"
						"g7" "h1=Q" "g8=Q" "Qc1" "Qc4" "Qxc4" "Kxc4" "Bh1"))
)

(define (test-position fen-string)
	(let ((cp (board-from-fen fen-string)))
		(if (string=? fen-string (board-to-fen cp))
			(begin
				(if (equal? white (chessp-side cp))
					(display "White to move")
					(display "Black to move"))
				(newline)
				(print-moves (board-moves cp)))
			(error 'test-position (string-append "Error importing FEN position: " (board-to-fen cp))))))

(define (test-checkmate n fen-string)
	(thread-specific-set! (current-thread) #t)
	(time
		(let* ((cp (board-from-fen fen-string)) (result (deep (* 2 n) 0 checkmate-score cp #f)))
			(display (car result))
			(display " ")
			(print-moves (cdr result)))))

; https://en.wikipedia.org/wiki/Checkmate
(define (go-test-is-mate)

	; fools mate
	(let ((pos (board-from-fen "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 0 1")))
		(if (and (is-checkmate? pos) (not (legal-moves-available? pos)))
			'ok
			(error 'test-is-mate (list (board-to-fen pos) (is-checkmate? pos) (not (legal-moves-available? pos)) ))))

	; D. Byrne vs. Fischer
	(let ((pos (board-from-fen "1Q6/5pk1/2p3p1/1p2N2p/1b5P/1bn5/2r3P1/2K5 w - - 0 1")))
		(if (and (is-checkmate? pos) (not (legal-moves-available? pos)))
			'ok
			(error 'test-is-mate (list (board-to-fen pos) (is-checkmate? pos) (not (legal-moves-available? pos)) ))))

	; rook checkmate
	(let ((pos (board-from-fen "8/8/8/5K1k/8/8/7R/8 b - - 0 1")))
		(if (and (is-checkmate? pos) (not (legal-moves-available? pos)))
			'ok
			(error 'test-is-mate (list (board-to-fen pos) (is-checkmate? pos) (not (legal-moves-available? pos)) ))))

	; rook queen checkmate
	(let ((pos (board-from-fen "8/8/8/8/8/8/5r2/4K1qk w - - 0 1")))
		(if (and (is-checkmate? pos) (not (legal-moves-available? pos)))
			'ok
			(error 'test-is-mate (list (board-to-fen pos) (is-checkmate? pos) (not (legal-moves-available? pos)) ))))

	; don't work!
	; (let ((pos (board-from-fen "r2qkb1r/pp2nppp/3p4/1BpNN1B1/3nP3/3P4/PPP2PPP/R2bK2R b KQkq - 2 1")))
	; 	(if (and (is-checkmate? pos) (not (legal-moves-available? pos)))
	; 		'ok
	; 		(error 'test-is-mate (list (board-to-fen pos) (is-checkmate? pos) (not (legal-moves-available? pos)) ))))
	; (let ((pos (board-from-fen "8/2r5/1k5p/1pp4P/8/K1qP4/PR2QB2/8 w - - 1 2")))
	; 	(if (and (is-checkmate? pos) (not (legal-moves-available? pos)))
	; 		'ok
	; 		(error 'test-is-mate (list (board-to-fen pos) (is-checkmate? pos) (not (legal-moves-available? pos)) ))))
	; (let ((pos (board-from-fen "rn3b1r/6pp/pp2Q3/4R2k/3R4/2N1BN2/PP3PPP/2K5 b - - 0 22")))
	; 	(if (and (is-checkmate? pos) (not (legal-moves-available? pos)))
	; 		'ok
	; 		(error 'test-is-mate (list (board-to-fen pos) (is-checkmate? pos) (not (legal-moves-available? pos)) ))))

)

; from http://wtharvey.com/m8n2.txt

; (test-checkmate 1 "8/8/8/qn6/kn6/1n6/1KP5/8 w - - 1 1")

; da testare
; (iterative-deepening 1 (board-from-fen "6k1/1p4pp/p5P1/1p1p4/5R2/8/2r3PP/6K1 w - - 0 1") 0 '())

(define (go-test-checkmate)

; from http://wtharvey.com/m8n2.txt
; 1. Nf6+ gxf6 2. Bxf7# -- Kxd1 g6 Nf6
		(set! transposition-table (make-table))
		(test-checkmate 2 "r2qkb1r/pp2nppp/3p4/2pNN1B1/2BnP3/3P4/PPP2PPP/R2bK2R w KQkq - 1 0")

; from http://wtharvey.com/m8n2.txt
; 1. Ne7 Nxf5 2. Qg6#  
		(set! transposition-table (make-table))
		(test-checkmate 2 "r2qrb2/p1pn1Qp1/1p4Nk/4PR2/3n4/7N/P5PP/R6K w - - 1 0")

; from http://wtharvey.com/m8n2.txt
; 1... Qc3+ 2. Rb3 Ra7#
		(set! transposition-table (make-table)) 
		(test-checkmate 2 "8/2r5/1k5p/1pp4P/8/K2P4/PR2QB2/2q5 b - - 0 1")

; from http://wtharvey.com/m8n2.txt
; 1... Kh6 2. c5 g5# 
		(set! transposition-table (make-table))
		(test-checkmate 2 "8/2r2pk1/3p2p1/3Pb3/2P1P2K/6r1/1R2B3/1R6 b - - 0 1")

; from http://wtharvey.com/m8n3.txt
; 1... Bc5+ 2. Kxc5 Qb6+ 3. Kd5 Qd6#
		(set! transposition-table (make-table))
		(test-checkmate 3 "r1b1kb1r/pppp1ppp/5q2/4n3/3KP3/2N3PN/PPP4P/R1BQ1B1R b kq - 0 1")

; from http://wtharvey.com/m8n3.txt
; 1. Bb5+ c6 2. Qe6+ Qe7 3. Qxe7# 
		(set! transposition-table (make-table))
		(test-checkmate 3 "r3k2r/ppp2Npp/1b5n/4p2b/2B1P2q/BQP2P2/P5PP/RN5K w kq - 1 0")
	)

; https://www.chessprogramming.org/Perft
(define (perft depth cp)
	; with 'all testing ordering speed    is :-( circa 60K- 80K nds interpreted
	; with 'unordered only move gen speed is :-( circa 90K-140K nds interpreted
	(let ((next-move (moves-sorter-tt 'unordered cp)))
		(let loop ((curr-move (next-move)) (nodes 0))
			(cond
				((null? (cdr curr-move)) nodes)
				((<= depth 1)
					(do ((i 0 (fx+ i 1)) (m curr-move))
						((null? (cdr m)) (fx+ nodes i))
						(set! m (next-move))))
				(else
					(loop	(next-move)
						(+ nodes (perft (- depth 1) (make-board (cdr curr-move) cp)))))))))

(define (divide depth cp)
	(let loop ((next-move (moves-sorter-tt 'unorderd cp)) (total 0)) 
		(let ((move (next-move)))
			(cond
				((null? (cdr move))
					(newline)
					(display "Nodes searched: ") (display total)
					(newline) (newline)
					total)
				(else
					(let ((count (if (> depth 1) (perft (- depth 1) (make-board (cdr move) cp)) 1)))
						(display (uci-notation (cdr move)))
						(display " ")
						(display count)
						(newline)
					(loop next-move (+ total count) )))))))

; test legal ep special case 
; "8/8/8/8/k2Pp2Q/8/8/3K4 b - d3 0 1" -> 6 legal moves, ep not allowed
; "8/8/8/8/k2Ppp1Q/8/8/3K4 b - d3 0 1" -> 8 legal moves, ep allowed

;; https://www.chessprogramming.org/Perft_Results
(define (go-test-perft)
;; initial position - ok
; (time (legal-debugger 5 (board-new)))

;; position 2 - ok
(time (pretty-print (equal? 4085603 (perft 4 (board-from-fen "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")))))

;; position 3 - ok
(time (pretty-print (equal? 11030083 (perft 6 (board-from-fen "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")))))

;; position 4 - ok
(time (pretty-print (equal? 422333 (perft 4 (board-from-fen "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")))))

;; position 5 - ok
(time (pretty-print (equal? 62379 (perft 3 (board-from-fen "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")))))

;; position 6 - ok
(time (pretty-print (equal? 3894594 (perft 4 (board-from-fen "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10")))))

)

(uci-interface)
