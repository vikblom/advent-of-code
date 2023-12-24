
(declare-const x Real)
(declare-const y Real)
(declare-const z Real)

(declare-const dx Real)
(declare-const dy Real)
(declare-const dz Real)

(declare-const t0 Real)
(declare-const t1 Real)
(declare-const t2 Real)

(push)

(assert (= (+ x (* t0 dx)) (+ 380596900441035 (* t0 (- 141)))))
(assert (= (+ y (* t0 dy)) (+ 475034410013298 (* t0 (- 244)))))
(assert (= (+ z (* t0 dz)) (+ 238677466991589 (* t0 154))))

(assert (= (+ x (* t1 dx)) (+ 233796913851006 (* t1 54))))
(assert (= (+ y (* t1 dy)) (+ 262774170759556 (* t1 10))))
(assert (= (+ z (* t1 dz)) (+ 265925724673108 (* t1 23))))

(assert (= (+ x (* t2 dx)) (+ 276006064958748 (* t2 14))))
(assert (= (+ y (* t2 dy)) (+ 296055609314709 (* t2 21))))
(assert (= (+ z (* t2 dz)) (+ 391999646036593 (* t2 24))))

(check-sat)

(get-model)
