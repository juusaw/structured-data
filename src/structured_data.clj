(ns structured-data)

(defn do-a-thing [x]
  (let [sum (+ x x)]
  (Math/pow sum sum)
  ))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v] (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[a b][c d]] rectangle] (- c a)))

(defn height [rectangle]
  (let [[[a b][c d]] rectangle] (- d b)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[a b][c d]] rectangle
    [x y] point]
    (and (<= a x c) (<= b y d))
  ))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))
  ))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
  (map get-second collection)
  ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (let [get-authors (fn [book] (get book :authors))]
  (apply clojure.set/union (map get-authors books))))

(defn all-author-names [books]
  (let [get-name (fn [key] (get key :name))]
    (set (map get-name (authors books)))))

(defn author->string [author]
  (let [name (get author :name)
        time (cond (contains? author :death-year) (str " (" (get author :birth-year) " - " (get author :death-year) ")")
                   (contains? author :birth-year) (str " (" (get author :birth-year) " - )"))]
    (str name time)
  ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [name (get book :title)
        authors (authors->string (get book :authors))]
  (str name ", written by " authors)
  ))

(defn books->string [books]
  (let [begin (cond
    (= 0 (count books)) "No books"
    (= 1 (count books)) "1 book. "
    :else (str (count books) " books. "))]
    (str begin (apply str (interpose ". " (map book->string books))) ".")
    ))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (not (get author :death-year))) authors))

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
