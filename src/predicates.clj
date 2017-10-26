(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn greater-than [n]
  (fn [k] (> k n)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (fn [a-key] (contains? a-set a-key)))

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))

(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (or (empty? string) (nil? string) (every? whitespace? string)))

(defn has-award? [book award]
  (contains? (:awards book) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
   (let [has-award1? (fn [award] (contains? (:awards book) award))]
    (every? has-award1? awards)))

(defn my-some [pred a-seq]
  (let [first-pass (filter pred a-seq)]
    (cond
      (empty? first-pass) false
      (coll? (first first-pass)) (first (flatten first-pass))
      :else true
      )
    ))

(defn my-every? [pred a-seq]
  (empty? (filter (complement pred) a-seq)))

(defn prime? [n]
  (let [divides? (fn [k] (= (mod n k) 0))]
    (not (some divides? (range 2 n)))))
;^^
