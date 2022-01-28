(ns interpreter)

(def inp (atom 0))                                      ; Input pointer
(def dp (atom 0))                                       ; Data pointer
(def ip (atom 0))                                       ; Instruction pointer
(def tape (atom (apply vector (take 5000 (repeat 0))))) ; Memory tape
(def output (atom ""))                                  ; Output of program

(defn inc-8bit [num]
  (mod (+ 1 num) 256))

(defn dec-8bit [num]
  (let [res (- num 1)]
    (if (= res -1) 255 res)))

(defn jump
  "Jump to matching delimiter, tracking nested jumps as we go."
  [source open close f]
  (loop [nests 0]
    (swap! ip f)
    (if (and (= 0 nests) (= close (nth source @ip))) ()
        (recur (if (= open (nth source @ip)) (inc nests) nests)))))

(defn execute-string
  [source input]
  (if (> (if-let [res (get (frequencies source) \,)] res 0) (count (map char input))) nil
      (do (while (< @ip (count source))
            (case (nth source @ip)
              \< (swap! dp dec)
              \> (swap! dp inc)
              \+ (swap! tape update @dp inc-8bit)
              \- (swap! tape update @dp dec-8bit)
              \, (do (swap! tape (nth input @inp) @dp) (swap! inp inc))
              \. (swap! output str (char (nth @tape @dp)))
              \[ (when (= 0 (nth @tape @dp)) (jump source \[ \] inc))
              \] (when (not (= 0 (nth @tape @dp))) (jump source \] \[ dec))
              :default)
            (swap! ip inc))
          @output)))
