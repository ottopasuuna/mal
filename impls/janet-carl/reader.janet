
(def Reader
  @{:tokens nil
    :cur-pos 0
    :next (fn next [self]
            (do
              (def val (:peek self))
              (set (self :cur-pos) (++ (self :cur-pos)))
              val))
    :peek (fn peak [self]
            (if (>= (self :cur-pos) (length (self :tokens)))
              (error "Reader error: Tried to peek out of token bounds"))
            ((self :tokens) (self :cur-pos)))
    :read_form (fn read_form [self]
                 (case (:peek self)
                   "(" (:read_list self)
                   (:read_atom self)))
    :read_atom (fn read_atom [self]
                 (cond
                   (scan-number (:peek self)) (scan-number (:next self))
                   (= (:peek self) "true") (do (:next self) true)
                   (= (:peek self) "false") (do (:next self) false)
                   (= (:peek self) "nil") (do (:next self) nil)
                   (:next self)))
    :read_list (fn read_list [self]
                 (var list @[])
                 (try
                  (do
                   (:next self) # Consume the "(" token
                   (while (not (= (:peek self) ")" ))
                    (array/push list (:read_form self)))
                   (:next self)) # Consume the ")" token
                  ([err] (if (string/find "out of token bounds" err)
                             (error "Reader error: unbalanced parens")
                             (error err))))
                 list)
    })

(defn make-reader [tokens]
  (do
    (var reader (table/setproto @{} Reader))
    (set (reader :tokens) tokens)
    reader))

(def MALTokens
  '{:whitespace-comma (some (choice "," :s))
    :tilde-at (capture "~@")
    :special-chars (capture (set "[]{}()'`~^@"))
    :string (capture (sequence
                       "\""
                       (any (if-not "\""
                              1))
                       "\""))
    :non-special-words (capture (some (if-not (choice :whitespace-comma (set "[]{}'\"`,;()"))
                                        1)))
    :token (choice :whitespace-comma
                   :tilde-at
                   :string
                   :special-chars
                   :non-special-words)
    :main (some :token)})
(peg/match MALTokens "    ,  ~@ []`")
(peg/match MALTokens "\"test\"")
(peg/match MALTokens "test it")
(peg/match MALTokens "(123 456)")

(defn tokenize [str]
  (peg/match MALTokens str))


(defn read_str [str]
  (let [tokens (tokenize str)
        reader (make-reader tokens)]
    (:read_form reader)))

(read_str "123")

(read_str "123 ")
(read_str "abc")
(read_str "abc ")
(read_str "(123 456)")
(read_str "( 123 456 789 )")
(read_str "( + 2 (* 3 4) )")
#(read_str "(1 2") #invalid
