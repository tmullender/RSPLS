(ns rspls.core
  (:require [clojure.math.combinatorics :refer [combinations]])
  (:gen-class))

(def options {:rock #{:lizard :scissors}
              :scissors #{:paper :lizard}
              :paper #{:rock :spock}
              :lizard #{:spock :paper}
              :spock #{:scissors :rock}})

(defn random-selection [current opponent]
  "A selection strategy that is random"
  (rand-nth (keys options)))

(defn copy-opponent [current opponent]
  "A selection strategy that copies the opponents last move"
  (let [chosen (:choices opponent)]
    (case chosen '() (random-selection current opponent) (first chosen))))

(defn ordered [current opponent]
  "A selection strategy that iterates over the available options"
  (let [all (keys options)
        chosen (:choices current)
        last (.indexOf all (first chosen))]
    (case chosen
      '() (random-selection current opponent)
      (nth all (rem (inc last) (count all)))))
  )

; The list of available strategies
(def strategies [random-selection ordered copy-opponent])

(defn create-player [name]
  "Creates a new player based on the given name"
  (let [strategy (rand-nth strategies)]
    (println name strategy)
    {:name name :strategy strategy :played 0 :games 0 :matches 0 :choices '()}))

(defn beats? [optionOne optionTwo]
  "Returns true if optionOne beats optionTwo"
  (let [result (contains? (options optionOne) optionTwo)]
    (println optionOne "vs" optionTwo)
    result
  ))

(defn increment [player key]
  "Increases the number associated with the given key"
  (assoc player key (inc (key player))))

(defn display [player]
  "Returns a map containing the keys to display"
  (select-keys player '(:name :matches :games)))

(defn add-choice [player choice]
  "Adds the choice to the list of choices already made"
  (update (increment player :played) :choices #(conj % choice))
  )

(defn play-game [one two]
  "Plays a game between two players by making a choice for each and then comparing them"
  (let [onesChoice ((:strategy one) one two)
        twosChoice ((:strategy two) two one)
        uOne (add-choice one onesChoice)
        uTwo (add-choice two twosChoice)]
    (cond
      (= onesChoice twosChoice) (play-game uOne uTwo)
      (beats? onesChoice twosChoice) [(increment uOne :games) uTwo]
      :else [uOne (increment uTwo :games)])
    )
  )

(defn play-match [one two best-of]
  "Plays a match, the best of best-of games, between two players"
  (let [wins (* best-of 0.5)]
    (println (display one) "vs" (display two))
    (cond
      (< wins (:games one)) [(increment one :matches) two]
      (< wins (:games two)) [one (increment two :matches)]
      :else (apply play-match (conj (play-game one two) best-of))
      )
    )
  )

(defn collect-scores [acc result]
  "Sum the number of matches and games"
  (let [name (:name result)
        scores (select-keys result '(:games :matches))]
    (if (contains? acc name)
      (assoc acc name (merge-with + scores (acc name)))
      (assoc acc name scores)
      )
    )
  )

(defn -main
  "We're going to play a game or two..."
  [best-of & players]
  (->> (combinations (map create-player players) 2)
       (map #(play-match (first %1) (second %1) (Integer/parseInt best-of)))
       (flatten)
       (reduce collect-scores {})
       (println "Final Scores")
       )

  )


