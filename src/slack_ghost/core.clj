(ns slack-ghost.core
  (:gen-class main true)
  (:require [slack-ghost.bot-core :as bot]))
 
(defn print-message [this data]
 (println (:text data)))

(defn -main [& args]
  (let [token (first args)
        event {:message [#'print-message]}
       bot (bot/constructor-bot token event)]
    (bot/connect bot)))
