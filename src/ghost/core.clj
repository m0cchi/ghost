(ns ghost.core
  (:gen-class main true)
  (:require [clj-slack.rtm :as rtm]
            [clj-slack.channels :as channels]
            [clj-slack.groups :as groups]
            [clj-slack.users :as users]
            [clojure.data.json :as json]
            [gniazdo.core :as ws]))

(defprotocol IBot
  (url [this])
  (connect [this])
  (receiver [this response])
  (select-func [this type keys])
  (on-type [this message]))

(defrecord Bot [connection users channels groups event]
  IBot
  (url [this]
    (:url (rtm/start (:connection this))))
  (connect [this]
    (let [s (ws/connect (url this)
                        :on-receive #(receiver this %))]
      s))
  (select-func [this type keys]
    (let [nop (fn [x] x)]
      (if (contains? (:event this) (keyword type))
        (cond
          (and (= type "message")
               (or
                (< -1 (.indexOf keys :reply_to))
                (< -1 (.indexOf keys :deleted_ts)))) nop
                :else #(on-type this %))
        nop)))
  (receiver [this response]
    (let [data (json/read-str response :key-fn keyword)
          type (:type data)
          keys (keys data)
          on (select-func this type keys)]
      (on data)))
  (on-type [this message]
    (let [message-fns ((keyword (:type message)) (:event this))]
      (doseq [message-fn message-fns]
        (message-fn message)))))

(defrecord User [id name])
  
(defrecord Channel [id name members])

(defrecord Group [id name members])

(defn- constructor-group [group]
  (let [id (:id group) 
        name (:name group)
        members (:members group)]
    (if id
      (->Group id name members)
      nil)))

(defn- constructor-channel [channel]
  (let [id (:id channel)
        name (:name channel)
        members (:members channel)]
    (if id
      (->Channel id name members)
      nil)))

(defn- constructor-user
  "create with hash"
 [member]
 (->User (:id member) (:name member)))

(defn- create
  ([create-fn data]
   (create create-fn (first data) (rest data)))
  ([create-fn head tail]
   (create create-fn head tail []))
  ([create-fn head tail workpieces]
   (let [workpiece (create-fn head)
        workpieces(if workpiece (conj workpieces workpiece) workpieces)]
     (if-not (empty? tail)
       (create create-fn (rest head) (rest tail) workpieces)
       workpieces))))

(defn constructor-bot
  ([token]
   (constructor-bot token {:message [#(println %)]}))
  ([token event]
   (let [connection {:api-url "https://slack.com/api" :token token}
         users (create #'constructor-user (:members (users/list connection)))
         channels (create #'constructor-channel (:channels (channels/list connection)))
         groups (create #'constructor-group (:groups (groups/list connection)))
         bot (->Bot connection users channels groups event)]
     bot)))
 
(defn print-message [data]
 (println (:text data)))

(defn -main [& args]
  (let [token (first args)
        event {:message [#'print-message]}
       bot (constructor-bot token event)]
    (connect bot)))
