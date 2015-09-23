(ns slack-ghost.bot-util)

(defn search
  ([list target key]
   (search target key (first list) (rest list)))
  ([target key head tail]
   (if head
     (if (= target (key head)) 
       head
       (search target key (first tail) (rest tail))))))

(defn search-user [this data]
  (search (:users this) (:user data) :id))

(defn search-channel [this data]
  (search (:channels this) (:channel data) :id))

(defn search-channel-with-name [this name]
  (search (:channels this) name :name))
