(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error [& args]
  (apply println args)
  :error)

(defn add-egg []
  (grab :egg)
  (squeeze)
  (add-to-bowl))

(defn add-flour []
  (grab :cup)
  (scoop :flour)
  (add-to-bowl)
  (release))

(defn add-milk []
  (grab :cup)
  (scoop :milk)
  (add-to-bowl)
  (release))

(defn add-sugar []
  (grab :cup)
  (scoop :sugar)
  (add-to-bowl)
  (release))

(defn add-butter []
  (grab :butter)
  (add-to-bowl))

(def scoop-ingredients #{:milk :flour :sugar})

(def squeeze-ingredients #{:egg})

(def simple-ingredients #{:butter})

(defn scooped? [ingredient]
  (contains? scoop-ingredients ingredient))

(defn squeezed? [ingredient]
  (contains? squeeze-ingredients ingredient))

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

(defn add-eggs [n]
  (dotimes [e n]
    (add-egg))
  :ok)

(defn add-flour-cups [n]
  (dotimes [e n]
    (add-flour))
  :ok)

(defn add-milk-cups [n]
  (dotimes [e n]
    (add-milk))
  :ok)

(defn add-sugar-cups [n]
  (dotimes [e n]
    (add-sugar))
  :ok)

(defn add-butters [n]
  (dotimes [e n]
    (add-butter))
  :ok)

(defn add-squeezed
  ([ingredient amount]
   (if (squeezed? ingredient)
     (do
       (dotimes [i amount]
         (grab ingredient)
         (squeeze)
         (add-to-bowl))
       :ok)
     (error "This function only works on squeezed ingredients. You asked me to squeeze" ingredient)))
  ([ingredient]
   (add-squeezed ingredient 1)))

(defn add-scooped
  ([ingredient amount]
   (if (scooped? ingredient)
     (do
       (dotimes [i amount]
         (grab :cup)
         (scoop ingredient)
         (add-to-bowl)
         (release))
       :ok)
     (error "This function only works on scooped ingredients. You asked me to scoop" ingredient)))
  ([ingredient]
   (add-scooped ingredient 1)))

(defn add-simple
  ([ingredient amount]
   (if (simple? ingredient)
     (do
       (dotimes [i amount]
         (grab ingredient)
         (add-to-bowl))
       :ok)
     (println "This function only works on simple ingredients. You asked me to add" ingredient)))
  ([ingredient]
   (add-simple ingredient 1)))

(defn add
  ([ingredient]
   (add ingredient 1))
  ([ingredient amount]
   (cond
     (squeezed? ingredient)
     (add-squeezed ingredient amount)
     (scooped? ingredient)
     (add-scooped ingredient amount)
     (simple? ingredient)
     (add-simple ingredient amount)
     :else (error "I do not know the ingredient" ingredient))))

(defn bake-cake []
  (add :egg 2)
  (add :flour 2)
  (add :milk 1)
  (add :sugar 1)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (add :egg 1)
  (add :flour 1)
  (add :sugar 1)
  (add :butter 1)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(def pantry-ingredients #{:flour :sugar})

(def fridge-ingredients #{:milk :egg :butter})

(defn from-pantry? [ingredient]
  (contains? pantry-ingredients ingredient))

(defn from-fridge? [ingredient]
  (contains? fridge-ingredients ingredient))

(defn fetch-from-pantry
  ([ingredient]
   (fetch-from-pantry ingredient 1))
  ([ingredient amount]
   (if (from-pantry? ingredient)
     (do
       (go-to :pantry)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "This function only works on ingredients that are stored in the pantry. You asked me to fetch" ingredient))))

(defn fetch-from-fridge
  ([ingredient]
   (fetch-from-fridge ingredient 1))
  ([ingredient amount]
   (if (from-fridge? ingredient)
     (do
       (go-to :fridge)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "This function only works on ingredients that are stored in the fridge. You asked me to fetch" ingredient))))

(defn fetch-ingredient
  ([ingredient amount]
   (cond
     (from-fridge? ingredient) (fetch-from-fridge ingredient amount)
     (from-pantry? ingredient) (fetch-from-pantry ingredient amount)))
  ([ingredient]
   (fetch-ingredient ingredient 1)))

(def cake-shopping-list {:flour   2
                         :egg     2
                         :sugar   1
                         :milk    1})

(def cookie-shopping-list {:flour   1
                           :egg     1
                           :sugar   1
                           :butter  1})

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(def locations {:pantry pantry-ingredients
                :fridge fridge-ingredients})

(defn fetch-list [shopping-list]
  (doseq [location (keys locations)]
    (go-to location)
    (doseq [ingredient (get locations location)]
      (load-up-amount ingredient (get shopping-list ingredient 0))))
  (go-to :prep-area)
  (doseq [location (keys locations)]
    (doseq [ingredient (get locations location 0)]
      (unload-amount ingredient (get shopping-list ingredient 0)))))

(defn add-ingredients [shopping-list-1 shopping-list-2]
  (merge-with + shopping-list-1 shopping-list-2))

(defn multiply-ingredients [shopping-list amount]
  (into {}
        (for [kv shopping-list]
          [(first kv) (* amount (second kv))])))

(defn get-shopping-list [key]
  (case key
    :cake   cake-shopping-list
    :cookie cookie-shopping-list
    (error "Can't find shopping list for" key)))

(defn order->ingredients [order]
  (let [items (get order :items)]
    (add-ingredients 
     (multiply-ingredients cake-shopping-list (get items :cake 0)) 
     (multiply-ingredients cookie-shopping-list (get items :cookies 0)))))

(defn orders->ingredients [orders]
  (reduce add-ingredients {}
          (for [order orders]
            (order->ingredients order))))

(defn bake [item]
  (case item
    :cake   (bake-cake)
    :cookie (bake-cookies)
    (error "I don't know how to bake" :item)))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ingredients (orders->ingredients orders)]
    (fetch-list ingredients)
    (doseq [order orders]
      (let [items (get order :items)
            racks (for [kv items
                        i (range (second kv))]
                    (bake (first kv)))]
        (delivery {:orderid (get order :orderid)
                   :address (get order :address)
                   :rackids racks})))))

(defn -main []
  (day-at-the-bakery))
