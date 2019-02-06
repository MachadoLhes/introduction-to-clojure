(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error [& args]
  (apply println args)
  :error)

(def baking {:recipes {:cake {:ingredients {:egg   2
                                             :flour 2
                                             :sugar 1
                                             :milk  1}
                               :steps [[:add :all]
                                       [:mix]
                                       [:pour]
                                       [:bake 25]
                                       [:cool]]}}})

(def scoop-ingredients #{:milk :flour :sugar :cocoa})

(def squeeze-ingredients #{:egg})

(def simple-ingredients #{:butter})

(defn scooped? [ingredient]
  (contains? scoop-ingredients ingredient))

(defn squeezed? [ingredient]
  (contains? squeeze-ingredients ingredient))

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

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

(defn bake-brownie []
  (add :butter 2)
  (add :sugar 1)
  (add :cocoa 2)
  (mix)
  (add :egg 2)
  (add :flour 2)
  (add :milk 1)
  (mix)
  (pour-into-pan)
  (bake-pan 35)
  (cool-pan))

(def pantry-ingredients #{:flour :sugar :cocoa})

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

(def brownie-shopping-list {:flour  2
                            :egg    2
                            :sugar  1
                            :cocoa  2
                            :milk   1
                            :butter 2})

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
  (cond 
    (= :cake key)   cake-shopping-list
    (= :cookie key) cookie-shopping-list
    (= :brownie key) brownie-shopping-list
    :else (error "Can't find shopping list for" key)))


(defn order->ingredients [order]
  (let [items (get order :items)]
    (add-ingredients 
     (multiply-ingredients cake-shopping-list (get items :cake 0))
     (add-ingredients 
      (multiply-ingredients cookie-shopping-list (get items :cookies 0))
      (multiply-ingredients brownie-shopping-list (get items :brownie 0))))))

(defn orders->ingredients [orders]
  (reduce add-ingredients {}
          (for [order orders]
            (order->ingredients order))))

(defn bake [item]
  (cond 
    (= :cake item)   (bake-cake)
    (= :cookie item) (bake-cookies)
    (= :brownie item) (bake-brownie)
    :else (error "I don't know how to bake" :item)))

(defn perform [ingredients step]
  (cond
    (= :cool (first step)) (cool-pan)
    (= :mix (first step))  (mix)
    (= :pour (first step)) (pour-into-pan)
    (= :bake (first step)) (bake (second step))
    (= :add (first step))  (cond
                             (and (= 2 (count step))
                                  (= :all (second step)))
                             (doseq [kv ingredients]
                               (add (first kv) (second kv)))
                             (and (= 2 (count step)) (contains? ingredients (second step)))
                             (add (second step) (get ingredients (second step)))
                             (= 3 (count step))
                             (add (second step) (get step 2))
                             :else 
                             (error "I don't know how to add" (second step)))
    :else (error "I don't know the step" (first step))))

(defn bake-recipe [recipe]
  (let [ingredients (get recipe :ingredients)]
    (doseq [step (get recipe :steps)]
      (perform (get recipe ingredients) step))))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders-day3)
        ingredients (orders->ingredients orders)]
    (fetch-list ingredients)
    (doseq [order orders]
      (let [items (get order :items)
            racks (for [kv items
                        i (range (second kv))]
                    (bake (first kv)))
            receipt {:orderid (get order :orderid)
                     :address (get order :address)
                     :rackids racks}]
        (delivery receipt)))))

(defn -main []
  (day-at-the-bakery))
