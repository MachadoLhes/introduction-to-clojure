(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error [& args]
  (apply println args)
  :error)

(def baking  {:recipes {:cake {:ingredients {:egg   2
                                             :flour 2
                                             :sugar 1
                                             :milk  1}
                               :steps [[:add :all]
                                       [:mix]
                                       [:pour]
                                       [:bake 25]
                                       [:cool]]}
                        :cookies {:ingredients {:egg 1
                                                :flour 1
                                                :butter 1
                                                :sugar 1}
                                  :steps [[:add :all]
                                          [:mix]
                                          [:pour]
                                          [:bake 30]
                                          [:cool]]}
                        :brownies {:ingredients {:egg 2
                                                 :flour 2
                                                 :butter 2
                                                 :cocoa 2
                                                 :sugar 1
                                                 :milk 1}
                                   :steps [[:add :butter]
                                           [:add :cocoa]
                                           [:add :sugar]
                                           [:mix]
                                           [:add :egg]
                                           [:add :flour]
                                           [:add :milk]
                                           [:mix]
                                           [:pour]
                                           [:bake 35]
                                           [:cool]]}}
              :ingredients {:egg {:storage :fridge
                                  :usage :squeezed}
                            :milk {:storage :fridge
                                   :usage :scooped}
                            :flour {:storage :pantry
                                    :usage :scooped}
                            :butter {:storage :fridge
                                     :usage :simple}
                            :sugar {:storage :pantry
                                    :usage :scooped}
                            :cocoa {:storage :pantry
                                    :usage :scooped}}})

(def usage {:squeezed (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (squeeze)
                          (add-to-bowl)))
            :simple (fn [ingredient amount]
                      (dotimes [i amount]
                        (grab ingredient)
                        (add-to-bowl)))
            :scooped (fn [ingredient amount]
                       (grab :cup)
                       (dotimes [i amount]
                         (scoop ingredient)
                         (add-to-bowl))
                       (release))})

(defn usage-type [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (get info :usage)))

(defn add
  ([ingredient]
    (add ingredient 1))
  ([ingredient amount]
    (let [ingredient-type (usage-type ingredient)]
      (if (contains? usage ingredient-type)
        (let [f (get usage ingredient-type)]
          (f ingredient amount))
        (error "I do not know the ingredient" ingredient)))))

(def actions {:cool (fn [ingredients step]
                      (cool-pan))
              :mix  (fn [ingredients step]
                      (mix))
              :pour (fn [ingredients step]
                      (pour-into-pan))
              :bake (fn [ingredients step]
                      (bake-pan (second step)))
              :add  (fn [ingredients step]
                      (cond
                        (and (= 2 (count step))
                             (= :all (second step)))
                          (doseq [kv ingredients]
                            (add (first kv) (second kv)))
                        (and (= 2 (count step))
                             (contains? ingredients (second step)))
                        (add (second step) (get ingredients (second step)))
                        (= 3 (count step))
                        (add (second step) (get step 2))
                        :else
                        (error "I don't know how to add" (second step) (get step 2))))})

(defn perform [ingredients step]
  (let [f (get actions (first step) (fn [ingredients step]
                                      (println "I do not know how to" (first step))))]
    (f ingredients step)))

(defn bake-recipe [recipe]
  (last
    (for [step (get recipe :steps)]
      (perform (get recipe :ingredients) step))))

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn fetch-ingredient
  ([ingredient]
    (fetch-ingredient ingredient 1))
  ([ingredient amount]
    (let [ingredients (get baking :ingredients)
          info (get ingredients ingredient)]
      (if (contains? ingredients ingredient)
        (do
          (go-to (get info :storage))
          (load-up-amount ingredient amount)
          (go-to :prep-area)
          (unload-amount ingredient amount))
        (error "I don't know the ingredient" ingredient)))))

(defn storage-location [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (get info :storage)))

(defn fetch-list [shopping]
  (let [by-location (group-by (fn [item-amount]
                                (storage-location (first item-amount)))
                              shopping)]
    (doseq [loc by-location]
      (go-to (first loc))
      (doseq [item-amount (second loc)]
        (load-up-amount (first item-amount) (second item-amount)))))

  (go-to :prep-area)
  (doseq [item-amount shopping]
    (unload-amount (first item-amount) (second item-amount))))

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [n ingredients]
  (into {}
    (for [kv ingredients]
      [(first kv) (* n (second kv))])))

(defn order->ingredients [order]
  (let [recipes (get baking :recipes)
        items (get order :items)]
    (reduce add-ingredients {}
            (for [kv items]
              (let [recipe (get recipes (first kv))
                    ingredients (get recipe :ingredients)]
                (multiply-ingredients (second kv) ingredients))))))

(defn orders->ingredients [orders]
  (reduce add-ingredients {}
    (for [order orders]
      (order->ingredients order))))

(defn bake [item]
  (let [recipes (get baking :recipes)]
    (bake-recipe (get recipes item))))

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

(defn add-egg []
  (grab :egg)
  (squeeze)
  (add-to-bowl))

(defn scooped? [ingredient]
  (cond
    (= ingredient :milk)
    true
    (= ingredient :flour)
    true
    (= ingredient :sugar)
    true
    :else
    false))

(defn squeezed? [ingredient]
  (= ingredient :egg))

(defn simple? [ingredient] 
  (= ingredient :butter))

(defn add-scooped [ingredient]
  (if (scooped? ingredient)
    (do
      (grab :cup)
      (scoop ingredient)
      (add-to-bowl)
      (release))
    (do
      (println ingredient "is not scoopable")
      :error)))

(defn add-squeezed [ingredient]
  (if (squeezed? ingredient)
    (do
      (grab ingredient)
      (squeeze)
      (add-to-bowl))
    (do
      (println ingredient "is not squeezable")
      :error)))

(defn add-simple [ingredient]
  (if (simple? ingredient)
    (do
      (grab ingredient)
      (add-to-bowl))
    (do
      (println ingredient "is not simple")
      :error)))

(defn add 
  ([ingredient]
   (cond
     (simple? ingredient)   (add-simple ingredient)
     (scooped? ingredient)  (add-scooped ingredient)
     (squeezed? ingredient) (add-squeezed ingredient)
     :else                  (do
                              (println "I don't know the ingredient" ingredient)
                              :error)))
  ([num ingredient]
   (dotimes [i num]
     (add ingredient))
   :ok))

(defn bake-cake []
  (add 2 :flour)
  (add 2 :egg)
  (add :milk)
  (add :sugar)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (add :egg)
  (add :flour)
  (add :sugar)
  (add :butter)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(defn -main []
  (day-at-the-bakery))
