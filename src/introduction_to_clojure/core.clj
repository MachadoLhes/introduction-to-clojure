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
                        :cookies {:ingredients {:egg   1
                                                :flour 1
                                                :sugar 1
                                                :butter  1}
                                  :steps [[:add :all]
                                          [:mix]
                                          [:pour]
                                          [:bake 30]
                                          [:cool]]}
                        :brownies {:ingredients {:egg    2
                                                 :flour  2
                                                 :sugar  1
                                                 :milk   1
                                                 :cocoa  2
                                                 :butter 2}
                                   :steps [[:add :butter]
                                           [:add :sugar]
                                           [:add :cocoa]
                                           [:mix]
                                           [:add :flour]
                                           [:add :egg]
                                           [:add :milk]
                                           [:mix]
                                           [:pour]
                                           [:bake 35]
                                           [:cool]]}}
              :ingredients {:egg {:storage :fridge
                                  :usage   :squeezed}
                            :milk {:storage :fridge
                                   :usage   :scooped}
                            :flour {:storage :pantry
                                    :usage   :scooped}
                            :butter {:storage :fridge
                                     :usage   :simple}
                            :sugar {:storage :pantry
                                    :usage   :scooped}
                            :cocoa {:storage :pantry
                                    :usage   :scooped}}})

(defn scooped? [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (= :scooped (get info :usage))))

(defn squeezed? [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (= :squeezed (get info :usage))))

(defn simple? [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (= :simple (get info :usage))))

(def usage {:simple (fn [ingredient amount]
                      (dotimes [i amount]
                        (grab ingredient)
                        (add-to-bowl)))
            :squeezed (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (squeeze)
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
                                      (error "I do not know how to" (first step))))]
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
       (error "I don't know the ingredient" :ingredient)))))

(defn sotrage-location [item]
  (let [ingredients (get baking :ingredients)
        info (get ingredients item)]
    (get info :storage)))

(defn fetch-list [shopping-list]
  (let [locations (group-by (fn [item-amount]
                              (sotrage-location (first item-amount))) shopping-list)]
    (doseq [location (keys locations)]
      (go-to location)
      (doseq [item-amount (get locations location)]
        (load-up-amount (first item-amount) (second item-amount))))
    (go-to :prep-area)
    (doseq [location (keys locations)]
      (doseq [item-amount (get locations location)]
        (unload-amount (first item-amount) (second item-amount))))))

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [n ingredients]
  (into {}
        (for [kv ingredients]
          [(first kv) (* n (second kv))])))

(defn order->ingredients [order]
  (let [items (get order :items)
        recipes (get baking :recipes)]
    (reduce add-ingredients {}
            (for [kv items]
              (let [recipe (get recipes (first kv))
                    ingredients (get recipes :ingredients)]
                (multiply-ingredients (second kv) ingredients))))

    (add-ingredients
     (multiply-ingredients (get items :brownies 0) {:butter 2
                                                    :sugar 1
                                                    :cocoa 2
                                                    :flour 2
                                                    :egg 2
                                                    :milk 1})
     (add-ingredients
      (multiply-ingredients (get items :cake 0) {:egg 2
                                                 :flour 2
                                                 :sugar 1
                                                 :milk 1})
      (multiply-ingredients (get items :cookie 0) {:egg 1
                                                   :flour 1
                                                   :butter 1
                                                   :sugar 1})))))

(defn orders->ingredients [orders]
  (reduce add-ingredients {}
          (for [order orders]
            (order->ingredients order))))

(defn bake [item]
  (let [recipes (get baking :recipes)]
    (if (contains? recipes item)
      (bake-recipe (get recipes item))
      (error "I don't know how to bake" item))))

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

(bake :brownies)

(defn -main []
  (day-at-the-bakery))
