(ns muck.version_control)

(def a (atom 0))

(defn up [] (swap! a (fn [n] (inc n))))

;;  :history {
;;    #hash {
;;      :location #hash
;;      :parent #hash
;;      :child #hash
;;      :branches [[:name #hash][:name #hash][:name #hash]]
;;      :state [whole list of actions.]
;;    }
;;  }
;;  :branches {:branchName most-recent-commit}
;;  :active-branch :branch-name
;;crear
;;Does each commit need to know its branch??
;;Why would it?
;;I don't think so. Where is no 'updating' the past here

(defn gen-hash []
  (keyword (str "hash" (up))))

;;For now only shape is a list of points [[x y] [x y] .....]
;;need list of branches as well {[banchName hash]}

(defn create-commit [new-step parent-commit]
   {
   :location (gen-hash)
   :parent (if (nil? parent-commit) nil (:location parent-commit))
   :child nil
   :branches []
   :state (conj (if (nil? parent-commit) [] (:state parent-commit)) new-step)
  })




(defn add-commit
  ([history new-commit] (assoc history (:location new-commit) new-commit))
  ([history new-commit parentHash]
    (let [updated-history (assoc history (:location new-commit) new-commit)]
      (if (= new-commit :start)
        updated-history
        (update-in updated-history [parentHash]
                 #(assoc % :child (:location new-commit)))))))

;;Program needs
;;Instant branch lookup. (with picture to show you branches)
;;Branch Tree. (walk up the tree of branches.)
;;History of current branch.


;;


