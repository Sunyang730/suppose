(ns muck.version_control)

(def a (atom 0))

(def branch-count (atom 0))


(defn up [thing] (swap! thing (fn [n] (inc n))))

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
  (keyword (str "hash" (up a))))


(defn gen-branch []
  (keyword (str "branch" (up branch-count))))

(gen-branch)

;;For now only shape is a list of points [[x y] [x y] .....]
;;need list of branches as well {[banchName hash]}

(defn create-commit [new-step parent-commit]
   {
   :location (gen-hash)
   :parent (:location parent-commit)
   :child nil
   :branches []
   :state (conj (:state parent-commit) new-step)
  })




(defn add-and-update-parent [update-parent]
  (fn [history new-commit parentHash]
    (let [updated-history (assoc history (:location new-commit) new-commit)]
        (update-in updated-history [parentHash]
                (fn [parent-map] (update-parent parent-map parentHash history new-commit))))))

(def add-commit
  (add-and-update-parent (fn [parent-map _ _ new-commit]
                           (assoc parent-map :child (:location new-commit)))))

(def add-branch
  (add-and-update-parent (fn [parent-map _ _ new-commit]
                           (merge-with conj parent-map {:branches new-commit}))))


(defn new-branch [{:keys [history active-branch branches active-commit] :as app-state}]
  (let [cloned-commit (create-commit [] (active-commit history))
        branch-name (gen-branch)]
    (merge app-state {:history (add-branch history cloned-commit active-branch)
                      :branches (assoc branches branch-name (:location cloned-commit))
                      :active-commit (:location cloned-commit)
                      :active-branch branch-name})))

;;Update the history
;;Update the branches
;;(defn update-branches [{:keys [history branches active-branch] :as app-state} new-commit parentHash]
;;  ())
;;Program needs
;;Instant branch lookup. (with picture to show you branches)
;;Branch Tree. (walk up the tree of branches.)
;;History of current branch.
;;
