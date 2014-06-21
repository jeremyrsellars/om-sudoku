
(ns om-sudoku.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [om-sudoku.sudoku :as sudoku]
            [clojure.string :as string]))

(enable-console-print!)

(def app-state
  (atom
   (let [board-rows sudoku/sample-board]
    {:board {:rows board-rows
             :dots true
             :all-values sudoku/all-values
             :value-count (count sudoku/all-values)
             :block-height sudoku/block-rows
             :block-width sudoku/block-cols
             :current-coord nil
             :won (sudoku/valid-solution? board-rows)}})))

(def get-board :rows)

(defn display [show]
  (if show
    #js {}
    #js {:display "none"}))


(defn border-left? [[row col] [block-height block-width]]
  (zero? (mod col block-width)))

(defn border-right? [[row col] [block-height block-width]]
  (zero? (mod (- col (dec block-width)) block-width)))

(defn border-top? [[row col] [block-height block-width]]
  (zero? (mod row block-height)))

(defn border-bottom? [[row col] [block-height block-width]]
  (zero? (mod (- row (dec block-height)) block-height)))


(defn cell-style [{:keys [coord block-height block-width background-color]}]
  (letfn [(pick-border [border?]
            (if (border? coord [block-height block-width])
              #js ["solid 1px black"]
              #js ["solid 1px #CCCCCC"]))]
  #js {:background-color background-color
       :border-left (pick-border border-left?)
       :border-right (pick-border border-right?)
       :border-top (pick-border border-top?)
       :border-bottom (pick-border border-bottom?)
       :padding "0px"
       :margin "0px"}))


(defn handle-change [e board coord]
  (let [s (.. e -target -value)
        new-value (if (empty? s) 0 (js/parseInt s))]
    (om/transact!
      board
      :rows
      (fn [_]
        (sudoku/set-value-at (get-board @board) coord new-value)))
    (om/transact!
      board
      :won
      (fn [_]
        (sudoku/valid-solution? (get-board @board))))))

(defn handle-coord-change [e board coord]
  (let [s (.. e -target -value)
        new-value (if (empty? s) 0 (js/parseInt s))]
    (om/transact!
      board
      :current-coord
      (fn [_] coord))))

(defn commit-change [text owner]
  (om/set-state! owner :editing true))

(defn sudoku-cell [board owner {:keys [coord] :as opts}]
  (reify
    om/IDisplayName
    (display-name [this]
       (str "cell @ row:" [coord 0] " col:" [coord 1]))
    om/IInitState
    (init-state [_]
      {:editing true})
    om/IRenderState
    (render-state [_ {:keys [editing]}]
     (dom/td
      #js {:style (cell-style
                         {:coord coord
                          :block-height (:block-height board)
                          :block-width  (:block-width board)})}
      (let [b           (get-board board)
            is-editing  (= coord (:current-coord board))
            has-value   (sudoku/has-value? b coord)
            text        (when has-value (sudoku/value-at b coord))
            values-here (sudoku/valid-values-for b coord)
            placeholder (->> values-here
                          (interpose ",")
                          (apply str))
            input
              (dom/input
                #js {:style #js {:background-color
                               (if (= 1 (count placeholder)) "#f0ed73" "white")}
                     :type "text"
                     :value text
                     :id (when is-editing "current-cell")
                     :placeholder placeholder
                     :onChange #(handle-change % board coord)
                     :onBlur (fn [e]
                               (handle-coord-change e board nil))
                     :onFocus (fn [e]
                               (handle-coord-change e board coord))
                     })]
        (om/set-state! owner :editing is-editing)
        (if (or is-editing has-value (not (:dots board))) input
          (apply dom/svg
              #js {:width 40
                   :height 40
                   :focusable true
                   :style #js {:background-color (if (= 1 (count placeholder)) "#f0ed73" "white")}
                   :onBlur (fn [e]
                              (handle-coord-change e board nil))
                   :onFocus (fn [e]
                              (handle-coord-change e board coord))
                   :onClick (fn [e]
                              (handle-coord-change e board coord))}
              (map #(dom/text #js {:x (* 10 (inc (Math.floor (/ (dec %) 3))))
                                   :y (* 12 (inc (mod (dec %) 3)))
                                   :style #js {:font-size "10"}
                                   :stroke "green" :fill "yellow"}
                              %)
                 values-here))

           ))))))

(defn board-view [board owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (js/setInterval
       #(let [cell (. js/document (getElementById "current-cell"))]
          (when cell (-> cell .focus)))
         0))
    om/IRender
    (render [_]
      (apply dom/table #js {:style #js {:border-spacing "0px" :border "solid 1px black"}}
        (for [row-index (range (:value-count board))]
          (apply dom/tr nil
            (for [col-index (range (:value-count board))]
              (om/build sudoku-cell board {:opts {:coord [row-index col-index]}}))))))))

(defn board-options-view [board owner]
  (reify
    om/IRender
    (render [_]
      (dom/input
         #js {:type "checkbox"
              :checked (:dots board)
              :onChange
                #(om/transact!
                  board
                  :dots (fn [_] (-> (om/get-node owner) .-checked)))}
         "Show Dots"))))

(defn game-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        (dom/h1 nil (if (:won (:board app)) "Winner!" "Playing"))
        (om/build board-view (:board app))
        (om/build board-options-view (:board app))))))

(true? (border-left? [0 0] [3 3]))
(false? (border-left? [5 1] [3 3]))
(false? (border-left? [8 2] [3 3]))
(true? (border-left? [8 3] [3 3]))
(true? (border-left? [8 6] [3 3]))
(false? (border-left? [8 8] [3 3]))


(false? (border-right? [0 0] [3 3]))
(false? (border-right? [5 1] [3 3]))
(true? (border-right? [8 2] [3 3]))
(false? (border-right? [8 3] [3 3]))
(false? (border-right? [8 6] [3 3]))
(true? (border-right? [8 8] [3 3]))

(true? (border-top? [0 0] [3 3]))
(false? (border-top? [1 5] [3 3]))
(false? (border-top? [2 8] [3 3]))
(true? (border-top? [3 8] [3 3]))
(true? (border-top? [6 8] [3 3]))
(false? (border-top? [8 8] [3 3]))


(false? (border-bottom? [0 0] [3 3]))
(false? (border-bottom? [1 5] [3 3]))
(true? (border-bottom? [2 8] [3 3]))
(false? (border-bottom? [3 8] [3 3]))
(false? (border-bottom? [6 8] [3 3]))
(true? (border-bottom? [8 8] [3 3]))

(#(hash-map :x (mod (dec %) 3)
    :y (Math.floor (/ (dec %) 3)))
    2)

(om/root game-view app-state
  {:target (. js/document (getElementById "board"))})
