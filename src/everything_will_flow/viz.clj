(ns everything-will-flow.viz
  (:require
    [clojure.java.io :as io])
  (:import
    [java.awt
     Toolkit
     Dimension
     Color]
    [java.awt.event
     KeyEvent]
    [java.awt.image
     BufferedImage]
    [javax.imageio
     ImageIO]
    [javax.swing
     AbstractAction JComponent JFrame JLabel JScrollPane ImageIcon KeyStroke]
    [javax.script
     ScriptEngineManager]))

(def ^:private shortcut-mask
  (.. Toolkit getDefaultToolkit getMenuShortcutKeyMask))

(def ^:private close-key
  (KeyStroke/getKeyStroke KeyEvent/VK_W (int shortcut-mask)))

(defn create-frame
  "Creates a frame for viewing graphviz images.  Only useful if you don't want to use the default frame."
  [name]
  (delay
    (let [frame (JFrame. ^String name)
          image-icon (ImageIcon.)
          pane (-> image-icon JLabel. JScrollPane.)]
      (doto pane
        (.. (getInputMap JComponent/WHEN_IN_FOCUSED_WINDOW)
          (put close-key "closeWindow"))
        (.. getActionMap (put "closeWindow"
                           (proxy [AbstractAction] []
                             (actionPerformed [e]
                               (.setVisible frame false))))))
      (doto frame
        (.setContentPane pane)
        (.setSize 1024 768)
        (.setDefaultCloseOperation javax.swing.WindowConstants/HIDE_ON_CLOSE))
      [frame image-icon pane])))

(defn- send-to-front
  "Makes absolutely, completely sure that the frame is moved to the front."
  [^JFrame frame]
  (doto frame
    (.setExtendedState JFrame/NORMAL)
    (.setAlwaysOnTop true)
    .repaint
    .toFront
    .requestFocus
    (.setAlwaysOnTop false))

  ;; may I one day be forgiven
  (when-let [applescript (.getEngineByName (ScriptEngineManager.) "AppleScript")]
    (try
      (.eval applescript "tell me to activate")
      (catch Throwable e
        ))))

(def default-frame (create-frame "flow"))

(defn view-image
  "Takes an `image`, and displays it in a window.  If `frame` is not specified, then the default frame will be used."
  ([image]
     (view-image default-frame image))
  ([frame image]
     (let [[^JFrame frame ^ImageIcon image-icon ^JLabel pane] @frame]
       (.setImage image-icon image)
       (.setVisible frame true)
       (java.awt.EventQueue/invokeLater
         #(send-to-front frame)))))

(defn save-image
  [filename image]
  (ImageIO/write image "png" (io/file filename)))

(defn ->hsv [t]
  (let [t (-> t (max 0) (min 1))]
    (Color/getHSBColor
      (- 0.75 (* 0.7 t))
      1
      (max 0.4
        (cond
          #_(zero? t)
          #_0.0

          (< t 0.05)
          (/ t 0.05)

          :else
          1)))))

(defn create-image [w h f]
  (let [image (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (dotimes [x w]
      (dotimes [y h]
        (.setRGB image x y (.getRGB ^Color (f x (- h y))))))
    image))

;;;

(defn rate-seq [resolution timestamps]
  (let [bucket->timestamps (group-by #(int (/ % resolution)) timestamps)]
    (->> (->> bucket->timestamps keys (apply max) inc range)
      (mapv #(vector % (-> % bucket->timestamps count))))))

(defn distribution-seq [resolution values]
  (let [bucket->values (group-by #(int (/ (first %) resolution)) values)]
    (->> (->> bucket->values keys (apply max) inc range)
      (mapv #(vector % (->> % bucket->values (map second)))))))

;;;

(defn normalize-distribution [y-res values]
  (let [cnt (count values)]
    (->> (rate-seq y-res values)
      (mapv #(/ (second %) cnt)))))

(defn log10 [values]
  (map (fn [[k v]] [k (Math/log10 (+ v 1))]) values))

(defn log [values]
  (map (fn [[k v]] [k (Math/log (+ v 1))]) values))

(defn spectrogram [x-res y-res magnify height values]
  (let [slices (->> (distribution-seq x-res values)
                 (map second)
                 (mapv (partial normalize-distribution y-res)))
        w (count slices)
        h (max height (->> slices (map count) (apply max)))
        ;;max (->> slices (apply concat) (apply max))
        ]
    (create-image (* magnify w) (* magnify h)
      (fn [x y]
        (let [y (int (/ y magnify))
              x (int (/ x magnify))]
          (->hsv
            (/ (-> slices (nth x) (nth y 0))
              (apply max (nth slices x)))))))))
