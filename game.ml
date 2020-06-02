open UniverseJs
open World
open Image
open Color

(* world_t : くまの座標, 動くマスクの座標と判定のマスク の組の型 *)
type world_t = (int * int) * (int * int) * (int * int)

(* -----画像関係----- *)

let width       = 400   (* 画面の幅 *)
let height      = 400   (* 画面の高さ *)

let image_width  = 300   (* くまの画像の横幅 *)
let image_height = 400   (* くまの画像の縦幅 *)
let mask_width   = 200   (* マスクの横幅 *)
let mask_height   = 160  (* 判定マスクの縦幅 *)

let background  = read_image "images/background.png"    (* 背景画像 *)
                             (float_of_int width) (float_of_int height)
let bear        = read_image "images/animal_smile_kuma.png" (* くま *)
                             (float_of_int image_width)
                             (float_of_int image_height)
let mask        = read_image "images/medical_mask_syoumen.png" (* 動くマスク *)
                             (float_of_int mask_width)
                             (float_of_int mask_height)
let mask2       = read_image "images/medical_mask_syoumen.png" (* 判定マスク *)
                             (float_of_int mask_width)
                             (float_of_int mask_height)
let maskbear    = read_image "images/mask_animal_kuma.png" (* マスクくま *)
                             (float_of_int image_width)
                             (float_of_int image_height)

(* -----画像関係ここまで----- *)

(* -----初期値関係----- *)

(* worldの初期値 *)
let initial_world =
  ((200, 200),                       (* くまの位置 *)
   (200, 220),                       (* 動くマスクの位置 *)   
   (500, 500)                        (* 判定のマスクの位置 *)   
  )
  
(* -----初期値関係ここまで----- *)

(* -----ゲーム終了の判定関係-----*)

(* くまとマスクの座標をもらう *)
(* マスクがはめられたら(200, 220)ゲームクリアを返す *)
let check ((ax, ay), (bx, by), (cx, cy)) =
  if (width / 2) - 10 <= cx && cx <= (width / 2) + 10
  then true
  else false

(* -----スコアの判定関係ここまで-----*)

(* -----on_tick関係----- *)

(* マスクの y 座標が一番上についていたら一番下にもってくる *)
(* my_mask : int -> int *)
let my_mask y =
  if y <= mask_height - image_height / 2 then height else y

(* マスクの x 座標が一番下についていたら一番上にもってくる *)
(* mx_mask : int -> int *)
let mx_mask x =
  if x <= mask_width - image_width / 2 then width else x

(* 各果物の動く縦幅 *)
let mask_y  = 20

(* マスクを動かす *)
(* move_mask : int * int -> int * int *)
let move_mask (x, y) = (x - mask_y, y)

(* マスクを下に動かす *)
(* 下についたら上にセット *)
(* move_on_tick : world_t -> (world_t, 'a) World.t *)
let move_on_tick ((ax, ay), (bx, by), (cx, cy)) =
  ((ax, ay),
   move_mask (mx_mask bx, my_mask by),
   (cx, cy)
  )

(* -----on_tick関係ここまで----- *)

(* -----描画関係----- *)

(* y座標を表示用に変換 *)
(* change_y : (int * int) -> int *)
let change_y y = height - y

(* 各座標から画像を作成 *)
(* draw : world_t -> Image.t *)
let draw ((ax, ay), (bx, by), (cx, cy)) =
  (place_image mask   (float_of_int bx, float_of_int by)
  (place_image bear   (float_of_int (width / 2), float_of_int (height / 2))
  (empty_scene (float_of_int width) (float_of_int height))))

(* draw_game_finish : world_t -> Image.t *)
let draw_game_finish world =
  place_image (text "Good" ~size:30 black)
              (float_of_int (width / 2), float_of_int (height / 2))
              (place_image maskbear (float_of_int (width / 2), float_of_int (height / 2)) (place_image background (float_of_int (width / 2), float_of_int (height / 2))(draw world)))
              

(* -----描画関係ここまで----- *)

(* -----キー操作関係-----*)

(* key_draw : world_t -> string -> world_t *)
let key_draw ((ax, ay), (bx, by), (cx, cy)) key = 
 (if key = " " then
    (if check ((ax, ay), (bx, by), (bx, by))
     then ((ax, ay), (bx, by), (bx, by))
     else ((ax, ay), (bx, by), (cx, cy)))           (* ここでゲーム終了したい *)
  else
    ((ax, ay), (bx, by), (cx, cy)))

(* -----キー操作関係ここまで-----*)

(* game_finished : world_t -> bool *)
let game_finished ((ax, ay), (bx, by), (cx, cy)) =
  if (width / 2) - 10 <= cx && cx <= (width / 2) + 10
  then true
  else false

(* ゲーム開始 *)
let _ =
  big_bang initial_world
           ~name:"wear_mask"
           ~width:width
           ~height:height
           ~to_draw:draw
           ~on_key_press:key_draw
           ~on_tick:move_on_tick
           ~rate:0.03                    (* ゲームの動く速さ *)
           ~stop_when:game_finished
           ~to_draw_last:draw_game_finish
