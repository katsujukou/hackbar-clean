module HackBar.Clean.Web.Views.ToiletPage where

import Prelude

import Data.Lens as L
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import HackBar.Clean.Types (ProgressState, ToiletCleaningProgress, toiletCleaningProgressDefault)
import HackBar.Clean.Web.Hooks.UseToiletCleaningProgress (useToiletCleaningProgress)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM, useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

make :: forall q i o m. MonadAff m => H.Component q i o m 
make = Hooks.component \_ _ -> Hooks.do
  currentProgress /\ currentProgressId <- useState toiletCleaningProgressDefault 
  toiletCleaningProgressAPI <- useToiletCleaningProgress

  useLifecycleEffect do
    progress <- toiletCleaningProgressAPI.fetch
    Hooks.put currentProgressId progress
    pure Nothing

  let 
    updateCurrentProgress :: (ToiletCleaningProgress -> ToiletCleaningProgress) -> Hooks.HookM m Unit
    updateCurrentProgress f = Hooks.modify_ currentProgressId f

    doSave _ = do
      value <- Hooks.get currentProgressId
      toiletCleaningProgressAPI.save value
      pure unit

    ctx =
      { currentProgress
      , progress: toiletCleaningProgressAPI.value
      , updateCurrentProgress
      , doSave
      }

  Hooks.pure $ render ctx

  where
    
  render { currentProgress, updateCurrentProgress, doSave } = do
    HH.div [ HP.class_ $ ClassName "my-3"]
      [ HH.h1 [ HP.class_ $ ClassName "text-lg font-bold text-sky-700" ]
        [ HH.text "🚽トイレそうじ進捗状況" ]
      , HH.div [ HP.class_ $ ClassName "m-3 "]
        [ HH.p [ HP.class_ $ ClassName "text-sm" ]
          [ HH.ul_ 
            [ HH.li_ [ HH.text "・赤い項目は先週から持ち越しのため、優先して対応してください" ] 
            , HH.li_ [ HH.text "・済になっていても、明らかに汚れている箇所は掃除するか、時間がなければ未に戻してください" ]
            ]
          ]
        ]
      , HH.div [ HP.class_ $ ClassName "mx-2"]
        [ HH.div [ HP.class_ $ ClassName "mb-3"]
          [ HH.h2 [ HP.class_ $ ClassName "font-bold"] 
            [ HH.text "●洗面台"]
          , HH.table [ HP.class_ $ ClassName "border-collapse border border-slate-400 w-[100%]" ]
            [ HH.tbody_ 
              [ renderTableLine { label: "鏡"
                                , state: currentProgress.senmendai.mirrors
                                , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"senmendai") <<< prop (Proxy@"mirrors"))
                                }
              , renderTableLine { label: "シンク"
                                , state: currentProgress.senmendai.sink
                                , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"senmendai") <<< prop (Proxy@"sink"))
                                }
              , renderTableLine { label: "洗面台の扉"
                                , state: currentProgress.senmendai.door
                                , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"senmendai") <<< prop (Proxy@"door"))
                                }
              ]
            ]
          ]
        , HH.div [ HP.class_ $ ClassName "mb-3" ]
          [ HH.h2 [ HP.class_ $ ClassName "font-bold"] 
            [ HH.text "●便器" ]
          , HH.table [ HP.class_ $ ClassName "border-collapse border border-slate-400 w-[100%]" ]
              [ HH.tbody_ 
                [ renderTableLine { label: "タンク"
                                  , state: currentProgress.benki.tank
                                  , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"benki") <<< prop (Proxy@"tank"))
                                  }
                , renderTableLine { label: "蓋"
                                  , state: currentProgress.benki.futa
                                  , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"benki") <<< prop (Proxy@"futa"))
                                  }
                , renderTableLine { label: "便座"
                                  , state: currentProgress.benki.benza
                                  , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"benki") <<< prop (Proxy@"benza"))
                                  }
                , renderTableLine { label: "ノズルカバー"
                                  , state: currentProgress.benki.nozzleCover
                                  , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"benki") <<< prop (Proxy@"nozzleCover"))
                                  }
                , renderTableLine { label: "ノズル"
                                  , state: currentProgress.benki.nozzle
                                  , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"benki") <<< prop (Proxy@"nozzle"))
                                  }
                , renderTableLine { label: "便器内"
                                  , state: currentProgress.benki.benso
                                  , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"benki") <<< prop (Proxy@"benso"))
                                  }
                , renderTableLine { label: "足"
                                  , state: currentProgress.benki.foot
                                  , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"benki") <<< prop (Proxy@"foot"))
                                  }
                ]
              ]
          ]
        , HH.div [ HP.class_ $ ClassName "mb-3" ]
          [ HH.h2 [ HP.class_ $ ClassName "font-bold"]  
            [ HH.text "●その他"]
          , HH.table [ HP.class_ $ ClassName "border-collapse border border-slate-400 w-[100%]" ]
              [ HH.tbody_ 
                [ renderTableLine { label: "ゴミ箱"
                                  , state: currentProgress.misc.dustBox { memo = "ゴミ箱の拭き掃除" }
                                  , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"misc") <<< prop (Proxy@"dustBox"))
                                  }
                , renderTableLine { label: "サニタリーボックス"
                                  , state: currentProgress.misc.sanitary { memo = "サニタリーボックス（奥の壁際、トイレの右側にあります）の拭き掃除。" }
                                  , handleClick: updateCurrentProgress <<< L.set (prop (Proxy@"misc") <<< prop (Proxy@"sanitary"))
                                  }
                ]
              ]
          ]
        , HH.div []
          [ HH.button
            [ HP.class_ $ ClassName "bg-blue-500 hover:bg-blue-600 text-white text-sm py-2 px-3 rounded"
            , HE.onClick \_ -> doSave unit
            ]
            [ HH.text "更新"]
          ]
        ]
      ]

  renderTableLine
    :: { label :: String
       , state :: ProgressState
       , handleClick :: ProgressState -> HookM m Unit
       } 
    -> HH.HTML _ _
  renderTableLine { label, state: st, handleClick } = do
    let 
      bgClass = case st.done, st.carryOver of 
        true, _ -> "bg-green-100 "
        _, true -> "bg-red-100 "
        _, _ -> " "

    HH.tr [ HP.class_ $ ClassName ("cursor-pointer hover:bg-slate-100 " <> bgClass) ]
      [ HH.td 
        [ HP.class_ $ ClassName "border border-slate-300 w-[160px] "
        , HE.onClick \_ -> handleClick (st { done = not st.done })
        ]
        [ HH.text label]
      , HH.td
        [ HP.class_ $ ClassName "border border-slate-300 w-[24px] cursor-pointer "
        , HE.onClick \_ -> handleClick (st { done = not st.done })
        ]
        [ HH.text $ if st.done then "✅" else "" ]
      , HH.td [ HP.class_ $ ClassName "border border-slate-300 "] 
        [ HH.text $ st.memo ]
      ]
