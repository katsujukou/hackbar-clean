module HackBar.Clean.Types where


type ProgressState = 
  { done :: Boolean 
  , memo :: String
  , carryOver :: Boolean
  }

-- | トイレの清掃状況
type ToiletCleaningProgress = 
  { senmendai ::
    { mirrors :: ProgressState
    , sink :: ProgressState
    , door :: ProgressState
    }
  , benki ::
    { tank :: ProgressState
    , futa :: ProgressState
    , benza :: ProgressState
    , nozzleCover :: ProgressState
    , nozzle :: ProgressState
    , benso :: ProgressState
    , foot :: ProgressState
    }
  , misc ::
    { dustBox :: ProgressState
    , sanitary :: ProgressState
    }
  }

toiletCleaningProgressDefault :: ToiletCleaningProgress
toiletCleaningProgressDefault = 
  { senmendai:
    { mirrors: { done: false, memo: "", carryOver: false }
    , sink: { done: false, memo: "", carryOver: false }
    , door: { done: false, memo: "", carryOver: false }
    }
  , benki:
    { tank: { done: false, memo: "", carryOver: false }
    , futa: { done: false, memo: "", carryOver: false }
    , benza: { done: false, memo: "", carryOver: false }
    , nozzleCover: { done: false, memo: "", carryOver: false }
    , nozzle: { done: false, memo: "", carryOver: false }
    , benso: { done: false, memo: "", carryOver: false }
    , foot: { done: false, memo: "", carryOver: false }
    }
  , misc:
    { dustBox: { done: false, memo: "ゴミ箱の拭き掃除", carryOver: false }
    , sanitary: { done: false, memo: "", carryOver: false }
    }
  }
