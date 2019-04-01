// implementation of "success story algorithm" from Schmidhuber

module Ssa

type TimeAndReward = {time: float; reward: float}

// compute ratio as described in Schmidhuber's paper
let ssaCalcRatio (checkpointTimeAndReward:TimeAndReward) (currentTimeAndReward:TimeAndReward): float =
  (currentTimeAndReward.reward - checkpointTimeAndReward.reward) / (currentTimeAndReward.time - checkpointTimeAndReward.time);

type SsaCheckpoint<'a> = {
  payload: 'a;
  timeAndReward: TimeAndReward; // time and reward of the checkpoint
}

let ssaIsCriterionValidForPair<'a> (a: SsaCheckpoint<'a> ) (b: SsaCheckpoint<'a>) (currentTimeAndReward:TimeAndReward) =
  let ratioForB = ssaCalcRatio b.timeAndReward currentTimeAndReward
  let ratioForA = ssaCalcRatio a.timeAndReward currentTimeAndReward
  
  ratioForB > ratioForA

// TODO< implement basic algorithm with the decision making >
