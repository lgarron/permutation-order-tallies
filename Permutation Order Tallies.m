(* ::Package:: *)

(* ::Subtitle:: *)
(*Permutation Puzzle Position Order Tallies*)


(* ::Input::Initialization:: *)
Clear[CyclePossibilities]
CyclePossibilities[n_,o_]:=CyclePossibilities[n,o]=Flatten[Table[{i,twist},{i,n},{twist,0,o-1}],1]


(* ::Input::Initialization:: *)
Clear[CyclePossibilitiesUpToLast]
CyclePossibilitiesUpToLast[n_,o_,Null]:=CyclePossibilities[n,o]
CyclePossibilitiesUpToLast[n_,o_,last_]:=CyclePossibilitiesUpToLast[n,o,last]=If[
n<last[[1]],
CyclePossibilities[n,o],
Take[CyclePossibilities[n,o],Position[CyclePossibilities[n,o],last][[1,1]]]
]


(* ::Input::Initialization:: *)
Clear[CyclePatterns]
CyclePatterns[0,_,_]={{}}
CyclePatterns[n_,o_,last_:Null]:=CyclePatterns[n,o,last]=Flatten[Table[Table[Catenate[{{first},rest}],{rest,CyclePatterns[n-first[[1]],o,first]}],{first,CyclePossibilitiesUpToLast[n,o,last]}],1]


(* ::Input::Initialization:: *)
IsOrientationParityValid[o_][c_]:=Mod[Total[c[[All,2]]],o]==0


(* ::Input::Initialization:: *)
PermutationParity[c_]:=Mod[Total[c[[All,1]]-1],2]
HasValidOrientationParityAndGivenPermParity[o_,p_][c_]:=IsOrientationParityValid[o][c]&&PermutationParity[c]==p


(* ::Input::Initialization:: *)
CyclesWithValidOrientationParity[n_,o_]:=Select[CyclePatterns[n,o],IsOrientationParityValid[o]]
CyclesWithValidOrientationParityAndGivenPermParity[n_,o_,p_]:=Select[CyclePatterns[n,o],HasValidOrientationParityAndGivenPermParity[o,p]]


(* ::Input::Initialization:: *)
NumPositions[n_,o_][c_]:=n!*o^n/Product[(tallyEntry[[1,1]]*o)^tallyEntry[[2]]*tallyEntry[[2]]!,{tallyEntry,Tally[c]}]


(* ::Input::Initialization:: *)
CycleOrder[n_,o_][{nc_,0}]:=nc
CycleOrder[n_,o_][{nc_,oc_}]:=nc*LCM[o,oc]/oc
CyclePatternOrder[n_,o_][cyclePattern_]:=CyclePatternOrder[n,o][cyclePattern]=LCM@@(CycleOrder[n,o]/@cyclePattern)


(* ::Input::Initialization:: *)
GatherTallySorted[l_]:=SortBy[{#[[1,1]],#[[All,2]]//Total}&/@GatherBy[l,First],First]


(* ::Input::Initialization:: *)
CycleTalliesWithPermParity[n_,o_,p_]:=GatherTallySorted[{CyclePatternOrder[n,o][#],NumPositions[n,o][#]}&/@CyclesWithValidOrientationParityAndGivenPermParity[n,o,p]]


(* ::Input::Initialization:: *)
CombineOrbitTallies[t1_,t2_]:=GatherTallySorted[{LCM[#[[1,1]],#[[2,1]]],#[[1,2]]*#[[2,2]]}&/@Tuples[{t1,t2}]]


(* ::Input:: *)
(*tally3x3x3=Catenate[{CombineOrbitTallies[CycleTalliesWithPermParity[12,2,0],CycleTalliesWithPermParity[8,3,0]],CombineOrbitTallies[CycleTalliesWithPermParity[12,2,1],CycleTalliesWithPermParity[8,3,1]]}]//GatherTallySorted*)


(* ::Input:: *)
(*Export[FileNameJoin[{NotebookDirectory[],"permutation-order-tally-3x3x3.csv"}],tally3x3x3]*)


(* ::Input:: *)
(*tallyMegaminx=Catenate[{CombineOrbitTallies[CycleTalliesWithPermParity[20,3,0],CycleTalliesWithPermParity[30,2,0]]}]//GatherTallySorted;*)


(* ::Input:: *)
(*tallyMegaminx//Total*)


(* ::Input:: *)
(*Export[FileNameJoin[{NotebookDirectory[],"permutation-order-tally-megaminx.csv"}],tallyMegaminx]*)
