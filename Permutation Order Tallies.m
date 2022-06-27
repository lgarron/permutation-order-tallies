(* ::Package:: *)

(* ::Title:: *)
(*Permutation Puzzle Position Order Tallies*)


(* ::Subsection:: *)
(*Initialization*)


(* ::Input::Initialization:: *)
CyclePossibilities[n_,o_]:=CyclePossibilities[n,o]=Flatten[Table[{i,twist},{i,n},{twist,0,o-1}],1]


(* ::Input::Initialization:: *)
CyclePossibilitiesUpToLast[n_,o_,Null]:=CyclePossibilities[n,o]
CyclePossibilitiesUpToLast[n_,o_,last_]:=CyclePossibilitiesUpToLast[n,o,last]=If[
n<last[[1]],
CyclePossibilities[n,o],
Take[CyclePossibilities[n,o],Position[CyclePossibilities[n,o],last][[1,1]]]
]


(* ::Input::Initialization:: *)
CyclePatterns[0,_,_]={{}};
CyclePatterns[n_,o_,last_:Null]:=CyclePatterns[n,o,last]=Flatten[Table[Table[Catenate[{{first},rest}],{rest,CyclePatterns[n-first[[1]],o,first]}],{first,CyclePossibilitiesUpToLast[n,o,last]}],1]


(* ::Input::Initialization:: *)
IsOrientationParityValid[o_][c_]:=Mod[Total[c[[All,2]]],o]==0


(* ::Input::Initialization:: *)
PermutationParity[c_]:=Mod[Total[c[[All,1]]-1],2]
HasValidOrientationParityAndGivenPermParity[o_,p_][c_]:=IsOrientationParityValid[o][c]&&PermutationParity[c]==p


(* ::Input::Initialization:: *)
CyclesWithValidOrientationParity[n_,o_]:=Select[CyclePatterns[n,o],IsOrientationParityValid[o]]
CyclesWithValidOrientationParityAndGivenPermParity[n_,o_,Null]:=CyclesWithValidOrientationParity[n,o]
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
CycleTalliesWithPermParity[n_,o_,p_]:=CycleTalliesWithPermParity[n,o,p]=GatherTallySorted[{CyclePatternOrder[n,o][#],NumPositions[n,o][#]}&/@CyclesWithValidOrientationParityAndGivenPermParity[n,o,p]]


(* ::Input::Initialization:: *)
FoldOrbitTalliesWithCartesianExplosion[l_]:=GatherTallySorted[{LCM@@#[[All,1]],Times@@#[[All,2]]}&/@Tuples[l]]
CombineOrbitTallies[l_]:=Fold[FoldOrbitTalliesWithCartesianExplosion[{#1,#2}]&,CycleTalliesWithPermParity@@#&/@l]
CombineOrbitMultiTallies[l_]:=GatherTallySorted@Catenate[CombineOrbitTallies/@l]


(* ::Input::Initialization:: *)
ParityCorrelatedOrbits[l_]:=Table[
Flatten/@Transpose[{l,parities}],
{parities,Append[#,Mod[Total[#],2]]&/@Tuples[{0,1},Length[l]-1]}
]


(* ::Input::Initialization:: *)
CombineOrbitGroups[l_]:=Catenate/@Tuples[l]


(* ::Subtitle:: *)
(*Puzzles*)


(* ::Subsection:: *)
(*3x3x3*)


(* ::Input:: *)
(*tally3x3x3=CombineOrbitMultiTallies[*)
(*ParityCorrelatedOrbits[{{12,2},{8,3}}] (* edges, corners *)*)
(*]*)


(* ::Input:: *)
(*tally3x3x3[[All,2]]//Total*)


(* ::Input:: *)
(*Export[FileNameJoin[{NotebookDirectory[],"permutation-order-tally-3x3x3.csv"}],tally3x3x3]*)


(* ::Subsection:: *)
(*Megaminx*)


(* ::Input:: *)
(*tallyMegaminx=CombineOrbitTallies[{{20,3,0}, {30,2,0}}];*)


(* ::Input:: *)
(*tallyMegaminx[[All,2]]//Total*)


(* ::Input:: *)
(*Export[FileNameJoin[{NotebookDirectory[],"permutation-order-tally-megaminx.csv"}],tallyMegaminx]*)


(* ::Subsection:: *)
(*4x4x4 Supercube*)


(* ::Input:: *)
(*tallySuper4x4x4=CombineOrbitMultiTallies[CombineOrbitGroups[{*)
(*{{{24,1,Null}}}, (* wings *)*)
(*ParityGroups[{{24,1},{8,3}}] (* centers, corners *)*)
(*}]];*)


(* ::Input:: *)
(*tallySuper4x4x4[[All,2]]//Total*)
(*24!*8!/2*3^7*24!*)


(* ::Input:: *)
(*Export[FileNameJoin[{NotebookDirectory[],"permutation-order-tally-super-4x4x4.csv"}],tallySuper4x4x4]*)


(* ::Subsection:: *)
(*5x5x5 Supercube*)


(* ::Input:: *)
(*tallySuper5x5x5=CombineOrbitMultiTallies[CombineOrbitGroups[{*)
(*{{{24,1,Null}}}, (* wings *)*)
(*ParityCorrelatedOrbits[{{12,2},{24,1},{24,1},{8,3}}] (* midges, X-centers, T-centers, corners *)*)
(*}]];*)


(* ::Input:: *)
(*tallySuper5x5x5[[All,2]]//Total*)
(*24!*12!*2^11*24!*24!*8!*3^7/2*)


(* ::Input:: *)
(*Export[FileNameJoin[{NotebookDirectory[],"permutation-order-tally-super-5x5x5-stationary-ignored-centers.csv"}],tallySuper5x5x5]*)
