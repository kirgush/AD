(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 14-Dec-2016 *)

BeginPackage["AD`"]
(* Exported symbols added here with SymbolName::usage *) 

traceView2::usage=""
traceView4::usage=""

sumKeyUnion::usage=""
makePartial::usage=""
propagatead::usage=""
ad::usage=""
toValue::usage=""
seqsum::usage="Recursive total to overcome slowness of built-in Total on `ad` objects. 
It sums over the first dimension of arrays.
  Example:

  seqsum[{ad[-23.7517,<|vol->-1.57748,fc->-1.02938|>],
          ad[-23.2275,<|vol->-0.39147,fc->-1.00758|>],
          ad[-22.1973,<|vol->1.86507,fc->-0.964734|>],
          ad[-22.0535,<|vol->2.17208,fc->-0.958753|>],
          ad[-23.1711,<|vol->-0.265439,fc->-1.00523|>],
          ad[-22.0938,<|vol->2.08633,fc->-0.960428|>],
          ad[-23.8149,<|vol->-1.72212,fc->-1.03201|>],
          ad[-23.2646,<|vol->-0.474541,fc->-1.00912|>],
          ad[-23.8558,<|vol->-1.81582,fc->-1.03371|>],
          ad[-23.9182,<|vol->-1.95914,fc->-1.0363|>]}]
";
seqprod::usage="Recursive total to overcome slowness of built-in Product on `ad` objects.";
recursiveStatistics::usage=""

simulationSpot::usage="simulationSpot[fc, termVol, rvs] returns simulations of spot prices;
   fc: forward curve, a list of positive numbers with length nC;
   termVol: terminal volatility, a list of non-negative numbers with length nC;
   rvs: a random sample drawn for a NormalDistribition[0, 1], with dimensions {nS, nC}.
   
   The simulations are simply generated via
   
   Exp[Log[fc]- 1/2 termVol^2 + termVol #] & /@ rvs
   
   and have dimensions {nS, nC}.
   
   ";

vf::usage="Update values based on exercise value and continuation value."

cSimulationSpot::usage=""

sdd::usage="";
stheta::usage="";
sm::usage="";

payoffBermudan::usage=""
payoffBermudanPrime::usage=""

regressionCoefficient::usage="";
regressionCoefficientAndSensitivity::usage="";
regressionCoefficientAndSensitivityBothLoops::usage="";
regressionCoefficientAndSensitivityBothLoopsNew::usage="";
backwardLoopValues::usage="";
backwardLoopSensitivities::usage="";
backwardLoopSensitivitiesNew::usage="";


Begin["`Private`"]
(* Implementation of the package *)

(* http://mathematica.stackexchange.com/q/29339/134 *)
traceView2[expr_] :=
  Module[{steps = {}, stack = {}, pre, post, show, dynamic},
    pre[e_] := (stack = {steps, stack}; steps = {})
  ; post[e_, r_] :=
      ( steps = First@stack ~Join~ {show[e, HoldForm[r], steps]}
      ; stack = stack[[2]]
      )
  ; SetAttributes[post, HoldAllComplete]
  ; show[e_, r_, steps_] :=
      Grid[
        steps /. {
          {} -> {{"Expr  ", Row[{e, " ", Style["inert", {Italic, Small}]}]}}
        , _ -> { {"Expr  ", e}
               , {"Steps", steps /.
                   { {} -> Style["no definitions apply", Italic]
                   , _ :> OpenerView[{Length@steps, dynamic@Column[steps]}]}
                 }
               , {"Result", r}
               }
        }
      , Alignment -> Left
      , Frame -> All
      , Background -> {{LightCyan}, None}
      ]
  ; TraceScan[pre, expr, ___, post]
  ; Deploy @ Pane[steps[[1]] /. dynamic -> Dynamic, ImageSize -> 10000]
  ]
SetAttributes[traceView2, {HoldAllComplete}]

traceView4[expr_] :=
  Module[{steps = {}, stack = {}, pre, post},
    pre[e_] := (stack = {steps, stack}; steps = {})
  ; post[e_, r_] :=
      ( steps = First@stack ~Join~ {{e, steps, HoldForm[r]}}
      ; stack = stack[[2]]
      )
  ; SetAttributes[post, HoldAllComplete]
  ; TraceScan[pre, expr, ___, post]
  ; DynamicModule[{focus, show, substep, enter, exit}
    , focus = steps
    ; substep[{e_, {}, _}, _] := {Null, e, Style["inert", {Italic, Small}]}
    ; substep[{e_, _, r_}, p_] :=
        { Button[Style["show", Small], enter[p]]
        , e
        , Style[Row[{"-> ", r}], Small]
        }
    ; enter[{p_}] := PrependTo[focus, focus[[1, 2, p]]]
    ; exit[] := focus = Drop[focus, 1]
    ; show[{e_, s_, r_}] :=
       Column[
         { Grid[
             { {"Expression", Column@Reverse@focus[[All, 1]]}
             , { Column[
                   { "Steps"
                   , focus /.
                       { {_} :> Sequence[]
                       , _ :> Button["Back", exit[], ImageSize -> Automatic]
                       }
                   }
                 ]
               , Grid[MapIndexed[substep, s], Alignment -> Left]
               }
             , {"Result", Column@focus[[All, 3]]}
             }
           , Alignment -> Left, Frame -> All, Background -> {{LightCyan}}
           ]
         }
       ]
    ; Dynamic @ show @ focus[[1]]
    ]
  ]
SetAttributes[traceView4, {HoldAllComplete}]

(*mathSymbolsOne = {Sin, Cos, Tan, Exp, Log, Erf, Erfc, Max};*)
mathSymbolsTwo = {Power};
mathComparison = {Equal, SameQ};
mathBinary = {Greater, GreaterEqual, Less, LessEqual};

(*sumKeyUnion /: sumKeyUnion[s_Association] = s;*)
(*sumKeyUnion /: sumKeyUnion[s1_Association, s2_Association] := Total[KeyUnion[{s1, s2}, 0 &]]*)
(*sumKeyUnion /: sumKeyUnion[s_List /; VectorQ[s, AssociationQ]] := Total[KeyUnion[s, 0 &]]*)

(*propagatead[s_, expr_, assoc_Association] :=  
	Association[KeyValueMap[#1 -> #2 (D[s[\[FormalZ]], \[FormalZ]] /. \[FormalZ] -> expr) &, assoc]]*)

(* TODO: propagate minimization and numerical manipulations *)

makePartial[s_, expr_, pos_] := Apply[s][ReplacePart[expr, pos->#]] &

(* TODO: make it work with arrays *)
(* One variable *)
(* Check of factor is zero before performing derivative ? *)
propagatead /: propagatead[s_, expr_, assoc_Association] := 
	With[{local=D[s[\[FormalZ]], \[FormalZ]] /. \[FormalZ] -> expr}, 
	Association[KeyValueMap[#1 -> #2 local &, assoc]]
	]
propagatead /: propagatead[s_, expr_List, assoc_List] := 
	Merge[Map[propagatead[makePartial[s, expr, #], expr[[#]], assoc[[#]]] &, Range[Length[expr]]], Total]

(*ad /: ad[] := Undefined;
ad /: ad[x_] := Undefined;
ad /: ad[x_, y_/;UnsameQ[Head[y], Association]] := Undefined;*)
(*ad[x_, y_] /; Not[MatchQ[{x, y}, {_Real, _Association}]] := \
Throw[$Failed, ad]*)
ad /: c_?NumericQ + ad[x_, y_Association] := ad[c + x, y];
ad /: c_?NumericQ ad[x_, y_Association] := ad[c x, Association[KeyValueMap[#1 -> c #2 &, y]]];
(* TODO: The conjugate of an expressions keeps the same sensitivities*)
ad /: Conjugate[ad[x_, y_Association]] := ad[Conjugate[x], y];
ad /: Power[ad[x_, y_Association], 0] := ad[1, Association[KeyValueMap[#1 -> 0 &, y]]];
ad /: Power[ad[x_, y_Association], n_?NumericQ] := ad[x^n, Association[KeyValueMap[#1-> n x^(n-1) #2 &, y]]];
(*ad /: Power[a_?NumericQ, ad[x_, y_Association]] := ad[a^x, propagatead[a^# &, x, y]];*)
ad /: Power[a_?NumericQ, ad[x_, y_Association]] := ad[a^x, Association[KeyValueMap[#1 -> a^x Log[a] #2 &, y]]];

(* Explicit propagation for the most commonly used transformations *)
ad /: ad[expr1_, y1_Association] + ad[expr2_, y2_Association] := ad[expr1 + expr2, Merge[{y1, y2}, Total]];
ad /: ad[expr1_, y1_Association] ad[expr2_, y2_Association] := ad[expr1 expr2, Merge[{KeyValueMap[#1 -> expr2 #2 &, y1], KeyValueMap[#1 -> expr1 #2 &, y2]}, Total]];
ad /: Exp[ad[expr_, y_Association]] := ad[Exp[expr], Association[KeyValueMap[#1 -> Exp[expr] #2 &, y]]];
ad /: Log[ad[expr_, y_Association]] := ad[Log[expr], Association[KeyValueMap[#1 -> 1/expr #2 &, y]]];
ad /: Erf[ad[expr_, y_Association]] := ad[Erf[expr], Association[KeyValueMap[#1 -> 2 Exp[-expr^2] / Sqrt[Pi] #2 &, y]]];
ad /: Erfc[ad[expr_, y_Association]] := ad[Erfc[expr], Association[KeyValueMap[#1 -> -2 Exp[-expr^2] / Sqrt[Pi] #2 &, y]]];
ad /: UnitStep[ad[expr_, y_Association]] := ad[UnitStep[expr], y]; (* TODO: double check *)
ad /: HeavisideTheta[ad[0, y_Association]] := 0;(*ad[0, y];*) (* TODO: double check *)
ad /: HeavisideTheta[ad[expr_, y_Association]] := HeavisideTheta[expr]; (*ad[HeavisideTheta[expr], y];*) (* TODO: double check *)
ad /: Max[ad[expr_, y_Association], a_?NumericQ] := If[expr - a >= 0, ad[expr, y], a];
ad /: Max[a_?NumericQ, ad[expr_, y_Association]] := If[expr - a >= 0, ad[expr, y], a];
ad /: Max[ad[expr1_, y1_Association], ad[expr2_, y2_Association]] := If[expr1 >= expr2, ad[expr1, y1], ad[expr2, y2]];

(* Generic propagation *)
(*ad /: ad[expr1_, y1_] + ad[expr2_, y2_] := ad[expr1 + expr2, propagatead[#1+#2 &, {expr1, expr2}, {y1, y2}]];*)
(*ad /: ad[expr1_, y1_] ad[expr2_, y2_] := ad[expr1 expr2, propagatead[#1 #2 &, {expr1, expr2}, {y1, y2}]];*)
(*ad /: s_[ad[x_, y_Association]] /; MemberQ[mathSymbolsOne, s] := ad[s[x], propagatead[s, x, y]];*)
ad /: s_[ad[x1_, y1_Association], ad[x2_, y2_Association]] /; MemberQ[mathSymbolsTwo, s] := 
	ad[s[x1, x2], propagatead[s[#1, #2] &, {x1, x2}, {y1, y2}]];
ad /: s_[ad[expr_, y_Association], a_?NumericQ] /; MemberQ[mathComparison, s] := s[expr, a];
ad /: s_[ad[expr1_, y1_Association], ad[expr2_, y2_]] /; MemberQ[mathBinary, s] := s[expr1, expr2];
ad /: s_[ad[expr_, y_Association], z_] /; MemberQ[mathBinary, s] := s[expr, z];
ad /: s_[z_, ad[expr_, y_Association]] /; MemberQ[mathBinary, s] := s[z, expr];

toValue[z_?ad] := z[[1]];

(* TODO: introduce levelspec *)
seqsum[arg_List] :=
    Module[{counter = 1, len = Length[arg], output = arg[[1]]},
      While[counter < len,
        output += arg[[counter + 1]];
        counter++
      ];
      output
    ];

seqprod[arg_List] :=
    Module[{counter = 1, len = Length[arg], output = arg[[1]]},
      While[counter < len,
        output *= arg[[counter + 1]];
        counter++
      ];
      output
    ];

recursiveStatistics[x_List] := Module[
	{out, out2, length=Length[x], counter=2},
	out=x[[1]];
	out2=x[[1]]^2;
	While[counter <= length,
	out = out + x[[counter]];
	out2 = out2 + x[[counter]]^2;
	counter++
	];
	{out / length, Sqrt[out2 / length - (out / length)^2] / Sqrt[length]}
	];
	

(* TODO: improve performance *)
(* Exp[Log[fc] - 1/2 termVol^2 + termVol #] & /@ rvs *)
simulationSpot[fc_, termVol_, rvs_] :=
    Module[{pre, random},
      pre = fc Exp[-1/2 termVol^2];
      random = Exp[Map[termVol # &, rvs]];
      Map[pre # &, random]
    ];

cSimulationSpot = 
	Compile[{{fc, _Real, 1}, {termVol, _Real, 1}, {rvs, _Real, 2}}, Exp[Log[fc]- 1/2 termVol^2 + termVol #] & /@ rvs,
	CompilationTarget->"C"];

$eps = 0;

sdd[x_ /; Dimensions[x]=={}, 0] := 0;
sdd[ad[0|0., _Association], 0] := 0;
sdd[x_ /; Dimensions[x]!={}, 0] := ConstantArray[0, Dimensions[x]];
sdd[x_, \[Epsilon]_] := PDF[NormalDistribution[0, Sqrt[2 \[Epsilon]]], x];

stheta[0|0., 0] := 0;
stheta[x_ /; Dimensions[x]!={} && SubsetQ[{0, 0.}, Union[Flatten[x]]], 0] := ConstantArray[0, Dimensions[x]];
stheta[ad[0|0., _Association], 0] := 0;
stheta[x_, 0] := HeavisideTheta[x] /. HeavisideTheta[0|0.]->0;
stheta[x_, \[Epsilon]_] := CDF[NormalDistribution[0, Sqrt[2 \[Epsilon]]], x];

(* Next two lines implementing elementwise Max *)
(*sm[x_, 0] /; Dimensions[x]=={} := Max[x, 0];
sm[x_, 0] /; Dimensions[x]!={} := MapThread[Max[#1, #2]&, {x, ConstantArray[0, Dimensions[x]]}, Length[Dimensions[x]]];*)
(* TODO: any clean way to do this ? *)
(*sm[0|0., eps_] := 0;*) (* TODO: Why ? This is wrong *)
sm[0|0., 0] := 0;
sm[ad[0|0., _Association], 0] := 0;
sm[x_, 0] := x HeavisideTheta[x] /. HeavisideTheta[0|0.]->0;
sm[x_, \[Epsilon]_] := 2 \[Epsilon] PDF[NormalDistribution[0, Sqrt[2 \[Epsilon]]], x] + x CDF[NormalDistribution[0, Sqrt[2 \[Epsilon]]], x];

(* TODO: Remove *)
smp[x_, \[Epsilon]_] = 1/2 Erfc[-(x / (2 Sqrt[\[Epsilon]]))];

(*vf[x_, y_, z_] := Piecewise[{{y + sm[x-y, 0.0025], x > y}, {z, x <= y}}];*)
(*vf[x_, y_, z_] := (y + sm[x-y, $eps]) Boole[x - y] + z Boole[x <= y];*)
(*vf[x_, y_, z_] := (y + sm[x - y, $eps]) stheta[x - y, $eps] + z stheta[y - x, $eps];*)

payoffBermudan[s_, strike_] := sm[s - strike, $eps];

payoffBermudanPrime[s_ /; Dimensions[s]=={}, strike_] := 
	Which[s - strike == 0, 0, 
		  $eps == 0, HeavisideTheta[s - strike], 
		  True, smp[s - strike, $eps]];
payoffBermudanPrime[s_ /; Head[s]==ad, strike_] := 
	Which[s - strike == 0, 0, 
		  $eps == 0, HeavisideTheta[s - strike], 
		  True, smp[s - strike, $eps]];
payoffBermudanPrime[s_, strike_] := MapThread[payoffBermudanPrime[#1, #2] &, {s - strike, ConstantArray[0, Dimensions[s]]}, Length[Dimensions[s]]];

numeraire[ir_, t_] := Exp[ir t];

(* TODO: need to have the rvs *)
(*
Output = {outBeta, outValue, outHoldValue, fcBar, volBar}
where
outBeta: nE x nB
outValue: nE x nMC
outHoldValue: ne x nMC
fcBar: nE
volBar: nE

All quantities have the nE dimension in the same order as the inputs

*)
regressionCoefficientAndSensitivity[exerciseDates_, forward_, vols_, ir_, rvs_, strike_, basisFunctions_] :=
    Module[ {nE, nMC, terminalvols, spot, numeraire, exerciseValue, valuesFinal, basisFunctionsPrime, phi, phiPrime,
      outAm1, outB, outBeta, outValue, outHoldValue, beta, h, nB, day, aux,
      aBar, bBar, betaBar, eBar, hBar, vBar, xBar, fcBar, volBar},
      (*nE = Length[exerciseDates];*)
      {nMC, nE} = Dimensions[rvs];
      If[ nE == 1,
        {{{}}, {valuesFinal}, {{}}, {{}}, {{}}, {{}}, {{}}, {{}}, {{}}, {{}}},

        (* spot simulations *)
        (* exerciseDates as differences to valuation date in years *)
        terminalvols = vols Sqrt[exerciseDates]; (* nE *)
        spot = simulationSpot[forward, terminalvols, rvs]; (* nMC x nE *)

        (* Left as a matrix to allow the ir to become a risk factor *)
        numeraire = Transpose[Exp[ir * exerciseDates] & /@ Range[nMC]]; (* nE x nMC *)
        exerciseValue = payoffBermudan[Transpose[spot], strike]; (* nE x nMC *)
        valuesFinal = exerciseValue[[-1]]; (* nMC *)

        nB = Length[basisFunctions[1]]; (* TODO: Fix this *)

        (* FIXED: Assuming one factor *)
        (*basisFunctionsPrime = Function[x, Evaluate@Derivative[1][basisFunctions][x]];*)
        (*basisFunctionsPrime = Function[\[FormalY], Derivative[1][basisFunctions][\[FormalY]]];*)
        basisFunctionsPrime = Function[x, {0 x , 1 x, 2 x, 3 x^2, 4 x^3}[[1;;nB]]];
        (*phi = Transpose[Map[basisFunctions, spot, {2}], {2, 3, 1}]; (* nB x nMC x nE *)*)
        phi = basisFunctions[spot]; (* nB x nMC x nE *)
        (*phiPrime = Transpose[Map[basisFunctionsPrime, spot, {2}], {2, 3, 1}]; (* nB x nMC x nE *)*)
        phiPrime = basisFunctionsPrime[spot]; (* nB x nMC x nE *)

        outAm1 = Table[0, {m, nE - 1}, {i, nB}, {j, nB}];
        outB = Table[0, {m, nE - 1}, {i, nB}];
        outBeta = Table[0, {m, nE - 1}, {i, nB}];
        outHoldValue = Table[0, {m, nE}, {n, nMC}];
        outValue = Table[0, {m, nE}, {n, nMC}];

        outHoldValue[[nE]] = 0 valuesFinal;
        outValue[[nE]] = valuesFinal;

        betaBar = Table[0, {m, nE - 1}, {i, nB}];
        aBar = Table[0, {m, nE - 1}, {i, nB}, {j, nB}];
        bBar = Table[0, {m, nE - 1}, {i, nB}];
        xBar = Table[0, {m, nE}, {n, nMC}];
        eBar = Table[0, {m, nE}, {n, nMC}];
        hBar = Table[0, {m, nE}, {n, nMC}];
        vBar = Table[0, {m, nE}, {n, nMC}];
        fcBar = Table[0, {m, nE}];
        volBar = Table[0, {m, nE}];

        vBar[[1]] = ConstantArray[1, nMC];

        day = nE - 1;
        While[day >= 1,
          aux = phi[[All, All, day]]; (* nB x nMC *)
          
          (* Use inverse *)
          (*outAm1[[day]] = Inverse[Outer[seqsum[#1 #2] &, aux, aux, 1, 1]];*)  (* nB x nB *)
          (* Don't use inverse *)
          outAm1[[day]] = Outer[seqsum[#1 #2] &, aux, aux, 1, 1]; (* nB x nB *)
          
          outB[[day]] = seqsum[numeraire[[day, All]] / numeraire[[day + 1, All]] valuesFinal #] & /@ aux; (* nB *) 
          
          (* Change to `seqsum` if increasing nB *)
          (*beta = Dot[outAm1[[day]], outB[[day]]]; (* nB *)*)
          
          (* Use inverse *)
          (*beta = seqsum[# outB[[day]]] & /@ outAm1[[day]];*)  (* nB *)
          (* Don't use inverse *)
          beta = LinearSolve[outAm1[[day]], outB[[day]]];

          outBeta[[day]] = beta;
          (* Change to `seqsum` if increasing nB *)
          (*h = Dot[beta, phi[[All, All, day]]]; (* nMC *)*)
          h = seqsum[beta phi[[All, All, day]]]; (* nMC *)

          outHoldValue[[day]] = h;
          (*valuesFinal = MapThread[vf[#1, #2, #3] &,
            {exerciseValue[[day, All]], h, numeraire[[day, All]] / numeraire[[day + 1, All]] valuesFinal}
          ]; (* nMC *)*)

          (* Updating the values *)
          (* Longstaff-Schwartz *)
          (*aux = Boole[Thread[exerciseValue[[day, All]] - h > 0]];*)
          aux = stheta[exerciseValue[[day, All]] - h, $eps];
          valuesFinal = exerciseValue[[day, All]] aux + numeraire[[day, All]] / numeraire[[day + 1, All]] valuesFinal (1 - aux); (* nMC *)
          
          (* Simple *)
          (*aux = Boole[Thread[exerciseValue[[day, All]] - h > 0]];
          valuesFinal = exerciseValue[[day, All]] aux + h (1 - aux);*)

          outValue[[day]] = valuesFinal;
          day = day - 1;
        ];

        (* Sensitivities *)
        day = 1;
        While[day < nE,

          (* Longstaff-Schwartz *)
          (*vBar[[day + 1]] += vBar[[day]] numeraire[[day, All]] / numeraire[[day + 1, All]] MapThread[Derivative[0, 0, 1][vf][#1, #2, #3] &, {exerciseValue[[day, All]], outHoldValue[[day, All]], 1 & /@ Range[nMC]}];
          hBar[[day]] += vBar[[day]] MapThread[Derivative[0, 1, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];
          eBar[[day]] += vBar[[day]] MapThread[Derivative[1, 0, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];*)

          (* Longstaff-Schwartz - Manual *)
          (*aux = Boole[Thread[exerciseValue[[day, All]] > outHoldValue[[day, All]]]];*)
          aux = stheta[exerciseValue[[day, All]] - outHoldValue[[day, All]], $eps];
          vBar[[day + 1]] += vBar[[day]] numeraire[[day, All]] / numeraire[[day + 1, All]] (1 - aux);
          eBar[[day]] += vBar[[day]] aux;

          (* Simple - Manual *)
          (*aux = Boole[Thread[exerciseValue[[day]] - outHoldValue[[day]] > 0]];
          eBar[[day]] += vBar[[day]] aux;
          hBar[[day]] += vBar[[day]] (1 - aux);*)

          betaBar[[day]] += seqsum[hBar[[day]] #] & /@ phi[[All, All, day]]; (* nB *)
          (*betaBar[[day]] = betaBar[[day]][[All, 1]];*)
          xBar[[day]] += hBar[[day]] seqsum[outBeta[[day]] #] & /@ Transpose[phiPrime[[All, All, day]]]; (* nMC *)

          (* Use inverse *)
          (*aux = seqsum[betaBar[[day]] #] & /@ Transpose[outAm1[[day]]];*)
          (* REMOVE aBar[[day]] += Outer[- #1 #2 &, aux, outBeta[[day]]]; (* nB x nB *)*)
          (*bBar[[day]] += seqsum[betaBar[[day]] #] & /@ Transpose[outAm1[[day]]];*)
          (*aBar[[day]] += -Outer[#1 #2 &, bBar[[day]], outBeta[[day]]];*)
          (* Don't use inverse *)
          bBar[[day]] += LinearSolve[outAm1[[day]], betaBar[[day]]];
          aBar[[day]] += -Outer[#1 #2 &, bBar[[day]], outBeta[[day]]]; (* nB x nB *)
          
          
          aux = seqsum[bBar[[day]] #] & /@ Transpose[phi[[All, All, day]]];
          vBar[[day + 1]] += numeraire[[day]] / numeraire[[day + 1]] aux;
          aux = seqsum[bBar[[day]] #] & /@ Transpose[phiPrime[[All, All, day]]];
          xBar[[day]] += numeraire[[day]] / numeraire[[day + 1]] outValue[[day + 1]] aux;
          aux = Outer[#1 #2 &, phiPrime[[All, All, day]], phi[[All, All, day]], 1, 1] +
              Outer[#1 #2 &, phi[[All, All, day]], phiPrime[[All, All, day]], 1, 1];
          aux = aBar[[day]] aux;
          aux = seqsum /@ Transpose[Flatten[aux, 1]];
          xBar[[day]] += aux;
          xBar[[day]] += eBar[[day]] Derivative[1, 0][payoffBermudan][spot[[All, day]], strike];
          fcBar[[day]] += seqsum[xBar[[day]] spot[[All, day]]] / forward[[day]] / nMC;
          volBar[[day]] += Sqrt[exerciseDates[[day]]] seqsum[xBar[[day]] spot[[All, day]] (-terminalvols[[day]] + rvs[[All, day]])] / nMC;
          day = day + 1
        ];

        (* day = nE *)
        (*hBar[[day]] += vBar[[day]] MapThread[Derivative[0, 1, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];
        eBar[[day]] += vBar[[day]] MapThread[Derivative[1, 0, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];*)

        (* Longstaff-Schwartz - Manual *)
        (*aux = Boole[Thread[exerciseValue[[day, All]] > outHoldValue[[day, All]]]];*)
        aux = stheta[exerciseValue[[day, All]] - outHoldValue[[day, All]], $eps];
      	eBar[[day]] += vBar[[day]] aux;

        (* Simple - Manual *)
        (*aux = Boole[Thread[exerciseValue[[day, All]] - outHoldValue[[day, All]] > 0]];
        eBar[[day]] += vBar[[day]] aux;
        hBar[[day]] += vBar[[day]] (1 - aux);*)

        xBar[[day]] += eBar[[day]] Derivative[1, 0][payoffBermudan][spot[[All, day]], strike];
        fcBar[[day]] += seqsum[xBar[[day]] spot[[All, day]]] / forward[[day]] / nMC;
        volBar[[day]] += Sqrt[exerciseDates[[day]]] seqsum[xBar[[day]] spot[[All, day]] (-terminalvols[[day]] + rvs[[All, day]])] / nMC;

      ];
      {outBeta, outValue, outHoldValue, fcBar, volBar, vBar, hBar, eBar, xBar, spot}
    ];


backwardLoopValues[exerciseValue_, numeraire_, phi_] :=
    Module[{nE, nB, nMC, outA, outB, outBeta, outHoldValue, outValue, valuesFinal, day, aux, beta, h},

      {nB, nMC, nE} = Dimensions[phi];

      outA = ConstantArray[0, {nE - 1, nB, nB}];
      outB = ConstantArray[0, {nE - 1, nB}];
      outBeta = ConstantArray[0, {nE - 1, nB}];
      outHoldValue = ConstantArray[0, {nE, nMC}];
      outValue = ConstantArray[0, {nE, nMC}];

      valuesFinal = exerciseValue[[-1]];
      outHoldValue[[nE]] = ConstantArray[0, Dimensions[valuesFinal]];
      outValue[[nE]] = valuesFinal;

      day = nE - 1;
      While[day >= 1,
        aux = phi[[All, All, day]]; (* nB x nMC *)

        (* Use inverse *)
        (*outA[[day]] = Inverse[Outer[seqsum[#1 #2] &, aux, aux, 1, 1]];*)  (* nB x nB *)
        (* Don't use inverse *)
        outA[[day]] = 1 / nMC Outer[seqsum[#1 #2] &, aux, aux, 1, 1]; (* nB x nB *)

        (*outB[[day]] = 1 / nMC seqsum[numeraire[[day, All]] / numeraire[[day + 1, All]] valuesFinal #] & /@ aux; *)(* nB *)
        outB[[day]] = 1 / nMC seqsum[Transpose[numeraire[[day, All]] / numeraire[[day + 1, All]] valuesFinal # & /@ aux]]; (* nB *)

        (* Change to `seqsum` if increasing nB *)
        (*beta = Dot[outAm1[[day]], outB[[day]]]; (* nB *)*)

        (* Use inverse *)
        (*beta = seqsum[# outB[[day]]] & /@ outAm1[[day]];*)  (* nB *)
        (* Don't use inverse *)
        beta = LinearSolve[outA[[day]], outB[[day]]];

        outBeta[[day]] = beta;
        (* Change to `seqsum` if increasing nB *)
        (*h = Dot[beta, phi[[All, All, day]]]; (* nMC *)*)
        h = seqsum[beta phi[[All, All, day]]]; (* nMC *)

        outHoldValue[[day]] = h;
        (*valuesFinal = MapThread[vf[#1, #2, #3] &,
            {exerciseValue[[day, All]], h, numeraire[[day, All]] / numeraire[[day + 1, All]] valuesFinal}
          ]; (* nMC *)*)

        (* Updating the values *)
        (* Longstaff-Schwartz *)
        (*aux = Boole[Thread[exerciseValue[[day, All]] - h > 0]];*)
        aux = stheta[exerciseValue[[day, All]] - h, $eps];
        valuesFinal = exerciseValue[[day, All]] aux + numeraire[[day, All]] / numeraire[[day + 1, All]] valuesFinal (1 - aux); (* nMC *)

        (* Simple *)
        (*aux = Boole[Thread[exerciseValue[[day, All]] - h > 0]];
          valuesFinal = exerciseValue[[day, All]] aux + h (1 - aux);*)

        outValue[[day]] = valuesFinal;
        day = day - 1;
      ];

      {outA, outB, outBeta, outHoldValue, outValue}

    ];


backwardLoopSensitivities[exerciseValue_, strike_, numeraire_, outHoldValue_, phi_, phiPrime_, exerciseDates_, forward_, terminalvols_, rvs_, spot_, outA_, outBeta_, outValue_] :=
    Module[{nE, nB, nMC, betaBar, aBar, bBar, xBar, eBar, hBar, vBar, fcBar, volBar, day, aux, xua, EmH, thetaEmH, deltaEmH},

      {nB, nMC, nE} = Dimensions[phi];

      betaBar = ConstantArray[0, {nE - 1, nB}];
      aBar = ConstantArray[0, {nE - 1, nB, nB}];
      bBar = ConstantArray[0, {nE - 1, nB}];
      xBar = ConstantArray[0, {nE, nMC}];
      eBar = ConstantArray[0, {nE, nMC}];
      hBar = ConstantArray[0, {nE, nMC}];
      vBar = ConstantArray[0, {nE, nMC}];
      fcBar = ConstantArray[0, nE];
      volBar = ConstantArray[0, nE];

      EmH = exerciseValue - outHoldValue;
      thetaEmH = stheta[EmH, $eps];
      deltaEmH = sdd[EmH, $eps];

      vBar[[1]] = ConstantArray[1, nMC];

      day = 1;
      While[day < nE,

        (* Longstaff-Schwartz *)
        (*vBar[[day + 1]] += vBar[[day]] numeraire[[day, All]] / numeraire[[day + 1, All]] MapThread[Derivative[0, 0, 1][vf][#1, #2, #3] &, {exerciseValue[[day, All]], outHoldValue[[day, All]], 1 & /@ Range[nMC]}];
            hBar[[day]] += vBar[[day]] MapThread[Derivative[0, 1, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];
            eBar[[day]] += vBar[[day]] MapThread[Derivative[1, 0, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];*)

        (* Longstaff-Schwartz - Manual *)
        (*aux = Boole[Thread[exerciseValue[[day, All]] > outHoldValue[[day, All]]]];*)
        (*aux = stheta[exerciseValue[[day, All]] - outHoldValue[[day, All]], $eps];*)
        vBar[[day + 1]] += vBar[[day]] numeraire[[day]] / numeraire[[day + 1]] (1 - thetaEmH[[day]]);
        aux = (exerciseValue[[day]] - numeraire[[day]] / numeraire[[day + 1]] outValue[[day+1]]) deltaEmH[[day]];
        eBar[[day]] += vBar[[day]] (thetaEmH[[day]] + aux);
        hBar[[day]] += vBar[[day]] (-aux);

      (* Simple - Manual *)
        (*aux = Boole[Thread[exerciseValue[[day]] - outHoldValue[[day]] > 0]];
          eBar[[day]] += vBar[[day]] aux;
          hBar[[day]] += vBar[[day]] (1 - aux);*)

        betaBar[[day]] += seqsum[hBar[[day]] #] & /@ phi[[All, All, day]]; (* nB *)
        (*betaBar[[day]] = betaBar[[day]][[All, 1]];*)
        xBar[[day]] += hBar[[day]] seqsum[outBeta[[day]] #] & /@ Transpose[phiPrime[[All, All, day]]]; (* nMC *)

        (* Use inverse *)
        (*aux = seqsum[betaBar[[day]] #] & /@ Transpose[outAm1[[day]]];*)
        (* REMOVE aBar[[day]] += Outer[- #1 #2 &, aux, outBeta[[day]]]; (* nB x nB *)*)
        (*bBar[[day]] += seqsum[betaBar[[day]] #] & /@ Transpose[outAm1[[day]]];*)
        (*aBar[[day]] += -Outer[#1 #2 &, bBar[[day]], outBeta[[day]]];*)
        (* Don't use inverse *)
        bBar[[day]] += LinearSolve[outA[[day]], betaBar[[day]]];
        aBar[[day]] += -Outer[#1 #2 &, bBar[[day]], outBeta[[day]]]; (* nB x nB *)

        aux = seqsum[bBar[[day]] #] & /@ Transpose[phi[[All, All, day]]];
        vBar[[day + 1]] += 1/ nMC numeraire[[day]] / numeraire[[day + 1]] aux;
        xua = seqsum[bBar[[day]] #] & /@ Transpose[phiPrime[[All, All, day]]];
        xBar[[day]] += 1 / nMC numeraire[[day]] / numeraire[[day + 1]] outValue[[day + 1]] xua;

        xBar[[day]] += -1 / nMC (xua (seqsum[outBeta[[day]] #] & /@ Transpose[phi[[All, All, day]]]) +
            aux (seqsum[outBeta[[day]] #] & /@ Transpose[phiPrime[[All, All, day]]]));
        (*aux = Outer[#1 #2 &, phiPrime[[All, All, day]], phi[[All, All, day]], 1, 1] +*)
            (*Outer[#1 #2 &, phi[[All, All, day]], phiPrime[[All, All, day]], 1, 1];*)
        (*aux = aBar[[day]] aux;*)
        (*aux = seqsum /@ Transpose[Flatten[aux, 1]];*)
        (*xBar[[day]] += aux;*)
        (*xBar[[day]] += eBar[[day]] Derivative[1, 0][payoffBermudan][spot[[All, day]], strike];*)
        xBar[[day]] += eBar[[day]] payoffBermudanPrime[spot[[All, day]], strike];
        fcBar[[day]] += seqsum[xBar[[day]] spot[[All, day]]] / forward[[day]] / nMC;
        volBar[[day]] += Sqrt[exerciseDates[[day]]] seqsum[xBar[[day]] spot[[All, day]] (-terminalvols[[day]] + rvs[[All, day]])] / nMC;
        day = day + 1
      ];

      (* day = nE *)
      (*hBar[[day]] += vBar[[day]] MapThread[Derivative[0, 1, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];
        eBar[[day]] += vBar[[day]] MapThread[Derivative[1, 0, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];*)

      (* Longstaff-Schwartz - Manual *)
      (*aux = Boole[Thread[exerciseValue[[day, All]] > outHoldValue[[day, All]]]];*)
      (*aux = stheta[exerciseValue[[day, All]] - outHoldValue[[day, All]], $eps];*)
      (*eBar[[day]] += vBar[[day]] aux;*)
      eBar[[day]] += vBar[[day]]; (* This just follows from v[[nE]]=e[[nE]]  *)

      (* Simple - Manual *)
      (*aux = Boole[Thread[exerciseValue[[day, All]] - outHoldValue[[day, All]] > 0]];
        eBar[[day]] += vBar[[day]] aux;
        hBar[[day]] += vBar[[day]] (1 - aux);*)

      (*xBar[[day]] += eBar[[day]] Derivative[1, 0][payoffBermudan][spot[[All, day]], strike];*)
      xBar[[day]] += eBar[[day]] payoffBermudanPrime[spot[[All, day]], strike];
      fcBar[[day]] += seqsum[xBar[[day]] spot[[All, day]]] / forward[[day]] / nMC;
      volBar[[day]] += Sqrt[exerciseDates[[day]]] seqsum[xBar[[day]] spot[[All, day]] (-terminalvols[[day]] + rvs[[All, day]])] / nMC;

      {fcBar, volBar, vBar, hBar, eBar, xBar, spot}

    ];

(* TODO: can one find a transformation after which the sensitivities can be calculated in parallel over exercise days ? *)
backwardLoopSensitivitiesNew[exerciseValue_, strike_, numeraire_, outHoldValue_, phi_, phiPrime_, exerciseDates_, forward_, terminalvols_, rvs_, spot_, outA_, outBeta_, outValue_] :=
    Module[{nE, nB, nMC, betaBar, aBar, bBar, xBar, eBar, hBar, vBar, fcBar, volBar, day, aux, xua, HmE, thetaHmE, deltaHmE},

      {nB, nMC, nE} = Dimensions[phi];

      betaBar = ConstantArray[0, {nE - 1, nB}];
      aBar = ConstantArray[0, {nE - 1, nB, nB}];
      bBar = ConstantArray[0, {nE - 1, nB}];
      xBar = ConstantArray[0, {nE, nMC}];
      eBar = ConstantArray[0, {nE, nMC}];
      hBar = ConstantArray[0, {nE, nMC}];
      vBar = ConstantArray[0, {nE, nMC}];
      fcBar = ConstantArray[0, nE];
      volBar = ConstantArray[0, nE];

      HmE = outHoldValue - exerciseValue;
      thetaHmE = stheta[HmE, $eps];
      deltaHmE = sdd[HmE, $eps];

      aux = numeraire[[1 ;; nE - 1]] / numeraire[[2 ;; nE]] thetaHmE[[1 ;; nE - 1]];
      vBar = FoldList[Times, ConstantArray[1, nMC], aux];

      aux = (exerciseValue[[1;;nE-1]] - numeraire[[1;;nE-1]] / numeraire[[2;;nE]] outValue[[2;;nE]]) deltaHmE[[1;;nE-1]];
      eBar[[1;;nE-1]] = eBar[[1;;nE-1]] + vBar[[1;;nE-1]] ((1 - thetaHmE[[1;;nE-1]]) + aux);
      eBar [[nE]] = eBar[[nE]] + vBar[[nE]];

      hBar[[1;;nE-1]] = hBar[[1;;nE-1]] + vBar[[1;;nE-1]] (-aux);

      xBar = eBar payoffBermudanPrime[Transpose[spot], strike];

      betaBar = Transpose[seqsum[Transpose[hBar[[1;;nE-1]]] #] & /@ phi[[All, All, 1;;nE-1]]];

      aux = Transpose[seqsum[Transpose[betaBar #]]  & /@ Transpose[phiPrime[[All, All, 1;;nE-1]], {3, 1, 2}]];
      xBar[[1;;nE-1]] = xBar[[1;;nE-1]] + aux;

      aux = LinearSolve[Sequence @@ # ] & /@ Transpose[{outA, betaBar}];
      aBar = -Outer[#1 #2 &, aux, outBeta, 1, 1];
      bBar = aux;

      aux = Transpose[seqsum[Transpose[bBar #]] & /@ Transpose[phiPrime[[All, All, 1;;nE-1]], {3, 1, 2}]];
      xua = Transpose[seqsum[Transpose[bBar #]] & /@ Transpose[phi[[All, All, 1;;nE-1]], {3, 1, 2}]];

      xBar[[1;;nE-1]] = xBar[[1;;nE-1]] +
          (-1 / nMC) (aux Transpose[seqsum[Transpose[outBeta #]] & /@ Transpose[phi[[All, All, 1;;nE-1]], {3, 1, 2}]] +
              xua * Transpose[seqsum[Transpose[outBeta #]] & /@ Transpose[phiPrime[[All, All, 1;;nE-1]], {3, 1, 2}]]);

      xBar[[1;;nE-1]] = xBar[[1;;nE-1]] + 1 / nMC numeraire[[1;;nE-1]] / numeraire[[2;;nE]] outValue[[2;;nE]] aux;

      vBar[[2;;nE]] = vBar[[2;;nE]] + 1 / nMC numeraire[[1;;nE-1]] / numeraire[[2;;nE]] xua;

      fcBar = seqsum[#] & /@ (xBar Transpose[spot] / forward);

      volBar = -terminalvols exerciseDates (seqsum[#] & /@ (xBar Transpose[spot])) +
          Sqrt[exerciseDates] (seqsum[#] & /@ (xBar Transpose[spot rvs]));



      day = 1;
      While[day < nE,

        (* Longstaff-Schwartz *)
        (*vBar[[day + 1]] += vBar[[day]] numeraire[[day, All]] / numeraire[[day + 1, All]] MapThread[Derivative[0, 0, 1][vf][#1, #2, #3] &, {exerciseValue[[day, All]], outHoldValue[[day, All]], 1 & /@ Range[nMC]}];
              hBar[[day]] += vBar[[day]] MapThread[Derivative[0, 1, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];
              eBar[[day]] += vBar[[day]] MapThread[Derivative[1, 0, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];*)

        (* Longstaff-Schwartz - Manual *)
        (*aux = Boole[Thread[exerciseValue[[day, All]] > outHoldValue[[day, All]]]];*)
        aux = stheta[exerciseValue[[day, All]] - outHoldValue[[day, All]], $eps];
        vBar[[day + 1]] += vBar[[day]] numeraire[[day, All]] / numeraire[[day + 1, All]] (1 - aux);
        eBar[[day]] += vBar[[day]] aux;

        (* Simple - Manual *)
        (*aux = Boole[Thread[exerciseValue[[day]] - outHoldValue[[day]] > 0]];
          eBar[[day]] += vBar[[day]] aux;
          hBar[[day]] += vBar[[day]] (1 - aux);*)

        betaBar[[day]] += seqsum[hBar[[day]] #] & /@ phi[[All, All, day]]; (* nB *)
        (*betaBar[[day]] = betaBar[[day]][[All, 1]];*)
        xBar[[day]] += hBar[[day]] seqsum[outBeta[[day]] #] & /@ Transpose[phiPrime[[All, All, day]]]; (* nMC *)

        (* Use inverse *)
        (*aux = seqsum[betaBar[[day]] #] & /@ Transpose[outAm1[[day]]];*)
        (* REMOVE aBar[[day]] += Outer[- #1 #2 &, aux, outBeta[[day]]]; (* nB x nB *)*)
        (*bBar[[day]] += seqsum[betaBar[[day]] #] & /@ Transpose[outAm1[[day]]];*)
        (*aBar[[day]] += -Outer[#1 #2 &, bBar[[day]], outBeta[[day]]];*)
        (* Don't use inverse *)
        bBar[[day]] += LinearSolve[outA[[day]], betaBar[[day]]];
        aBar[[day]] += -Outer[#1 #2 &, bBar[[day]], outBeta[[day]]]; (* nB x nB *)


        aux = seqsum[bBar[[day]] #] & /@ Transpose[phi[[All, All, day]]];
        vBar[[day + 1]] += numeraire[[day]] / numeraire[[day + 1]] aux;
        aux = seqsum[bBar[[day]] #] & /@ Transpose[phiPrime[[All, All, day]]];
        xBar[[day]] += numeraire[[day]] / numeraire[[day + 1]] outValue[[day + 1]] aux;
        aux = Outer[#1 #2 &, phiPrime[[All, All, day]], phi[[All, All, day]], 1, 1] +
            Outer[#1 #2 &, phi[[All, All, day]], phiPrime[[All, All, day]], 1, 1];
        aux = aBar[[day]] aux;
        aux = seqsum /@ Transpose[Flatten[aux, 1]];
        xBar[[day]] += aux;
        (*xBar[[day]] += eBar[[day]] Derivative[1, 0][payoffBermudan][spot[[All, day]], strike];*)
        xBar[[day]] += eBar[[day]] payoffBermudanPrime[spot[[All, day]], strike];
        fcBar[[day]] += seqsum[xBar[[day]] spot[[All, day]]] / forward[[day]] / nMC;
        volBar[[day]] += Sqrt[exerciseDates[[day]]] seqsum[xBar[[day]] spot[[All, day]] (-terminalvols[[day]] + rvs[[All, day]])] / nMC;
        day = day + 1
      ];

      (* day = nE *)
      (*hBar[[day]] += vBar[[day]] MapThread[Derivative[0, 1, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];
        eBar[[day]] += vBar[[day]] MapThread[Derivative[1, 0, 0][vf][#1, #2, 0] &, {exerciseValue[[day, All]], outHoldValue[[day, All]]}];*)

      (* Longstaff-Schwartz - Manual *)
      (*aux = Boole[Thread[exerciseValue[[day, All]] > outHoldValue[[day, All]]]];*)
      aux = stheta[exerciseValue[[day, All]] - outHoldValue[[day, All]], $eps];
      eBar[[day]] += vBar[[day]] aux;

      (* Simple - Manual *)
      (*aux = Boole[Thread[exerciseValue[[day, All]] - outHoldValue[[day, All]] > 0]];
        eBar[[day]] += vBar[[day]] aux;
        hBar[[day]] += vBar[[day]] (1 - aux);*)

      (*xBar[[day]] += eBar[[day]] Derivative[1, 0][payoffBermudan][spot[[All, day]], strike];*)
      xBar[[day]] += eBar[[day]] payoffBermudanPrime[spot[[All, day]], strike];
      fcBar[[day]] += seqsum[xBar[[day]] spot[[All, day]]] / forward[[day]] / nMC;
      volBar[[day]] += Sqrt[exerciseDates[[day]]] seqsum[xBar[[day]] spot[[All, day]] (-terminalvols[[day]] + rvs[[All, day]])] / nMC;

      {fcBar, volBar, vBar, hBar, eBar, xBar, spot}

    ];


forwardLoopValuesAndSensitivities[exerciseValue_, numeraire_, beta_, phi_, phiPrime_, spot_, strike_, exerciseDates_, forward_, terminalvols_, rvs_] :=
    Module[{nB, nMC, nE, notExercised, contValue, cashflow, value, localBeta,
      cashflowBar, betaBar, aBar, bBar, xBar, eBar, hBar, vBar, fcBar, volBar, Amat, Bvec, day, aux, xua, tmp, dndE, betaFwd,
      HmE, thetaHgE, deltaHgE},

      (* Strip input beta of the `ad` structure *)
      localBeta = If[Head[beta[[1, 1]]]===ad, beta[[All, All, 1]], beta];

      {nB, nMC, nE} = Dimensions[phi];

      cashflow = ConstantArray[0, nMC];
      notExercised = ConstantArray[0, {nE, nMC}]; (* notExercised[[day]] ~ not exercised up to day - 1 inclusive *)
      dndE = ConstantArray[0, {nE, nE, nMC}];
      contValue = ConstantArray[0, {nE, nMC}];
      value = ConstantArray[0, {nE, nMC}];

      cashflowBar = ConstantArray[1 / nMC, nMC];
      betaBar = ConstantArray[0, {nE - 1, nB}];
      aBar = ConstantArray[0, {nE - 1, nB, nB}];
      bBar = ConstantArray[0, {nE - 1, nB}];
      xBar = ConstantArray[0, {nE, nMC}];
      eBar = ConstantArray[0, {nE, nMC}];
      hBar = ConstantArray[0, {nE, nMC}];
      vBar = ConstantArray[0, {nE, nMC}];
      fcBar = ConstantArray[0, nE];
      volBar = ConstantArray[0, nE];

      (*vBar[[1]] = ConstantArray[1, nMC];*)

      contValue[[1 ;; nE - 1, 1 ;; nMC]] = MapThread[seqsum[#1 #2] &, {localBeta, Transpose[phi[[All, All, 1 ;; nE - 1]], {2, 3, 1}]}]; (* nE-1 x nMC *)

      HmE = contValue - exerciseValue;
      thetaHgE = stheta[HmE, $eps]; (* nE x nMC *)
      deltaHgE = sdd[HmE, $eps]; (* nE x nMC *)

      notExercised = FoldList[Times, ConstantArray[1, nMC], thetaHgE[[1 ;; nE - 1]]]; (* nE x nMC *)

      cashflow = seqsum[notExercised (ConstantArray[1, Dimensions[thetaHgE]] - thetaHgE) exerciseValue / numeraire]; (* nMC *)

	  dndE = 
	  Table[
	  	Which[day==1 || m>=day, ConstantArray[0, nMC],
	  	      day==2 && m==1, -deltaHgE[[m]],
	  	      True, -deltaHgE[[m]] * seqprod[thetaHgE[[Drop[Range[1, day-1], {m}]]]]], {day, 1, nE}, {m, 1, nE}];(* nE \mu x nE m x nMC *)

      (*dndE =
          Table[-deltaHgE[[m]] * seqprod[thetaHgE[[Drop[Range[1, day - 1], {m}]]]], {day, 2, nE}, {m, 1, day-1}]; (* nE \mu x nE m x nMC *)*)

      (* TODO: output this *)
      (*value = seqsum[cashflow] / nMC;*)

      aux = seqsum[(1 - thetaHgE) exerciseValue / numeraire # & /@ Transpose[dndE, {2, 1, 3}]]; (* nE x nMC *)

      xua = notExercised / numeraire;  (* nE x nMC *)

      tmp = sdd[-HmE, $eps] exerciseValue; (* nE x nMC *) 

      eBar = eBar + ((cashflowBar #) & /@ (aux + xua (tmp + (1 - thetaHgE))));
      hBar = hBar + ((cashflowBar #) & /@ (-aux - xua tmp));

      xBar = xBar + eBar Transpose[payoffBermudanPrime[spot, strike]];
 
	  aux = Transpose[hBar[[1 ;; nE - 1]]];
	  betaBar = Transpose[seqsum[aux #] & /@ phi[[All, All, 1 ;; nE - 1]]]; (* nE-1 x nB *)
	  
	  (* zero betaBar *)
	  (*betaBar = ConstantArray[0, Dimensions[betaBar]];*)
	 
	  xBar[[1;;nE-1]] += hBar[[1;;nE-1]] Transpose[(seqsum[Transpose[beta] #] & /@ Transpose[phiPrime[[All, All, 1;;nE-1]], {2, 1, 3}])];

      (* TODO: this can be an output, no propagation ? *)
      value = contValue + sm[exerciseValue - contValue, $eps];

      aux = phi[[All, All, 1;;nE-1]];
      Amat = 1 / nMC Transpose[Outer[seqsum[#1 #2] &, aux, aux, 1, 1], {2, 3, 1}]; (* nE-1 x nB x nB *)
      
      aux = Transpose[numeraire[[1 ;; nE - 1]] / numeraire[[2 ;; nE]] * value[[2 ;; nE]]];
      Bvec = 1 / nMC Transpose[seqsum[aux #] & /@ phi[[All, All, 1 ;; nE - 1]]]; (* nE-1 x nB *)
      
      betaFwd = MapThread[LinearSolve[#1, #2]&, {Amat, Bvec}, 1];
      bBar = MapThread[LinearSolve[#1, #2]&, {Amat, betaBar}, 1];
      (* TODO: `beta` or `betaFwd` *)
      aBar = -Outer[#1 #2 &, bBar, beta, 1, 1];
      
      aux = Transpose[seqsum[Transpose[bBar #]] & /@ Transpose[phiPrime[[All, All, 1;;nE-1]], {3, 1, 2}]];
      xua = Transpose[seqsum[Transpose[bBar #]] & /@ Transpose[phi[[All, All, 1;;nE-1]], {3, 1, 2}]];

      xBar[[1;;nE-1]] = xBar[[1;;nE-1]] +
          (-1 / nMC) (aux Transpose[seqsum[Transpose[localBeta #]] & /@ Transpose[phi[[All, All, 1;;nE-1]], {3, 1, 2}]] +
              xua * Transpose[seqsum[Transpose[localBeta #]] & /@ Transpose[phiPrime[[All, All, 1;;nE-1]], {3, 1, 2}]]);

      xBar[[1;;nE-1]] = xBar[[1;;nE-1]] + 1 / nMC numeraire[[1;;nE-1]] / numeraire[[2;;nE]] value[[2;;nE]] aux;

      vBar[[2;;nE]] = vBar[[2;;nE]] + 1 / nMC numeraire[[1;;nE-1]] / numeraire[[2;;nE]] xua;

      fcBar = seqsum[#] & /@ (xBar Transpose[spot] / forward);

      volBar = -terminalvols exerciseDates (seqsum[#] & /@ (xBar Transpose[spot])) +
          Sqrt[exerciseDates] (seqsum[#] & /@ (xBar Transpose[spot rvs]));

      {betaFwd, contValue, value, cashflow, fcBar, volBar, vBar, hBar, eBar, xBar, spot, notExercised}

    ];


regressionCoefficientAndSensitivityBothLoops[exerciseDates_, forward_, vols_, ir_, rvs_, strike_, basisFunctions_] :=
    Module[ {nE, nB, nMC, terminalvols, spot, numeraire, exerciseValue, valuesFinal, basisFunctionsPrime, phi, phiPrime,
      outA, outB, outBeta, outValue, outHoldValue, eBar, hBar, vBar, xBar, fcBar, volBar},

      {nMC, nE} = Dimensions[rvs];
      If[ nE == 1,
        {{{}}, {valuesFinal}, {{}}, {{}}, {{}}, {{}}, {{}}, {{}}, {{}}, {{}}},

      (* spot simulations *)
      (* exerciseDates as differences to valuation date in years *)
        terminalvols = vols Sqrt[exerciseDates]; (* nE *)
        spot = simulationSpot[forward, terminalvols, rvs]; (* nMC x nE *)

        (* Left as a matrix to allow the ir to become a risk factor *)
        numeraire = Transpose[Exp[ir * exerciseDates] & /@ Range[nMC]]; (* nE x nMC *)
        exerciseValue = payoffBermudan[Transpose[spot], strike]; (* nE x nMC *)
        valuesFinal = exerciseValue[[-1]];(* nMC *)

        nB = Length[basisFunctions[1]]; (* TODO: Fix this *)

        (* FIXED: Assuming one factor *)
        (*basisFunctionsPrime = Function[x, Evaluate@Derivative[1][basisFunctions][x]];*)
        (*basisFunctionsPrime = Function[\[FormalY], Derivative[1][basisFunctions][\[FormalY]]];*)
        basisFunctionsPrime = Function[x, {0 x , 1 x, 2 x, 3 x^2, 4 x^3}[[1 ;; nB]]];
        (*phi = Transpose[Map[basisFunctions, spot, {2}], {2, 3, 1}]; (* nB x nMC x nE *)*)
        phi = basisFunctions[spot]; (* nB x nMC x nE *)
        (*phiPrime = Transpose[Map[basisFunctionsPrime, spot, {2}], {2, 3, 1}]; (* nB x nMC x nE *)*)
        phiPrime = basisFunctionsPrime[spot]; (* nB x nMC x nE *)

        (*  Values *)
        {outA, outB, outBeta, outHoldValue, outValue} = backwardLoopValues[exerciseValue, numeraire, phi];

        (* Sensitivities *)
        {fcBar, volBar, vBar, hBar, eBar, xBar, spot} = backwardLoopSensitivities[exerciseValue, strike, numeraire, outHoldValue, phi, phiPrime, exerciseDates, forward, terminalvols, rvs, spot, outA, outBeta, outValue];



        {outBeta, outValue, outHoldValue, fcBar, volBar, vBar, hBar, eBar, xBar, spot}
      ]
    ];

(* exerciseDates as differences to valuation date in years *)
(* Same number of sims for backward and forward loops *)
regressionCoefficientAndSensitivityBothLoopsNew[exerciseDates_, forward_, vols_, ir_, rvsBkwd_, rvsFwd_, strike_, basisFunctions_] :=
    Module[ {nE, nB, nMC, terminalvols, spotBkwd, spotFwd, numeraire, exerciseValue, valuesFinal, basisFunctionsPrime, phi, phiPrime,
      outABkwd, outBBkwd, outBetaBkwd, outValueBkwd, outHoldValueBkwd, eBarBkwd, hBarBkwd, vBarBkwd, xBarBkwd, fcBarBkwd, volBarBkwd,
      outAFwd, outBFwd, outBetaFwd, outValueFwd, outHoldValueFwd, outCashflowFwd, eBarFwd, hBarFwd, vBarFwd, xBarFwd, fcBarFwd, volBarFwd,
      elapsedStart, elapsedEnd, elapsedBkwd, elapsedFwd, notExercised},

      nE = Dimensions[rvsBkwd][[2]];
      terminalvols = vols Sqrt[exerciseDates]; (* nE *)

      If[ nE == 1,

        elapsedStart = DateObject[];
        (* Backward loop: setup*)
        spotBkwd = simulationSpot[forward, terminalvols, rvsBkwd];  (*nMC x nE*)
        exerciseValue = payoffBermudan[Transpose[spotBkwd], strike];  (*nE x nMC*)

        (*  Backward loop: values *)
        {outABkwd, outBBkwd, outBetaBkwd, outHoldValueBkwd, outValueBkwd} = {{}, {}, {}, {}, exerciseValue[[-1]]};

        (* Backward loop: sensitivities *)
        {fcBarBkwd, volBarBkwd, vBarBkwd, hBarBkwd, eBarBkwd, xBarBkwd, spotBkwd} = {{}, {}, {}, {}, {}, {}, spotBkwd};
        elapsedEnd = DateObject[];
        elapsedBkwd = QuantityMagnitude[DateDifference[elapsedStart, elapsedEnd, "Minute"]];

        elapsedStart = DateObject[];
        (* Forward loop: setup *)
        spotFwd = simulationSpot[forward, terminalvols, rvsFwd]; (* nMC x nE *)
        exerciseValue = payoffBermudan[Transpose[spotFwd], strike]; (* nE x nMC *)

        (* Forward loop: values and sensitivities *)
        {outBetaFwd, outHoldValueFwd, outValueFwd, fcBarFwd, volBarFwd, vBarFwd, hBarFwd, eBarFwd, xBarFwd, spotFwd, notExercised} =
            {{}, {}, exerciseValue[[-1]], {}, {}, {}, {}, {}, {}, spotFwd, {}};
        elapsedEnd = DateObject[];
        elapsedFwd = QuantityMagnitude[DateDifference[elapsedStart, elapsedEnd, "Minute"]];

        (* Output *)
        Association[{"fcBarBkwd"->fcBarBkwd, "volBarBkwd"->volBarBkwd, "outBetaBkwd"->outBetaBkwd,
          "outHoldValueBkwd"->outHoldValueBkwd, "outValueBkwd"->outValueBkwd,
          "outBetaFwd"->outBetaFwd, "outHoldValueFwd"->outHoldValueFwd, "outValueFwd"->outValueFwd,
          "fcBarFwd"->fcBarFwd,"volBarFwd"->volBarFwd, "spotBkwd"->spotBkwd, "spotFwd"->spotFwd,
          "elapsedBkwd"->elapsedBkwd, "elapsedFwd"->elapsedFwd, "notExercised"->notExercised}],

        (* nE > 1 *)
        (* Left as a matrix to allow the ir to become a risk factor *)

        {nMC, nE} = Dimensions[rvsBkwd];
        nB = Length[basisFunctions[1]]; (* TODO: Fix this *)

        numeraire = Transpose[Exp[ir * exerciseDates] & /@ Range[nMC]]; (*nE x nMC*)

        (* FIXED: Assuming one factor *)
        (*basisFunctionsPrime = Function[x, Evaluate@Derivative[1][basisFunctions][x]];*)
        (*basisFunctionsPrime = Function[\[FormalY], Derivative[1][basisFunctions][\[FormalY]]];*)
        basisFunctionsPrime = Function[x, {0 x , 1 x, 2 x, 3 x^2, 4 x^3}[[1 ;; nB]]];

        elapsedStart = DateObject[];
        (* Backward loop: setup*)
        spotBkwd = simulationSpot[forward, terminalvols, rvsBkwd];  (*nMC x nE*)
        phi = basisFunctions[spotBkwd];  (*nB x nMC x nE*)
        phiPrime = basisFunctionsPrime[spotBkwd];  (*nB x nMC x nE*)
        exerciseValue = payoffBermudan[Transpose[spotBkwd], strike];  (*nE x nMC*)
        (*valuesFinal = exerciseValue[[-1]];*) (*nMC*)

        (*  Backward loop: values *)
        {outABkwd, outBBkwd, outBetaBkwd, outHoldValueBkwd, outValueBkwd} = backwardLoopValues[exerciseValue, numeraire, phi];

        (* Backward loop: sensitivities *)
        {fcBarBkwd, volBarBkwd, vBarBkwd, hBarBkwd, eBarBkwd, xBarBkwd, spotBkwd} = backwardLoopSensitivities[exerciseValue, strike, numeraire, outHoldValueBkwd, phi, phiPrime, exerciseDates, forward, terminalvols, rvsBkwd, spotBkwd, outABkwd, outBetaBkwd, outValueBkwd];
        elapsedEnd = DateObject[];
        elapsedBkwd = QuantityMagnitude[DateDifference[elapsedStart, elapsedEnd, "Minute"]];

        elapsedStart = DateObject[];
        (* Forward loop: setup *)
        spotFwd = simulationSpot[forward, terminalvols, rvsFwd]; (* nMC x nE *)
        phi = basisFunctions[spotFwd]; (* nB x nMC x nE *)
        phiPrime = basisFunctionsPrime[spotFwd]; (* nB x nMC x nE *)
        exerciseValue = payoffBermudan[Transpose[spotFwd], strike]; (* nE x nMC *)

        (* Forward loop: values and sensitivities *)
        (* TODO: pass only values for backward beta *)
        {outBetaFwd, outHoldValueFwd, outValueFwd, outCashflowFwd, fcBarFwd, volBarFwd, vBarFwd, hBarFwd, eBarFwd, xBarFwd, spotFwd, notExercised} =
            forwardLoopValuesAndSensitivities[exerciseValue, numeraire, outBetaBkwd, phi, phiPrime, spotFwd, strike, exerciseDates, forward, terminalvols, rvsFwd];
        elapsedEnd = DateObject[];
        elapsedFwd = QuantityMagnitude[DateDifference[elapsedStart, elapsedEnd, "Minute"]];

        (* Output *)
        Association[{"fcBarBkwd"->fcBarBkwd, "volBarBkwd"->volBarBkwd, "outBetaBkwd"->outBetaBkwd,
          "outHoldValueBkwd"->outHoldValueBkwd, "outValueBkwd"->outValueBkwd,
          "outBetaFwd"->outBetaFwd, "outHoldValueFwd"->outHoldValueFwd, "outValueFwd"->outValueFwd, "outCashflowFwd"->outCashflowFwd, 
          "fcBarFwd"->fcBarFwd,"volBarFwd"->volBarFwd, "spotBkwd"->spotBkwd, "spotFwd"->spotFwd,
          "elapsedBkwd"->elapsedBkwd, "elapsedFwd"->elapsedFwd, "notExercised"->notExercised}]
      ]
    ];

End[];

EndPackage[];

