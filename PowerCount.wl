(* ::Package:: *)

BeginPackage["PowerCount`"]


IsCnt::usage="IsCount[x,y,z,..] sets the symbols being included in power counting."
NoCnt::usage="NoCount[a,b,c,..] sets the symbols being excluded from power counting."
CntQ::usgae="CntQ[x] returns whether x is included in power counting."
NCntQ::usgae="NCntQ[x] returns whether x is excluded from in power counting."
SetScl::usage="SetScl[{{\!\(\*SubscriptBox[\(x\), \(1\)]\),\!\(\*SubscriptBox[\(x\), \(2\)]\),...},{\!\(\*SubscriptBox[\(y\), \(1\)]\),\!\(\*SubscriptBox[\(y\), \(2\)]\),...},...},{\!\(\*SubscriptBox[\(s\), \(1\)]\),\!\(\*SubscriptBox[\(s\), \(2\)]\),...}] sets the symbols \!\(\*SubscriptBox[\(x\), \(i\)]\) with scale \!\(\*SubscriptBox[\(s\), \(i\)]\)."
Pwr::usage="Pwr[x] gives the scale of symbol x."
SetHrchy::usage="SetHrcy[\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(1\)]\)>\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(2\)]\)>\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(3\)]\)>...] sets the hierarchy of scales, with all scales are postive. Gives assumption=\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(1\)]\)>0\[And]\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(2\)]\)>0\[And]\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(3\)]\)>0...\[And]\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(1\)]\)>\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(2\)]\)>\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(3\)]\)>..."
PwrCnt::usage="PwrCnt gives the power counting of expression x."
ToPwr::usage="ToPwr[\[CapitalLambda]] convert \[CapitalLambda] to O[\[CapitalLambda]]."
SumPwr::usage="SumPwr[exp] gives the power counting of the hole exprssion exp, returns the largest power."
SrtPwr::usage="SrtPwr[exp] sort the powers of each term in expression exp, whose Head is Plus, according to the power counting, with terms in the order of decending power counting."
SclHrchy::usage="SclHrchy restores the hirachy of the scales, which is used as an assumption in sort an expression by power counting."
ShowPwr::usgae="ShowPwr[exp] arranges terms in exp in an expression by their powers, and dispalay as subscripts"
SrtByPwr::usage="SrtPwr[exp] sort expression exp's terms, whose Head is Plus, by its power counting (in the order of decending power counting)."
PwrLst::usage="PwrLst[exp] gives a table summerize the power counting and corresponding terms."

Begin["Private`"]


CntQ[x_?NumericQ]:=False
NCntQ[x_?NumericQ]:=True
CntQ[Times[a_,b_]]/;And@@(CntQ/@{a,b}):=True
CntQ[X_^n_]/;CntQ[X]:=True
NCntQ[X_^n_]/;NCntQ[X]:=True
NCntQ[Times[a_,b_]]/;And@@(NCntQ/@{a,b}):=True
NCntQ[Plus[a_,b_]]/;And@@(NCntQ/@{a,b}):=True
NCntQ[f_[x__]]/;And@@(NCntQ/@List[x]):=True

IsCnt[X__]:={(CntQ[#]=True)&/@List[X];(NCntQ[#]=False)&/@List[X];};
NoCnt[X__]:={(CntQ[#]=False)&/@List[X];(NCntQ[#]=True)&/@List[X];};

SetScl[X__,S__]:=MapThread[Map[Function[x,x/:Pwr[x]:=#]&[#2],#1]&,{X,S}];
SetHrchy[X__]:=(SclHrchy=X)

Pwr[X_^n_]:=Assuming[SclHrchy,Simplify[Pwr[X]^n]]
Pwr[X_-Y_]:=Pwr[X+Y]
Pwr[X_Plus]:=Assuming[SclHrchy,Pwr/@X]
Pwr[X_Times]:=Assuming[SclHrchy,Simplify[Pwr/@X]]
Pwr[X_?NCntQ]:=1
PwrCnt[X_]:=(Pwr[X]//.{a__*X1__^n_./;(NCntQ[a]&&CntQ[X1]):>X1^n})

ToPwr/:MakeBoxes[ToPwr[X_],TraditionalForm]:=StyleBox[RowBox[{StyleBox["O", 14,Bold,Blue, FontFamily -> "Lucida Handwriting"]," ","(",ToBoxes[X,TraditionalForm][[1]],")"}],SpanMaxSize->Infinity]
ToPwr[X_Plus]:=ToPwr/@X
ToPwr[ToPwr[X__]]:=ToPwr[X]
SetAttributes[ToPwr,Listable];
SetAttributes[PwrCnt,Listable];

SumPwr[X_]:=(Assuming[SclHrchy,Module[{exp=(ToPwr[X])},exp//.Plus[ToPwr[x_],ToPwr[y_]]:>ToPwr[Max[x,y]//Simplify]]])//.Max->Plus

SrtPwr[X_]:=Module[{XPWRLst=PwrCnt/@List@@(Expand[X])},Inactive[Plus]@@ToPwr[Assuming[SclHrchy,Sort[XPWRLst,Simplify[#1>=#2]&]]]]
SrtByPwr[X_,f_:Identity]:=Inactive[Plus]@@Assuming[SclHrchy,(f[Plus@@#]&)/@Sort[GatherBy[List@@(Expand[X]),PwrCnt],Simplify[First[Union[PwrCnt[#1]]]>=First[Union[PwrCnt[#2]]]]&]]

ShowPwr[X_,f_:Identity]:=Inactive[Plus]@@(Subscript[Framed[Assuming[SclHrchy,f[#]],RoundingRadius->5,FrameStyle->{Thick,Gray,DotDashed},Background->LightGreen],ToPwr[PwrCnt[#]]]&/@(Plus@@#&)/@Sort[GatherBy[List@@(Expand[X]),PwrCnt],Assuming[SclHrchy,Simplify[First[Union[PwrCnt[#1]]]>=First[Union[PwrCnt[#2]]]]]&])

PwrLst[X_,f_:Identity]:=Assuming[SclHrchy,Grid[MapIndexed[{#2[[1]],(ToPwr@*PwrCnt)[#],f[#]}&,((Plus@@#&)/@Sort[GatherBy[List@@(Expand[X]),PwrCnt],Assuming[SclHrchy,Simplify[First[Union[PwrCnt[#1]]]>=First[Union[PwrCnt[#2]]]]]&])],Frame->All,FrameStyle->Directive[LightGray,Thick],Alignment->Left]]

End[];
EndPackage[];
