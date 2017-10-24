# PowerCount

## Overview
PowerCount is a Mathematica package performing power counting on an expression.

## Usage
First, one needs to set the symbols to be included in or excluded from the power counting.

`IsCnt[x1,x2,...]` sets symbols $x_1,x_2,...$ to be included in power counting. 

`NoCnt[a1,a2,...]` sets symbols $a_1,a_2,...$ to be excluded from power counting. Numerical constants (e.g. $\pi, e...$ are excluded from power counting automatically). 

`CntQ[x]` returns whether a symbol $x$ is included in the power counting.
`NCntQ[x]` returns whether a symbol $x$ is excluded from power counting.

`SetScl[{{x1,x2...},{x3,x4...},...},{M1,M2,...}]` sets the scale of symbols $x1,x_2,...\sim M_1$; $x_3,x_4,...\sim M_2; ...$.

`SetHrchy[M1>M2>...>1>m_1>m_2>...>0]` sets the hierarchy of scales, $M_1>M_2>...>1$ are treated as large scales, while $1>m_1>m_2>...$ are treated as small scales.

`SclHrchy` is a symbol set by `SetHrchy[...]`, which are used as an assumption in power counting to compare different scales. 

`PwrCnt[x]` gives the scale of symbol $x$.  

`ShowPwr[exp]` assembles the expression $exp$'s terms which has the same power counting together, and display the corresponding power counting as subscripts.

`SrtByPwr[exp]` assembles the expression $exp$'s terms which has
the same power counting together, and sort the terms in the order of descending power counting.