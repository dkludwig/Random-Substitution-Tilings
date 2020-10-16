(* ::Package:: *)

(* ::Title:: *)
(*Substitution Tilings: Graphics and Computation*)


(* ::Subsubtitle:: *)
(*Danial Ludwig*)
(*University of Maryland*)


(* ::Section:: *)
(*Introduction*)


(* ::Text:: *)
(*This package is a set of Mathematica functions designed to:  *)
(*	\[Bullet] Generate random "degenerate" substitutions of triangles with angles that are all integer multiples of \[Pi]/p (outlined here: https://arxiv.org/abs/1906.00466) and graphically display them*)
(*	\[Bullet] Organize the substitution space of every \[Pi]/p family through representations of substitutions rules as graphs, and do basic computations using the associated adjacency matrices*)


(* ::Section:: *)
(*How to use this Package*)


(* ::Text:: *)
(*Place this file in the same directory as your project notebook.  You can then load the package easily by inserting the following code at the beginning of your notebook:*)
(**)
(*SetDirectory[NotebookDirectory[]];*)
(*<< SubstitutionTilingsPackage.m*)
(**)
(*Then go to Cell\[RightArrow]Cell Properties and click on "Initialization Cell".  This will ensure the package will be loaded when you evaluate your notebook.*)


(* ::Section:: *)
(*General Conventions*)


(* ::Text:: *)
(*For all graphics functions, the structure used to represent a single triangle is a 2-D list of form {{x1,y1},{x2,y2},{x3,y3},{angle1,angle2,angle3}}, where the angles are represented by their specific integer multiple of the base unit \[Pi]/p.*)
(*e.g myTri = {{0,0},{Sqrt[3],0},{1,0},{3,1,2}} => p = 3+1+2 = 6, so the actual angle measures are  {\[Pi]/2,\[Pi]/6,\[Pi]/3}*)
(**)
(*As outlined in the introduction of the linked paper, the substitution rules here act on one triangle "type" at a time. Similar triangles are counted as the same "type" up to rotation, which means scalene triangles have two distinct types for each set of angles, but isosceles triangles do not. The convention used here for triangle types is a 2-D list of form {{angles},orientation}*)
(*e.g triType = {{3,2,2},"CCW"}, where the two options for orientation, "CCW" and "CW", represent the distinct spacial orientations the vertices can have when their angles are ordered from greatest to least.*)
(**)
(*Substitution rules themselves are represented by 3-D lists of form {triType,splitRule}, where a "splitRule" is a 1-D list of form {vertexToBisect,bisectingAmt} *)
(*e.g mySubRule = {{{3,1,1},"CCW"},{1,1}} acts on triangles with angles measures 3\[Pi]/5, \[Pi]/5, \[Pi]/5 by bisecting the 3\[Pi]/5 angle by amount \[Pi]/5.*)


(* ::Section:: *)
(*Graphics*)


(* ::Subsection:: *)
(*generateTri[triType]*)


(* ::Text:: *)
(*Returns a triangle with angle 1 at (0,0), angle 2 at (1,0) and angle 3 at a point that is solved for.*)


(* ::Subsection:: *)
(*split[tri,splitRule]*)


(* ::Text:: *)
(*Returns a list of two triangles, the result of the input triangle being split at input vertex number by input bisecting amount *)


(* ::Subsection:: *)
(*sub[triList, subRule]*)


(* ::Text:: *)
(*Returns list of triangles resulting from applying subRule to triList*)


(* ::Subsection:: *)
(*randomSub[triList,n]*)


(* ::Text:: *)
(*Option defaults: {ParallelizeQ->False,ProbVector->-1}*)
(*Returns list of triangles resulting from randomly substituting triList n times. Option for ProbVector allows you to weight each possible subRule*)


(* ::Subsection:: *)
(*draw[triList]*)


(* ::Text:: *)
(*Option defaults: {Colors->{},Size->400,EdgeClr->Black}*)
(*Returns graphic of input list of triangles (all having the same base unit \[Pi]/p) formatted with input options*)


(* ::Section:: *)
(*Computation*)


(* ::Subsection:: *)
(*getAllTriTypes[p]*)


(* ::Text:: *)
(*Returns a list of all possible triTypes for a given p*)


(* ::Subsection:: *)
(*getAllSubRules[p]*)


(* ::Text:: *)
(*Returns a list of all possible substitution rules for a given p*)


(* ::Subsection:: *)
(*toGraph[subRule]*)


(* ::Text:: *)
(*Option defaults: {Size->Medium,VSize->0.7,GraphObject->False}*)
(*Returns adjacency matrix for input substitution rule or full graph object if GraphObject->True. Other options format graph object*)


(* ::Subsection:: *)
(*getAllAdjMatrices[p]*)


(* ::Text:: *)
(*Option defaults: {SparseQ->True,ParallelizeQ->False}*)
(*Returns a list of all adjacency matrices for a given p*)


(* ::Subsection:: *)
(*exportAllAdjMatrices[p] *)


(* ::Text:: *)
(*Option defaults: {ParallelizeQ->False}*)
(*Exports all adjacency matrices as .dat files in your notebook's immediate directory*)


(* ::Subsection:: *)
(*getLiapunovSpectrum[adjMatrices,probVector,n]*)


(* ::Text:: *)
(*Option Defaults: {NormalizeQ->True,SortQ->True}*)
(*Returns a list of Liapunov Exponents based on probability vector associated with the adjacency matrices, numerically approximated using n matrix multiplications*)


(* ::Section:: *)
(*Code for functions*)


(* ::Subitem:: *)
(*Helper Functions*)


getTriType[tri_]:={Sort[tri[[4]],Greater],If[isCCW@tri,"CCW","CW"]}


isEquilateral[angles_]:=angles[[1]]==angles[[2]]&&angles[[1]]==angles[[3]];


isIsosceles[angles_]:=
angles[[1]]==angles[[2]]||angles[[2]]==angles[[3]]||angles[[1]]==angles[[3]];


isCCW[tri_]:=
Module[{v1,v2},
v1 = Append[tri[[2]]-tri[[1]],0];
v2 = Append[tri[[3]]-tri[[2]],0];
Cross[v1,v2][[3]]>0
]


order[tri_]:=
Module[{new,shuffledVertices},
new=tri;
(*The purpose of this function is to insure a unique ordering of all possible triangles*)

(*If the triangle is equilateral, the first point is the one with the smallest y-component*)
If[isEquilateral@new[[4]],(
If[new[[3,2]]<new[[2,2]],new={new[[1]],new[[3]],new[[2]],{new[[4,1]],new[[4,3]],new[[4,2]]}}];
If[new[[2,2]]<new[[1,2]],new={new[[2]],new[[1]],new[[3]],{new[[4,2]],new[[4,1]],new[[4,3]]}}];
If[new[[3,2]]<new[[2,2]],new={new[[1]],new[[3]],new[[2]],{new[[4,1]],new[[4,3]],new[[4,2]]}}];

(*Orders the other vertices, taking into account that two points may have equal y-components*)
Which[
Chop[new[[1,2]]-new[[2,2]]]==0&&!isCCW@new,new={new[[2]],new[[1]],new[[3]],{new[[4,2]],new[[4,1]],new[[4,3]]}},
!isCCW@new,new={new[[1]],new[[3]],new[[2]],{new[[4,1]],new[[4,3]],new[[4,2]]}}
];
),(
(*orders angle measures from greatest to least*)
If[new[[4,3]]>new[[4,2]],new={new[[1]],new[[3]],new[[2]],{new[[4,1]],new[[4,3]],new[[4,2]]}}];
If[new[[4,2]]>new[[4,1]],new={new[[2]],new[[1]],new[[3]],{new[[4,2]],new[[4,1]],new[[4,3]]}}];
If[new[[4,3]]>new[[4,2]],new={new[[1]],new[[3]],new[[2]],{new[[4,1]],new[[4,3]],new[[4,2]]}}];

(*if isosceles, also orders the points counterclockwise*)
Which[
new[[4,1]]==new[[4,2]]&&!isCCW@new,new={new[[2]],new[[1]],new[[3]],{new[[4,2]],new[[4,1]],new[[4,3]]}},
new[[4,2]]==new[[4,3]]&&!isCCW@new,new={new[[1]],new[[3]],new[[2]],{new[[4,1]],new[[4,3]],new[[4,2]]}}
];
)];

new
]


wrap[angleNum_]:=Mod[angleNum-1,3]+1;


drawVertex[triType_,rotAmt_,splitRule_:{}]:=
Module[{tri,tri1,tri2},
tri=generateTri@triType;
If[Length@splitRule==0,
Graphics[{White,EdgeForm[Directive[Black]],Triangle[Transpose[RotationMatrix[rotAmt Pi/(triType[[1,1]]+triType[[1,2]]+triType[[1,3]])].Transpose[{tri[[1]],tri[[2]],tri[[3]]}]]]}]
,
{tri1,tri2}=split[tri,splitRule];
Graphics[{White,EdgeForm[Directive[Black]],Triangle[Transpose[RotationMatrix[rotAmt Pi/(triType[[1,1]]+triType[[1,2]]+triType[[1,3]])].Transpose[{tri1[[1]],tri1[[2]],tri1[[3]]}]]],
Triangle[Transpose[RotationMatrix[rotAmt Pi/(triType[[1,1]]+triType[[1,2]]+triType[[1,3]])].Transpose[{tri2[[1]],tri2[[2]],tri2[[3]]}]]]}]
]
]


getRotAmt[tri_]:=
Module[{p,vBase},
p=tri[[4,1]]+tri[[4,2]]+tri[[4,3]];
vBase=tri[[2]]-tri[[1]];

Round[
If[isCCW@tri,
If[Chop@vBase[[2]]>=0,ArcCos[{1,0}.vBase/Norm[vBase]],2 Pi-ArcCos[{1,0}.vBase/Norm[vBase]]],
If[Chop@vBase[[2]]<=0,ArcCos[{-1,0}.vBase/Norm[vBase]],2 Pi-ArcCos[{-1,0}.vBase/Norm[vBase]]]
]
/Pi*p]
]




(* ::Subitem:: *)
(*Graphics*)


generateTri::usage="generateTri[triType]\nReturns a triangle with angle 1 at (0,0), angle 2 at (1,0) and angle 3 at a point that is solved for";
generateTri[triType_]:=
Module[{p,v1,v2,x,y},
p=triType[[1,1]]+triType[[1,2]]+triType[[1,3]];

(*uses the vertices (0,0) and (1,0) every time, but the the third vertex is in the direction of v1 and v2, which are determined by the angles of vertices 1 and 2*)
If[triType[[2]]=="CCW",(
(*the ordering of the triangle determines which way to rotate v1 and v2*)
v1=RotationMatrix[triType[[1,1]] Pi/p].{1,0};
v2=RotationMatrix[-triType[[1,2]] Pi/p].{-1,0};

(*explicit form of the intersection point of the lines defined by the point (0,0) and vector v1, (1,0) and v2*)
Which[
v1[[1]]==0,(x=0;y=(v2[[2]]/v2[[1]]) (-1)),
v2[[1]]==0,(x=1;y=v1[[2]]/v1[[1]]),
True,(
x=(v2[[2]]/v2[[1]]) (v2[[2]]/v2[[1]]-v1[[2]]/v1[[1]])^-1;
y=(v1[[2]]/v1[[1]]) x
)];

order@{{0.,0.},{1.,0.},N[{x,y}],triType[[1]]}
),(
(*note the switch in direction of rotation*)
v1=RotationMatrix[-triType[[1,1]] Pi/p].{-1,0};
v2=RotationMatrix[triType[[1,2]] Pi/p].{1,0};

Which[
v1[[1]]==0,(x=1;y=(v2[[2]]/v2[[1]])),
v2[[1]]==0,(x=0;y=(v1[[2]]/v1[[1]])(-1)),
True,(
x=(v1[[2]]/v1[[1]]) (v1[[2]]/v1[[1]]-v2[[2]]/v2[[1]])^-1;
y=(v2[[2]]/v2[[1]]) x
)];

order[{{1.,0.},{0.,0.},N[{x,y}],triType[[1]]}]
)]
]



split::usage="split[triInd,splitRule]\nReturns a list of two triangles, the result of the input triangle being split at input vertex number by input bisecting amount";
split[triIn_,splitRule_]:=
Module[{p,vRot,vOpp,newX,newY,tri,vertexNum,bisectingAmt},
{vertexNum,bisectingAmt}=splitRule;
tri={N[triIn[[1]]],N[triIn[[2]]],N[triIn[[3]]],triIn[[4]]};
p=tri[[4,1]]+tri[[4,2]]+tri[[4,3]];

(*if the input vertexNum is not 1, 2, or 3, an exception is thrown*)
If[vertexNum<1||vertexNum>3,Throw["Input vertexNum = "<>ToString[vertexNum]<>" is not 1, 2, or 3"]];

(*if the input bisectingAmt is too large for the angle at the input vertexNum, an exception is thrown*)
     If[bisectingAmt>tri[[4,vertexNum]]-1||bisectingAmt<=0,
Throw["Input bisectingAmt = "<>ToString[bisectingAmt]<>" is too large for angle value = "<>ToString[tri[[4,vertexNum]]]<>" at input vertexNum = "<>ToString[vertexNum]]];

(*gets vector from 1st to 2nd point, 2nd to 3rd point, or 3rd to 1st point, for vertexNum = 1, 2, or 3 respectively, then rotates it counterclockwise by amt (n/p)\[Pi] if the triangle is ordered counterclockwise, or clockwise (n/p)\[Pi] if clockwise. This makes sure the vector rotates towards the inside of the triangle *)
vRot= tri[[wrap[vertexNum+1]]]-tri[[wrap[vertexNum]]];

If[isCCW[tri],
vRot =RotationMatrix[(bisectingAmt/p) Pi].vRot,
vRot=RotationMatrix[-(bisectingAmt/p) Pi].vRot];

(*gets vector in the direction of the side opposite to the angle at vertexNum*)
vOpp=tri[[wrap[vertexNum+2]]]-tri[[wrap[vertexNum+1]]];

(*explicit formula for intersection point of the lines defined by the point at vertexNum and vector vRot, vertexNum + 1 and vOpp*)
Which[
Chop@vRot[[1]]==0,
(newX=tri[[vertexNum,1]];newY=(vOpp[[2]]/vOpp[[1]])(newX-tri[[wrap[vertexNum+1],1]])+tri[[wrap[vertexNum+1],2]]),
Chop@vOpp[[1]]==0,
(newX=tri[[wrap[vertexNum+1],1]];newY=(vRot[[2]]/vRot[[1]])(newX-tri[[vertexNum,1]])+tri[[vertexNum,2]]),
True,
(newX=((vRot[[2]]/vRot[[1]]) tri[[vertexNum,1]]-(vOpp[[2]]/vOpp[[1]]) tri[[wrap[vertexNum+1],1]]+tri[[wrap[vertexNum+1],2]]-tri[[vertexNum,2]]) (vRot[[2]]/vRot[[1]]-vOpp[[2]]/vOpp[[1]])^-1;newY=(vRot[[2]]/vRot[[1]])(newX-tri[[vertexNum,1]])+tri[[vertexNum,2]])
];

(*returns a list containing the two internal triangles*)
{order@{tri[[vertexNum]],tri[[wrap[vertexNum+1]]],N[{newX,newY}],{bisectingAmt,tri[[4,wrap[vertexNum+1]]],p-bisectingAmt-tri[[4,wrap[vertexNum+1]]]}},
order@{tri[[vertexNum]],tri[[wrap[vertexNum+2]]],N[{newX,newY}],
{tri[[4,vertexNum]]-bisectingAmt,tri[[4,wrap[vertexNum+2]]],p-(tri[[4,vertexNum]]-bisectingAmt)-tri[[4,wrap[vertexNum+2]]]}}}
]




sub::usage="sub[triList,subRule]\nReturns list of triangles resulting from applying subRule to triList";
sub[triList_,subRule_]:=
Module[{tris},
(*handles both a single triangle input or a list of triangles input*)
If[Depth[N[triList]]==4,(
tris=Map[order,triList];
),(
tris={order@triList};
)];

(*If a triangle in tris has matching angles and orientation to that in subRule, it is mapped to its two internal triangles after split is called on it. Otherwise, it is mapped to itself as the single member of a list, and then at the end Flatten makes the output a list of triangles like the function's input*)
Flatten[
Map[
If[#[[4]]==subRule[[1,1]]&&If[isCCW@#,"CCW","CW"]==subRule[[1,2]],split[#,subRule[[2]]],{#}]&
,tris]
,1]
]



randomSub::usage="randomSub[triList,n,Options]\nOption defaults: {ParallelizeQ\[Rule]False,ProbVector\[Rule]-1}\nReturns list of triangles resulting from randomly substituting triList n times. Option for ProbVector allows you to weight each possible subRule";
Options[randomSub]={ParallelizeQ->False,ProbVector->-1};
randomSub[triList_,n_,OptionsPattern[]]:=
Module[{p,tris,allSubRules,thisSubRule,rand,currentProb},

(*handles both a single triangle input or a list of triangles input*)
If[Depth[N[triList]]==4,(
p=triList[[1,4,1]]+triList[[1,4,2]]+triList[[1,4,3]];
tris=Map[order,triList];
),(
p=triList[[4,1]]+triList[[4,2]]+triList[[4,3]];
tris={order@triList};
)];

allSubRules=getAllSubRules[p];

(*if the input probability vector is not the right length, an exception is thrown*)
If[OptionValue@ProbVector!=-1&&Length[OptionValue@ProbVector]!= Length@allSubRules,Throw["Input ProbVector is not of length "<>ToString[Length@allSubRules]]];

Do[
If[Length@OptionValue@ProbVector==0,
thisSubRule=RandomChoice@allSubRules
,
thisSubRule=RandomChoice[OptionValue@ProbVector->allSubRules]
];

(*applies the substitution rule thisSubRule to all triangles in tris (like in the sub function)*)
tris=
Flatten[
If[OptionValue@ParallelizeQ,
ParallelMap[
If[#[[4]]==thisSubRule[[1,1]]&&If[isCCW@#,"CCW","CW"]==thisSubRule[[1,2]],split[#,thisSubRule[[2]]],{#}]&
,tris]
,
Map[
If[#[[4]]==thisSubRule[[1,1]]&&If[isCCW@#,"CCW","CW"]==thisSubRule[[1,2]],split[#,thisSubRule[[2]]],{#}]&
,tris]
]
,1]

,{i,n}];

tris
]





draw::usage="draw[triList,Options]\nOption defaults: {Colors\[Rule]{},Size\[Rule]400,EdgeClr\[Rule]Black}\nReturns graphic of input list of triangles (all having the same base unit \[Pi]/p) formatted with input options";
Options[draw]={ColorsAssoc->{},Size->400,EdgeClr->Black};
draw[triList_,OptionsPattern[]]:= 
Module[{p,allTriTypes,graphics,colorsAssoc,tris},
(*handles both a single triangle input or a list of triangles input*)
If[Depth[N[triList]]==4,(
p=triList[[1,4,1]]+triList[[1,4,2]]+triList[[1,4,3]];
tris=Map[order,triList];
),(
p=triList[[4,1]]+triList[[4,2]]+triList[[4,3]];
tris={order@triList};
)];

allTriTypes=getAllTriTypes[p];

(*Associates a random color with a triangle type unless colors have been input. Random colors are also chosen if the number of color rules input is incorrect*)
If[
Length[OptionValue[ColorsAssoc]]==Length[allTriTypes],
colorsAssoc=Association[OptionValue[ColorsAssoc]],
colorsAssoc=Association@Table[allTriTypes[[i]]->RGBColor[RandomInteger[{0,255}]/255,RandomInteger[{0,255}]/255,RandomInteger[{0,255}]/255],{i,Length[allTriTypes]}]
];

(*matches up each triangle with its color in a list of graphics primitives*)
graphics=
Flatten[
Table[
{colorsAssoc[{Sort[tris[[i,4]],Greater],If[isCCW@tris[[i]],"CCW","CW"]}],Triangle[{tris[[i,1]],tris[[i,2]],tris[[i,3]]}]}
,{i,Length[tris]}]
,1];

graphics=Prepend[graphics,EdgeForm[Directive[Thin,OptionValue[EdgeClr]]]];
(*draws the graphics list with input size*)
Graphics[graphics,ImageSize->OptionValue[Size]]

]


(* ::Subitem:: *)
(*Computation*)


getAllTriTypes::usage="getAllTriTypes[p]\nReturns a list of all possible triTypes for a given p";
getAllTriTypes[p_]:=
(*The integer partitions of p of size 3 represent all possible angle combinations. If an angle combination represents an isosceles triangle, there is only one "triType" associated with it (denoted CCW), as reflections are still reachable by rotation. Scalene triangles, on the other hand, have two distinct triTypes for each angle combination (CCW and CW)*)
Flatten[
Map[If[isIsosceles@#,{{#,"CCW"}},{{#,"CCW"},{#,"CW"}}]&,IntegerPartitions[p,{3}]]
,1];


getAllSubRules::usage="getAllSubRules[p]\nReturns a list of all possible substitution rules for a given p";
getAllSubRules[p_]:=
(*maps all triTypes to all subRules on that triType with all possible splitRules*)
Flatten[
Map[
Table[
Append[{#},
Which[
i<#[[1,1]],{1,i},
i<#[[1,2]]+#[[1,1]]-1,{2,i-(#[[1,1]]-1)},
True,{3,i-(#[[1,2]]-1)-(#[[1,1]]-1)}
]
]
,{i,p-3}]&
,getAllTriTypes[p]]
,1];



toGraph::usage="toGraph[subRule]\nOption defaults: {Size\[Rule]Medium,VSize\[Rule]0.7,GraphObject\[Rule]False}\nReturns adjacency matrix for input substitution rule or full graph object if GraphObject->True. Other options format graph object";
Options[toGraph]={Size->Medium,VSize->0.7,GraphObject->False};
toGraph[subRule_,OptionsPattern[]]:=
Module[{p,parentTriType,allTriTypes,numVertices,edges={},parentTri,childTri1,childTri2,child1TriType,child2TriType,rotAmt1,rotAmt2,child1Vertex,child2Vertex,parentTriVertex,vertexImages},

parentTriType=subRule[[1]];
p=parentTriType[[1,1]]+parentTriType[[1,2]]+parentTriType[[1,3]];

allTriTypes=getAllTriTypes@p;
numVertices=If[Mod[p,3]!=0,2 p (Length@allTriTypes),2 p(Length@allTriTypes)-4 p/3];
parentTri=generateTri@parentTriType;

(*gets the two child triangles from the split*)
{childTri1,childTri2}=split[parentTri,subRule[[2]]];

child1TriType={childTri1[[4]],If[isCCW@childTri1,"CCW","CW"]};
child2TriType={childTri2[[4]],If[isCCW@childTri2,"CCW","CW"]};

(*rotAmt is the integer multiple of \[Pi]/p that each child triangle is rotated by (maximum of 2p, which is a full rotation)*)
rotAmt1=getRotAmt@childTri1;
rotAmt2=getRotAmt@childTri2;

(*This gives the vertex number in the graph that is associated with the childTri1, childTri2, and parentTri's type and rotation*)
child1Vertex = rotAmt1+2 p (FirstPosition[allTriTypes,child1TriType][[1]]-1)+1;
child2Vertex = rotAmt2+2 p (FirstPosition[allTriTypes,child2TriType][[1]]-1)+1;
parentTriVertex=2 p (FirstPosition[allTriTypes,parentTriType][[1]]-1) +1;


(*Gets edges for all triangle types that aren't parentTriType*)
Do[
If[allTriTypes[[i]]!=parentTriType,
edges=Join[edges,Table[j->j,{j,2 p (i-1)+1,If[isEquilateral@allTriTypes[[i,1]],2 p (i-1)+2 p/3 ,2 p i]}]]]
,{i,Length[allTriTypes]}];

(*appends the edges from the substituted vertices to their children (i.e. in the section of triInd)*)
edges=Join[edges,
Table[(parentTriVertex+j)-> (2 p (Floor[(child1Vertex-1)/(2 p)])+1+Mod[child1Vertex+j-1,If[isEquilateral@child1TriType[[1]],2p/3,2 p]])
,{j,0,If[isEquilateral@parentTriType[[1]],2p/3-1,2 p-1]}]
];

edges=Join[edges,
Table[(parentTriVertex+j)-> (2 p (Floor[(child2Vertex-1)/(2 p)])+1+Mod[child2Vertex+j-1,If[isEquilateral@child2TriType[[1]],2p/3,2 p]])
,{j,0,If[isEquilateral@parentTriType[[1]],2p/3-1,2 p-1]}]
];

(*returns either a graph object or the adjacency matrix*)
If[OptionValue[GraphObject]==True,
vertexImages=Table[
i->drawVertex[allTriTypes[[Floor[(i-1)/(2p)]+1]],Mod[i-1,2p],If[allTriTypes[[Floor[(i-1)/(2p)]+1]]==parentTriType,subRule[[2]],{}]]
,{i,numVertices}];
Graph[Range[numVertices],edges,GraphLayout->"CircularEmbedding",VertexShape->vertexImages,VertexSize->OptionValue[VSize],ImageSize->OptionValue[Size]]
,
N@AdjacencyMatrix@Graph[Range[numVertices],edges]
]
]



getAllAdjMatrices::usage="getAllAdjMatrices[p]\nOption defaults: {SparseQ\[Rule]True,ParallelizeQ\[Rule]False}\nReturns a list of all adjacency matrices for a given p.";
Options[getAllAdjMatrices]={SparseQ->True,ParallelizeQ->False};
getAllAdjMatrices[p_,OptionsPattern[]]:=
If[OptionValue[ParallelizeQ],
ParallelMap[
If[OptionValue[SparseQ],
toGraph[#,GraphObject->False]&
,
Normal@toGraph[#,GraphObject->False]&
]
,getAllSubRules@p]
,
Map[
If[OptionValue[SparseQ],
toGraph[#,GraphObject->False]&
,
Normal@toGraph[#,GraphObject->False]&
]

,getAllSubRules@p]
]



exportAllAdjMatrices::usage="exportAllAdjMatricesp,Options]\nOption defaults: {ParallelizeQ\[Rule]False}\nExports all adjacency matrices as .dat files in your notebook's immediate directory";
Options[exportAllAdjMatrices]={ParallelizeQ->False};
exportAllAdjMatrices[p_,OptionsPattern[]]:=
If[OptionValue[ParallelizeQ],
ParallelMap[
Export[StringReplace[ToString[#],{" "->"","{"->"","}"->"",","->"_"}]<>".dat",Normal@toGraph[#,GraphObject->False]]&
,getAllSubRules@p]
,
Map[
Export[StringReplace[ToString[#],{" "->"","{"->"","}"->"",","->"_"}]<>".dat",Normal@toGraph[#,GraphObject->False]]&
,getAllSubRules@p]
]



getLiapunovSpectrum::usage=
"getLiapunovExponents[adjMatrices,probVector,n,Options]\n Option Defaults: {NormalizeQ\[Rule]True,SortQ\[Rule]True}\nReturns the list of Liapunov Exponents based on probability vector associated with the adjacency matrices, numerically approximated using n matrix multiplications";
Options[getLiapunovSpectrum]={NormalizeQ->True,SortQ->True};
getLiapunovSpectrum[adjMatrices_,probVector_,n_,OptionsPattern[]]:=
Module[{liapunovSpectrum,qT,r},
{qT,r}=QRDecomposition@RandomChoice[probVector->adjMatrices];
liapunovSpectrum=Log@Abs@Diagonal@r;

Do[
{qT,r}=QRDecomposition[RandomChoice[probVector->adjMatrices].Transpose[qT]];
liapunovSpectrum+=Log@Abs@Diagonal@r;
,{i,n}];

liapunovSpectrum/=n;
If[OptionValue@SortQ,liapunovSpectrum=Sort[liapunovSpectrum,Greater]];
If[OptionValue@NormalizeQ&&liapunovSpectrum[[1]]!=0,liapunovSpectrum/liapunovSpectrum[[1]]*2,liapunovSpectrum]
]


