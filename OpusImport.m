(* ::Package:: *)

(* ::Input::Initialization:: *)
(* Author: Claudius Hoberg; Ruhr-University Bochum; Date: 2018/08/13 *)

BeginPackage["OpusImport`"]
ImportOpus::usage="ImportOpus[filename, datatype] loads Opus data. Possible datatypes: single, reference, absorbance, interferogram_single, interferogram_reference, single_meta, reference_meta, interferogram_single_meta, interferogram_reference_meta, absorbance_meta"

Begin["Private`"]

ReadBlock[file_]:=(
blockType=IntegerDigits[BinaryRead[file,"Integer32",ByteOrdering->-1],2,32];lengthOfBlock=BinaryRead[file,"Integer32",ByteOrdering->-1];pointerOfBlock=BinaryRead[file,"Integer32",ByteOrdering->-1];{blockType,lengthOfBlock,pointerOfBlock}
);

DBSCPLX[inp_]:=FromDigits[inp[[-2;;-1]],2];
DBSSTYP[inp_]:=FromDigits[inp[[-4;;-3]],2];
DBSPARM[inp_]:=FromDigits[inp[[-10;;-5]],2];
DBSDATA[inp_]:=FromDigits[inp[[-17;;-11]],2];
DBSDERIV[inp_]:=FromDigits[inp[[-19;;-18]],2];
DBSEXTND[inp_]:=FromDigits[inp[[-27;;-20]],2];

GetParamList[file_,item_]:=(
SetStreamPosition[file,directory[[item,3]]];blockEndPosition=directory[[item,3]]+4 directory[[item,2]];paramList={};While[StreamPosition[file]<blockEndPosition,
rpbuf=ReadParam[file];
AppendTo[paramList,rpbuf];
];
paramList[[1;;-2]]
);

GetFreqAxis[file_,item_]:=(
params=GetParamList[file,item];lowerFreq=params[[Position[params,"FXV"][[1,1]],3]];higherFreq=params[[Position[params,"LXV"][[1,1]],3]];noDatPoints=params[[Position[params,"NPT"][[1,1]],3]];freqAxis=Subdivide[lowerFreq,higherFreq,noDatPoints-1];
freqAxis
);

GetFreqLowHigh[file_,item_]:=(
params=GetParamList[file,item];lowerFreq=params[[Position[params,"FXV"][[1,1]],3]];higherFreq=params[[Position[params,"LXV"][[1,1]],3]];noDatPoints=params[[Position[params,"NPT"][[1,1]],3]];
{lowerFreq, higherFreq, noDatPoints}
);

StringTrimZeros[str_]:=(
strbuf=ToCharacterCode[str];FromCharacterCode[DeleteCases[strbuf,0]]
);

ReadParam[file_]:=(
paramKey=BinaryRead[file,"TerminatedString",ByteOrdering->-1];paramType=BinaryRead[file,"Integer16",ByteOrdering->-1];paramWidth=2 BinaryRead[file,"Integer16",ByteOrdering->-1];If[paramType==0,paramContent=BinaryRead[file,"Integer32",ByteOrdering->-1];];If[paramType==1,paramContent=BinaryRead[file,"Real64",ByteOrdering->-1];];If[paramType==2,paramContent=StringJoin[Table[BinaryRead[file,"Character8",ByteOrdering->-1],{i,1,paramWidth}]];paramContent=StringTrimZeros[paramContent];];If[paramType==3,paramContent=StringJoin[Table[BinaryRead[file,"Character8",ByteOrdering->-1],{i,1,paramWidth}]];paramContent=StringTrimZeros[paramContent];];If[paramType==4,paramContent=StringJoin[Table[BinaryRead[file,"Character8",ByteOrdering->-1],{i,1,paramWidth}]];paramContent=StringTrimZeros[paramContent];];{paramKey,paramWidth,paramContent}
);

GetYValues[file_,item_]:=(
blockSize=directory[[item,2]];SetStreamPosition[file,directory[[item,3]]];yvalues=Table[BinaryRead[file,"Real32",ByteOrdering->-1],{i,1,blockSize}];
yvalues
);

ParamCleaner[params_]:=buf=params[[All,{1,3}]];

ImportOpus[filename_,dataType_]:=Module[{},file=File[filename];magikNr=BaseForm[BinaryRead[file,"Integer32",ByteOrdering->+1],16];
fileVersion=BinaryRead[file,"Real64",ByteOrdering->-1];firstDir=BinaryRead[file,"Integer32",ByteOrdering->-1];maxDirBlockSize=BinaryRead[file,"Integer32",ByteOrdering->-1];currentDirBlockSize=BinaryRead[file,"Integer32",ByteOrdering->-1];SetStreamPosition[file,firstDir];

directory=Table[ReadBlock[file],{i,1,currentDirBlockSize}];
dirmatrix=MatrixForm[directory,TableHeadings->{None,{"block type","block length","block pointer"}}];

fileContents=Transpose[{DBSDATA/@directory[[All,1]],DBSPARM/@directory[[All,1]]}];fileContents=Transpose[{DBSDATA/@directory[[All,1]],DBSSTYP/@directory[[All,1]],DBSPARM/@directory[[All,1]]}];fileContentsTable=TableForm[fileContents,TableHeadings->{None,{"data", "type","param"}}];

singleChannelData={};
singleChannelParamList={};
(*singleChannelID=Flatten[{Position[fileContents,{1,1,1}],Position[fileContents,{1,1,0}]}];*)
singleChannelID={Position[fileContents,{1,1,1}],Position[fileContents,{1,1,0}]};

If[Length[singleChannelID[[1]]]>0,(
singleChannelID=Flatten[singleChannelID[[All,1]]];
singleChannelData=Transpose[{GetFreqAxis[file,singleChannelID[[1]]],GetYValues[file,singleChannelID[[2]]]}];singleChannelParamList=GetParamList[file,singleChannelID[[1]]];
)
];


refChannelData={};
refChannelParamList={};
(*refChannelID=Flatten[{Position[fileContents,{1,2,1}],Position[fileContents,{1,2,0}]}];*)
refChannelID={Position[fileContents,{1,2,1}],Position[fileContents,{1,2,0}]};
If[Length[refChannelID[[1]]]>0,(
refChannelID=Flatten[refChannelID[[All,1]]];
refChannelData=Transpose[{GetFreqAxis[file,refChannelID[[1]]],GetYValues[file,refChannelID[[2]]]}];refChannelParamList=GetParamList[file,refChannelID[[1]]];
)
];

absorbanceChannelData={};
absorbanceChannelParamList={};
(*absorbanceChannelID=Flatten[{Position[fileContents,{3,1,1}],Position[fileContents,{3,1,0}]}];*)
absorbanceChannelID={Position[fileContents,{3,1,1}],Position[fileContents,{3,1,0}]};
If[Length[absorbanceChannelID[[1]]]>0,(
absorbanceChannelID=Flatten[absorbanceChannelID[[All,1]]];
absorbanceChannelData=Transpose[{GetFreqAxis[file,absorbanceChannelID[[1]]],GetYValues[file,absorbanceChannelID[[2]]]}];
absorbanceChannelParamList=GetParamList[file,absorbanceChannelID[[1]]];
)
];

interferogramSingleData={};
interferogramSingleParamList={};
(*interferogramSingleID=Flatten[{Position[fileContents,{2,1,1}],Position[fileContents,{2,1,0}]}];*)
interferogramSingleID={Position[fileContents,{2,1,1}],Position[fileContents,{2,1,0}]};
If[Length[interferogramSingleID[[1]]]>0,(
interferogramSingleID=Flatten[interferogramSingleID[[All,1]]];
interferogramSingleData=Transpose[{GetFreqAxis[file,interferogramSingleID[[1]]],GetYValues[file,interferogramSingleID[[2]]]}];interferogramSingleParamList=GetParamList[file,interferogramSingleID[[1]]];
)
];

interferogramRefData={};
interferogramRefParamList={};
(*interferogramRefID=Flatten[{Position[fileContents,{2,2,1}],Position[fileContents,{2,2,0}]}];*)
interferogramRefID={Position[fileContents,{2,2,1}],Position[fileContents,{2,2,0}]};
If[Length[interferogramRefID[[1]]]>0,(
interferogramRefID=Flatten[interferogramRefID[[All,1]]];
interferogramRefData=Transpose[{GetFreqAxis[file,interferogramRefID[[1]]],GetYValues[file,interferogramRefID[[2]]]}];interferogramRefParamList=GetParamList[file,interferogramRefID[[1]]];
)
];

Close[file];

Which[
dataType=="single",singleChannelData,
dataType=="single_meta",ParamCleaner[singleChannelParamList],
dataType=="ref",refChannelData,
dataType=="ref_meta",ParamCleaner[refChannelParamList],
dataType=="interferogram_single",interferogramSingleData,
dataType=="interferogram_single_meta",ParamCleaner[interferogramSingleParamList],
dataType=="interferogram_ref",interferogramRefData,
dataType=="interferogram_ref_meta",ParamCleaner[interferogramRefParamList],
dataType=="absorbance",absorbanceChannelData,
dataType=="absorbance_meta",ParamCleaner[absorbanceChannelParamList],
dataType=="dir",dirmatrix,
dataType=="filecontents",fileContentsTable
]
]

End[]

EndPackage[]






