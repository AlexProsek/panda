Get[FileNameJoin[{DirectoryName[$InputFileName], "ExportNNMAT4.wl"}]];

outputFile = FileNameJoin[{DirectoryName[$InputFileName], "ExportNNMAT4-test.mat"}];
net = NetInitialize[NetChain[
  {LinearLayer[2], Ramp, LinearLayer[1], LogisticSigmoid},
  "Input" -> 2
]];

result = ExportNNMAT4[net, outputFile, {2}];
If[!StringQ[result] || !FileExistsQ[outputFile],
  Print["ExportNNMAT4 failed to create the output MAT file."];
  Exit[1]
];

labels = Import[outputFile, "Labels"];
If[First[labels] =!= "__panda_nn__",
  Print["Manifest is not the first MAT variable."];
  Exit[1]
];
headerBytes = Take[BinaryReadList[outputFile, "Byte"], 4];
If[headerBytes =!= {50, 0, 0, 0},
  Print["Manifest MAT header does not declare uint8: ", headerBytes];
  Exit[1]
];

manifestBytes = Import[outputFile, {"MAT", "Data", "__panda_nn__"}];
manifestText = FromCharacterCode[Round[Flatten[manifestBytes]], "UTF8"];
If[!StringContainsQ[manifestText, "\"format\":\"panda.nn.mat4\""] ||
   !StringContainsQ[manifestText, "\"version\":1"] ||
   !StringContainsQ[manifestText, "\"input_shape\":[2]"] ||
   !StringContainsQ[manifestText, "\"layers\""] ||
   !StringContainsQ[manifestText, "\"type\":\"sigmoid\""],
  Print["Manifest content does not match the expected network."];
  Exit[1]
];

If[!MemberQ[{4, "4"}, Import[outputFile, "Version"]],
  Print["Output is not a MAT version 4 file."];
  Exit[1]
];

DeleteFile[outputFile];
Print["ExportNNMAT4 smoke test passed."];
