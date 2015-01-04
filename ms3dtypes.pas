unit ms3dtypes;

interface
uses morphmath;


type
  MS3D_Header            = PACKED RECORD
                             Id                  : ARRAY [ 0..9 ] OF CHAR;
                             Version             : INTEGER
                           END;
  MS3D_Vertex            = PACKED RECORD
                             Flags               : BYTE;
                             Position            : Vector3D;
                             BoneID              : SHORTINT;
                             refCount            : BYTE
                           END;
  MS3D_Triangle          = PACKED RECORD
                             Flags               : WORD;
                             VertexIndices       : array[0..2]of word;
                             VertexNormals       : array[0..2]of Vector3D;
                             S, T                :  array [0..2]of single;
                             SmoothingGroup,
                             GroupIndex          : BYTE
                           END;
  MS3D_Group             = PACKED RECORD
                             Flags               : BYTE;
                             Name                : ARRAY [ 0..31 ] OF CHAR;
                             nTriangles          : WORD;
                             TriangleIndices     : ARRAY OF WORD;
                             MaterialIndex       : BYTE
                           END;
  MS3D_Material          = PACKED RECORD
                             Name                : ARRAY [ 0..31 ] OF CHAR;
                             Ambient,
                             Diffuse,
                             Specular,
                             Emissive            : array[0..3]of single;
                             Shininess,
                             Transparency        : SINGLE;
                             Mode                : BYTE; //unused!
                             Texture,
                             Alphamap            : ARRAY [ 0..127 ] OF CHAR
                           END;
  MS3D_Joint             = PACKED RECORD
                             Flags               : BYTE;
                             Name,
                             ParentName          : ARRAY [ 0..31 ] OF CHAR;
                             Rotation,
                             Translation         : Vector3D;
                             nRotKeyframes,
                             nTransKeyframes     : WORD
                           END;
  MS3D_Keyframe          = PACKED RECORD
                             Time                : SINGLE;
                             Parameter           : Vector3D
                           END;
  TJointName             = PACKED RECORD
                             JointIndex          : WORD;
                             Name                : STRING
                           END;
  TModelKeyframe         = RECORD
                             JointIndex            : WORD;
                             Time                  : SINGLE; //in ms
                             Parameter             : Vector3D
                           END;
   TModelJoint            = RECORD
                             LocalRotation,
                             LocalTranslation      : Vector3D;
                             AbsoluteMatrix,
                             RelativeMatrix,
                             FinalMatrix           : Matrix;
                             CurTransKeyframe,
                             CurRotKeyframe        : WORD;
                             Parent                : INTEGER;
                             nRotationKeyframes,
                             nTranslationKeyframes : WORD;
                             RotationKeyframes,
                             TranslationKeyframes  : ARRAY OF TModelKeyframe
                           END;

implementation

end.
