## Tokenizing process

1. Tokenizer reads ISA segment to detect separators, constructs a SegmentToken consisting of SimpleElementTokens representing the ISA
2. Tokenizer reads subsequent segments up to and including an IEA segment
3. Tokenizer returns a [SegmentToken] (list of SegmentTokens) representing the interchange

## Parsing process
4. Parser takes the ISA SegmentToken and looks up the InterchangeDef's SegmentUses by the ISA12 version number.
5. Parser takes the Segment ID (first ElementToken in the SegmentToken) and looks up the SegmentDef's ElementUses. It uses the ElementUses to parse each ElementToken into an ElementVal, then uses the ElementVals to construct a SegmentVal (representing the ISA segment)
6. Parse one or more Functional Groups. Read the GS08 value from the GS SegmentToken to determine which functional group version is being used. Look up the FunctionalGroupDef by the functional group version to determine what header and trailer segments are used. Parse the GS SegmentToken into a SegmentVal.
7. Parse one or more Transaction Sets. Read the ST01 to determine what Transaction Set is being used (e.g. 850, 856, 315, 214) and look up that TransactionSetDef to find its TableDefs and their LoopDefs and SegmentUses (and their ElementUses).
8. Construct a tree with this hierarchy:
TransmissionVal
  InterchangeVal
    SegmentVal
    TableVal
      SegmentVal
      LoopVal
      SegmentVal
    SegmentVal
